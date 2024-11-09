;;; corfu-pixel-perfect.el --- Corfu in pixel precision -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jimmy Yuen Ho Wong

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Created: 2024-10-30
;; Version: 0.3
;; Package-Requires: ((emacs "29.1") (corfu "1.5"))
;; URL: https://github.com/wyuenho/emacs-corfu-pixel-perfect
;; Keywords: abbrev, convenience, matching, completion, text

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Corfu-pixel-perfect allows the use of variable fonts, emojis and images while
;; still maintaining perfect pixel alignment of the popup content virtually no
;; performance degradation.

;;; Code:

(require 'corfu)
(require 'mule-util)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup corfu-pixel-perfect nil
  "Corfu Pixel Perfect."
  :group 'corfu
  :prefix "corfu-pixel-perfect")

(defcustom corfu-pixel-perfect-ignore-annotation-modes nil
  "A list of major modes that should not show annotations."
  :type '(repeat function))

(defcustom corfu-pixel-perfect-ignore-annotation-except-current t
  "Whether to show the annotation for the current selection.

When used in combination with
`corfu-pixel-perfect-ignore-annotation-modes', all the
annotations will be hidden except for the current selection.

When `corfu-pixel-perfect-ignore-annotation-modes' does not have
an entry for the current major mode, this option has no effect."
  :type 'boolean)

(defcustom corfu-pixel-perfect-ellipsis nil
  "Whether to show ellipsis.

If nil, the bar is shown on the fringe with no truncation
indicator.

If it's the symbol `fast', the bar is shown on the margin with
ellipsis as the truncation indicator.  When `corfu-max-width' is
less than the width of the content, a column of ellipsis will
appear to indicate additional content is clipped behind the popup
frame at all times. This is the fastest option if you just want
some truncation indication.

If it's the symbol `annotation-first', the behavior is similar to
`fast', but instead of a column of ellipsis at all times, when
the end of the candidate column approaches the right side of the
popup frame, ellipsis will start replacing the elided portion
from the longest candidate onwards until `corfu-min-width' is
reached.  The bar is back on the fringe when you use this option.

If it's the symbol `proportional', when `corfu-max-width' is less
than the width of the content, both the candidate and annotation
columns are elided proportionally to their lengths. This is the
best option for most cases as it preserves the most information
as the popup frame is progressively narrowed. The bar is back on
the fringe when you use this option."
  :local t
  :type '(choice (const :tag "Fast" fast)
                 (const :tag "Annotation first" annotation-first)
                 (const :tag "Proportional" proportional)
                 (const :tag "None" nil)))

(define-fringe-bitmap 'corfu-pixel-perfect-scroll-bar [])

(defconst corfu-pixel-perfect--display-table
  (let ((dt (make-display-table)))
    (set-display-table-slot dt 'truncation ?\x2026)
    dt)
  "Truncation ellipsis when `corfu-pixel-perfect-ellipsis' is `fast'")

(defun corfu-pixel-perfect--make-buffer-advice (buffer)
  "Put a display table with an ellipsis on BUFFER."
  (with-current-buffer buffer
    (setq-local buffer-display-table corfu-pixel-perfect--display-table
                left-fringe-width nil
                right-fringe-width nil
                fringe-indicator-alist nil)
    (add-to-invisibility-spec 'corfu-pixel-perfect))
  buffer)

(defun corfu-pixel-perfect--make-frame-advice (fn &rest args)
  "Ensure buffer local variables take effect in FRAME."
  (make-frame-visible
   (let ((inhibit-redisplay t)
         ;; Setting the fringe on the frame via buffer local vars is just crazy...
         (frame (let ((left-fringe-width 0)
                      (right-fringe-width 0))
                  (cl-letf (((symbol-function 'make-frame-visible) (symbol-function 'ignore)))
                    (apply fn args)))))

     ;; If the buffer had never been shown before, the margin text will not be
     ;; visible until the frame is visible, so we need to force the window to
     ;; update again. In addition, if the buffer had been shown before, but has
     ;; its margin or fringe widths updated, we'll need to set the window buffer
     ;; again to trigger the update. Virtually no perf hit here.
     (set-window-buffer (frame-root-window frame) (current-buffer))

     frame)))

(cl-defmethod corfu--affixate :around (cands &context (corfu-pixel-perfect-mode (eql t)))
  (let ((result (cl-call-next-method cands)))
    (cdr result)))

(defun corfu-pixel-perfect--trim (cands)
  "Trim white space in candidates CANDS."
  (cl-loop for c in cands do
           (cl-loop for s in-ref c do
                    (setf s (string-trim s))))
  cands)

;; modified from `string-pixel-width' in subr-x.el
(defun corfu-pixel-perfect--string-pixel-size (string)
  "Return the size of STRING in pixels.

The return value is a `cons' cell where the `car' is the width and
`cdr' is the height."
  (if (zerop (length string))
      (cons 0 0)
    (with-current-buffer (get-buffer-create " *corfu--string-pixel-size*")
      (setq-local display-line-numbers nil
                  buffer-invisibility-spec nil)
      (delete-region (point-min) (point-max))
      (setq line-prefix nil
            wrap-prefix nil)
      (insert (propertize string 'line-prefix nil 'wrap-prefix nil))
      (buffer-text-pixel-size nil nil t))))

(defun corfu-pixel-perfect--string-pixel-width (string)
  "Pixel width of STRING, even when it's invisible."
  (car (corfu-pixel-perfect--string-pixel-size string)))

(defun corfu-pixel-perfect--column-pixel-width (cands col)
  "Pixel width of column COL in candidates CANDS.

COL is one of the following symbols: `candidate', `prefix',
`annotation'."
  (let ((col-fn (cond ((eq 'candidate col) 'car)
                      ((eq 'prefix col) 'cadr)
                      ((eq 'annotation col) 'caddr))))
    (corfu-pixel-perfect--string-pixel-width
     (string-join
      (cl-loop for x in cands collect (funcall col-fn x)) "\n"))))

(defun corfu-pixel-perfect--hide-annotation-maybe (cands curr)
  "Hide annotation conditionally.
CANDS is a list of triples of candidate string, prefix and suffix (annotation).
CURR is the index of the current selection."
  (when (memq major-mode corfu-pixel-perfect-ignore-annotation-modes)
    (cl-loop for triple in cands
             with i = 0
             do
             (unless (and (= i curr)
                          corfu-pixel-perfect-ignore-annotation-except-current)
               (let ((suffix (caddr triple)))
                 (add-text-properties 0 (length suffix) '(invisible corfu-pixel-perfect) suffix)
                 (setf (caddr triple) suffix)))
             (cl-incf i)))
  cands)

(defun corfu-pixel-perfect--truncate-string-to-pixel-width (str width)
  "Truncate string STR to WIDTH.
WIDTH is in pixels. If the string is longer than width when
rendered, it is truncated with the last character(s) replaced
with the result of `truncate-string-ellipsis'. If shorter,
returns an empty string."
  (if (> (corfu-pixel-perfect--string-pixel-width str) width)
      (let* ((glyphs (string-glyph-split str))
             (glyph-width (corfu-pixel-perfect--string-pixel-width (car glyphs)))
             (face (and glyphs (get-text-property 0 'face (car (last glyphs)))))
             (ellipsis (apply 'propertize (truncate-string-ellipsis) (if face `(face ,face))))
             (ellipsis-width (corfu-pixel-perfect--string-pixel-width ellipsis))
             result)
        (while (and glyphs (<= glyph-width width))
          (push (pop glyphs) result)
          (setq width (- width glyph-width)
                glyph-width (corfu-pixel-perfect--string-pixel-width (car glyphs))))

        (when (and glyphs result) ;; truncated
          (while (and result (> ellipsis-width width))
            (push (pop result) glyphs)
            (setq width (+ width glyph-width)
                  glyph-width (corfu-pixel-perfect--string-pixel-width (car result))))
          (push ellipsis result))

        (string-join (nreverse result)))
    str))

(defun corfu-pixel-perfect--truncate-proportionally-maybe (cands)
  "Truncate candidates CANDS proportionally if necessary."
  (when (eq corfu-pixel-perfect-ellipsis 'proportional)
    (let* ((cw (corfu-pixel-perfect--column-pixel-width cands 'candidate))
           (pw (corfu-pixel-perfect--column-pixel-width cands 'prefix))
           (sw (corfu-pixel-perfect--column-pixel-width cands 'annotation))
           (width (+ pw cw sw))
           (fw (default-font-width))
           (max-width (ceiling (* fw (min (- (frame-width) 4) corfu-max-width))))
           (min-width (ceiling (* fw corfu-min-width)))
           (excess-width (- width max-width)))
      (when (> excess-width 0)
        (let* ((denom (float (+ cw sw)))
               (adjusted-cw (max (ceiling (* min-width (/ cw denom)))
                                 (- cw (ceiling (* excess-width (/ cw denom))))))
               (adjusted-sw (max (floor (* min-width (/ sw denom)))
                                 (- sw (floor (* excess-width (/ sw denom)))))))
          (cl-loop for x in-ref cands do
                   (setf (car x) (corfu-pixel-perfect--truncate-string-to-pixel-width (car x) adjusted-cw))
                   (setf (caddr x) (corfu-pixel-perfect--truncate-string-to-pixel-width (caddr x) adjusted-sw)))))))
  cands)

(defun corfu-pixel-perfect--truncate-from-annotation-maybe (cands)
  "Truncate candidates CANDS from the annotations column first if necessary."
  (when (eq corfu-pixel-perfect-ellipsis 'annotation-first)
    (let* ((cw (corfu-pixel-perfect--column-pixel-width cands 'candidate))
           (pw (corfu-pixel-perfect--column-pixel-width cands 'prefix))
           (sw (corfu-pixel-perfect--column-pixel-width cands 'annotation))
           (width (+ pw cw sw))
           (fw (default-font-width))
           (min-width (ceiling (* fw corfu-min-width)))
           (max-width (ceiling (* fw (min (- (frame-width) 4) corfu-max-width))))
           (adjusted-cw cw)
           (adjusted-sw sw))

      (when (> width max-width)
        (setq adjusted-sw (max 0 (- sw (- width max-width)))
              width (+ pw cw adjusted-sw)))

      (when (< width min-width)
        (setq adjusted-sw (+ adjusted-sw (- min-width width))
              width (+ pw cw adjusted-sw)))

      (when (> width max-width)
        (setq adjusted-cw (max 0 (- cw (- width max-width)))
              width (+ pw adjusted-cw adjusted-sw)))

      (when (< width min-width)
        (setq adjusted-cw (+ adjusted-cw (- min-width width))
              width (+ pw adjusted-cw adjusted-sw)))

      (cl-loop for x in-ref cands
               do
               (cond ((and (< adjusted-cw cw) (> adjusted-cw 0))
                      (setf (car x) (corfu-pixel-perfect--truncate-string-to-pixel-width (car x) adjusted-cw)))
                     ((= adjusted-cw 0)
                      (setf (car x) "")))

               (cond ((and (< adjusted-sw sw) (> adjusted-sw 0))
                      (setf (caddr x) (corfu-pixel-perfect--truncate-string-to-pixel-width (caddr x) adjusted-sw)))
                     ((= adjusted-sw 0)
                      (setf (caddr x) ""))))))
  cands)

(defun corfu-pixel-perfect--format-candidates (cands curr ml mr)
  "Format annotated CANDS.
CURR is index of the currently selected candidate.
ML is the left margin padding in pixels on graphical displays or columns on the
terminal.
MR is the left margin padding in pixels on graphical displays or columns on the
terminal."
  ;; `corfu-current' may affect frame-width too
  (let ((i 0)
        (head cands))
    (while (and head (< i (length cands)) (< i curr))
      (pop head)
      (cl-incf i))

    (let* ((head (car head))
           (cand (substring (car head)))
           (prefix (substring (cadr head)))
           (suffix (substring (caddr head))))

      (add-face-text-property 0 (length cand) 'corfu-current t cand)
      (add-face-text-property 0 (length prefix) 'corfu-current t prefix)
      (add-face-text-property 0 (length suffix) 'corfu-current t suffix)

      (setf (car head) cand
            (cadr head) prefix
            (caddr head) suffix)))

  (let* ((cw (corfu-pixel-perfect--column-pixel-width cands 'candidate))
         (pw (corfu-pixel-perfect--column-pixel-width cands 'prefix))
         (sw (corfu-pixel-perfect--column-pixel-width cands 'annotation))
         (fw (default-font-width))
         (sw (+ sw (if (> sw 0) fw 0)))
         (width (max (+ pw cw sw) (* fw corfu-min-width)))
         (marginl (propertize " " 'display `(space :width (,ml))))
         (marginr (propertize " " 'display `(space :width (,mr))))
         (current-marginl (substring marginl))
         (current-marginr (substring marginr)))

    (add-face-text-property 0 (length current-marginl) 'corfu-current t current-marginl)
    (add-face-text-property 0 (length current-marginr) 'corfu-current t current-marginr)

    (cl-loop for (cand prefix suffix) being the elements of cands
             using (index i)
             ;; do not use relative space pixel params as they may lead to wrong
             ;; `string-pixel-width' results
             collect
             (concat
              (if (= i curr) current-marginl marginl)
              prefix
              (apply 'propertize " "
                     'display `(space :align-to (,(+ ml pw)))
                     (if (= i curr) '(face corfu-current)))
              cand
              (apply 'propertize " "
                     'display `(space :align-to (,(+ ml pw cw
                                                     ;; pads out the string to fit min width
                                                     (- width (+ pw cw sw))
                                                     (- sw (corfu-pixel-perfect--string-pixel-width suffix)))))
                     (if (= i curr) '(face corfu-current)))
              suffix
              (if (= i curr) current-marginr marginr)))))

(defun corfu-pixel-perfect--scroll-bar-range ()
  "Return the range of the scroll bar.

If a scroll bar is required, the value returned is an inclusive
range in a list with 2 elements, nil otherwise."
  (let* ((last (min (+ corfu--scroll corfu-count) corfu--total))
         (bar (ceiling (* corfu-count corfu-count) corfu--total))
         (lo (min (- corfu-count bar 1) (floor (* corfu-count corfu--scroll) corfu--total))))
    ;; Nonlinearity at the end and the beginning
    (when (/= corfu--scroll 0)
      (setq lo (max 1 lo)))
    (when (/= last corfu--total)
      (setq lo (min (- corfu-count bar 2) lo)))
    (and (> corfu--total corfu-count) (list lo bar))))

(defun corfu-pixel-perfect--candidates-popup (pos)
  "Show candidates popup at POS."
  (when (> corfu--total 0)
    (corfu--compute-scroll)
    (pcase-let* ((`(,lo ,bar) (corfu-pixel-perfect--scroll-bar-range))
                 (curr (- corfu--index corfu--scroll))
                 (cands (cl-loop repeat corfu-count
                                 for c in (nthcdr corfu--scroll corfu--candidates)
                                 collect (funcall corfu--hilit (substring c))))
                 (cands (corfu--affixate cands))
                 (cands (corfu-pixel-perfect--trim cands))
                 (pw (corfu-pixel-perfect--column-pixel-width cands 'prefix))
                 (fw (default-font-width))
                 ;; Disable the left margin if there are prefixes
                 (ml (if (> pw 0) 0 corfu-left-margin-width))
                 (ml (max 0 (ceiling (* fw ml))))
                 (mr (max 0 (ceiling (* fw corfu-right-margin-width))))
                 (offset (+ pw ml))
                 (cands (corfu-pixel-perfect--truncate-from-annotation-maybe cands))
                 (cands (corfu-pixel-perfect--truncate-proportionally-maybe cands))
                 (cands (corfu-pixel-perfect--hide-annotation-maybe cands curr))
                 (lines (corfu-pixel-perfect--format-candidates cands curr ml mr)))
      (corfu--popup-show pos offset nil lines curr lo bar))))

(cl-defmethod corfu--popup-show :around (pos off _ lines
                                             &context (corfu-pixel-perfect-mode (eql t))
                                             &optional _ lo bar)
  "Show LINES as popup at POS - PW.
OFF is the number of pixels on graphical displays or columns in the terminal to
move the popup to the left.
A scroll bar is displayed from LO to LO+BAR."
  (pcase-let ((`(,content-width . ,content-height)
               (corfu-pixel-perfect--string-pixel-size (string-join lines "\n")))
              (lh (default-line-height))
              (buf (current-buffer)))
    (with-current-buffer (corfu--make-buffer " *corfu*")
      (let* ((ch (default-line-height))
             (cw (default-font-width))
             (bw (max 0 (min 16 (ceiling (* cw corfu-bar-width)))))
             (bw (if lo (if (eq (buffer-local-value 'corfu-pixel-perfect-ellipsis buf) 'fast)
                            (ceiling (* cw corfu-bar-width))
                          bw)
                   0))
             (width (+ bw
                       (if (memq (buffer-local-value 'corfu-pixel-perfect-ellipsis buf) '(nil fast))
                           ;; -4 because of margins and some additional safety
                           (min (* cw (min (- (frame-width) 4) corfu-max-width)) content-width)
                         ;; anything else the content is already truncated
                         content-width)))
             (sbar (propertize " " 'display
                               (if (eq (buffer-local-value 'corfu-pixel-perfect-ellipsis buf) 'fast)
                                   `((margin right-margin)
                                     ,(propertize " " 'face 'corfu-bar))
                                 '(right-fringe corfu-pixel-perfect-scroll-bar corfu-bar))))
             (pos (posn-x-y pos))
             ;; XXX HACK: Minimum popup height must be at least 1 line of the
             ;; parent frame (gh:minad/corfu#261).
             (height (max lh content-height))
             (edge (window-body-pixel-edges))
             (border (alist-get 'internal-border-width corfu--frame-parameters))
             (x (max 0 (min (+ (car edge) (- (or (car pos) 0) off border))
                            (- (frame-pixel-width) width))))
             (yb (+ (cadr edge) (window-tab-line-height) (or (cdr pos) 0) lh))
             (y (if (> (+ yb (* corfu-count ch) lh lh) (frame-pixel-height))
                    (- yb height lh border border)
                  yb)))
        (with-silent-modifications
          (erase-buffer)

          ;; Adjust margin and fringe when a scroll bar is needed
          (if lo
              (if (eq (buffer-local-value 'corfu-pixel-perfect-ellipsis buf) 'fast)
                  (setq-local right-margin-width 1
                              right-fringe-width nil)
                (setq-local right-fringe-width bw
                            right-margin-width nil))
            (setq-local right-margin-width nil
                        right-fringe-width nil))

          ;; Enable ellipsis when the window text area is shorter than
          ;; the content-width
          (setf (alist-get 'no-special-glyphs corfu--frame-parameters)
                (if (buffer-local-value 'corfu-pixel-perfect-ellipsis buf)
                    (>= (- width bw) content-width)
                  t))

          (insert (string-join
                   (cl-loop for i from 0 to (1- (length lines))
                            with line = nil
                            do
                            (setq line (concat
                                        ;; add scroll bar at the beginning to
                                        ;; prevent it from being truncated
                                        (when (and lo (<= lo i (+ lo bar)))
                                          sbar)
                                        (pop lines)))
                            collect line)
                   "\n"))
          (goto-char (point-min)))

        (with-current-buffer (current-buffer)
          (setq corfu--frame (corfu--make-frame corfu--frame x y width height)))))))

(defvar corfu-pixel-perfect--corfu--frame-parameters nil)

;;;###autoload
(define-minor-mode corfu-pixel-perfect-mode
  "Corfu in pixel perfect alignment."
  :global t
  :group 'corfu-pixel-perfect
  (when corfu--frame
    (when-let ((buf (get-buffer " *corfu*")))
      (kill-buffer buf))
    (when (frame-live-p corfu--frame)
      (delete-frame corfu--frame))
    (setq corfu--frame nil))
  (if corfu-pixel-perfect-mode
      (progn
        (setq corfu-pixel-perfect--corfu--frame-parameters (copy-tree corfu--frame-parameters))
        (advice-add 'corfu--make-buffer :filter-return 'corfu-pixel-perfect--make-buffer-advice)
        (advice-add 'corfu--make-frame :around 'corfu-pixel-perfect--make-frame-advice)
        (advice-add 'corfu--format-candidates :override 'corfu-pixel-perfect--format-candidates)
        (advice-add 'corfu--candidates-popup :override 'corfu-pixel-perfect--candidates-popup))
    (setq corfu--frame-parameters corfu-pixel-perfect--corfu--frame-parameters)
    (advice-remove 'corfu--make-buffer 'corfu-pixel-perfect--make-buffer-advice)
    (advice-remove 'corfu--make-frame 'corfu-pixel-perfect--make-frame-advice)
    (advice-remove 'corfu--format-candidates 'corfu-pixel-perfect--format-candidates)
    (advice-remove 'corfu--candidates-popup 'corfu-pixel-perfect--candidates-popup)))

(provide 'corfu-pixel-perfect)

;;; corfu-pixel-perfect.el ends here
