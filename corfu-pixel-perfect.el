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

(defun corfu-pixel-perfect--make-buffer-advice (fn &rest args)
  "Set up buffer local variables for pixel perfection."
  (let ((orig-get-buffer-create (symbol-function 'get-buffer-create))
        buffer)

    (cl-letf (((symbol-function 'get-buffer-create)
               (lambda (name &optional _)
                 (funcall orig-get-buffer-create name t))))
      (setq buffer (apply fn args)))

    (with-current-buffer buffer
      (setq-local buffer-display-table corfu-pixel-perfect--display-table
                  left-fringe-width nil
                  right-fringe-width nil)
      (add-hook 'window-size-change-functions 'corfu-pixel-perfect--refresh-popup nil t)
      (add-hook 'window-size-change-functions 'corfu-pixel-perfect--reposition-corfu-popupinfo-frame nil t)
      (add-to-invisibility-spec 'corfu-pixel-perfect))

    buffer))

(defun corfu-pixel-perfect--make-frame-advice (fn &rest args)
  "Ensure buffer local variables take effect in FRAME."
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

    frame))

;; modified from `string-pixel-width' in subr-x.el
(defun corfu-pixel-perfect--string-pixel-size (string)
  "Return the size of STRING in pixels.

The return value is a `cons' cell where the `car' is the width and
`cdr' is the height."
  (if (zerop (length string))
      (cons 0 0)
    (with-current-buffer (get-buffer-create " *corfu-pixel-perfect--string-pixel-size*" t)
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

(defun corfu-pixel-perfect--add-face-to-triple (face triple)
  "Apply FACE to the strings in the list TRIPLE.

The result value is a deep copy of TRIPLE in addition to having
FACE applied to the 3 strings."
  (mapcar
   (lambda (s)
     (add-face-text-property 0 (length s) face t s)
     s)
   (mapcar 'substring triple)))

(cl-defmethod corfu--affixate :around (cands &context (corfu-pixel-perfect-mode (eql t)))
  (let ((result (cl-call-next-method cands)))
    (cdr result)))

(defun corfu-pixel-perfect--trim (cands)
  "Trim white space in candidates CANDS."
  (cl-loop for c in cands do
           (cl-loop for s in-ref c do
                    (setf s (string-trim s))))
  cands)

(defun corfu-pixel-perfect--prepare-candidates (cands)
  "Prepare completion candidates CANDS for formatting."
  (let* ((cands (cl-loop for c in cands
                         collect (funcall corfu--hilit (substring c))))
         (cands (corfu--affixate cands))
         (cands (corfu-pixel-perfect--trim cands)))
    cands))

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
           (max-width (* fw (min (- (frame-width) 4) corfu-max-width)))
           (min-width (- (* fw corfu-min-width) pw))
           (excess-width (- width max-width)))
      (when (> excess-width 0)
        (let* ((denom (+ cw sw))
               (adjusted-cw (max (floor (* min-width cw) denom)
                                 (- cw (floor (* excess-width cw) denom))))
               (adjusted-sw (max (ceiling (* min-width sw) denom)
                                 (- sw (ceiling (* excess-width sw) denom)))))
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
           (min-width (* fw corfu-min-width))
           (max-width (* fw (min (- (frame-width) 4) corfu-max-width)))
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
  (when-let ((selected (nth curr cands)))
    (pcase-let ((`(,cand ,prefix ,suffix)
                 (corfu-pixel-perfect--add-face-to-triple
                  'corfu-current selected)))
      (setf (car selected) cand
            (cadr selected) prefix
            (caddr selected) suffix)))

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

(defun corfu-pixel-perfect--show-scroll-bar-p ()
  (not (not (corfu-pixel-perfect--scroll-bar-range))))

(defun corfu-pixel-perfect--bar-pixel-width ()
  "Scroll bar width in pixels."
  (let* ((ellipsis (buffer-local-value 'corfu-pixel-perfect-ellipsis (current-buffer)))
         (cw (default-font-width))
         (bw (max 0 (min 16 (ceiling (* cw corfu-bar-width))))))

    (if (eq ellipsis 'fast)
        (* cw (ceiling corfu-bar-width))
      bw)))

(defun corfu-pixel-perfect--refresh-buffer (buffer lines)
  "Rerender BUFFER with LINES and refresh scroll bar position."
  (pcase-let* ((`(,lo ,bar) (corfu-pixel-perfect--scroll-bar-range))
               (fw (default-font-width))
               (bw (corfu-pixel-perfect--bar-pixel-width))
               (ellipsis (buffer-local-value 'corfu-pixel-perfect-ellipsis (current-buffer)))
               (sbar (propertize " " 'display
                                 (if (eq ellipsis 'fast)
                                     `((margin right-margin)
                                       ,(propertize (make-string (/ bw fw) ?\s)'face 'corfu-bar))
                                   '(right-fringe corfu-pixel-perfect-scroll-bar corfu-bar)))))
    (with-current-buffer buffer
      (with-silent-modifications
        (delete-region (point-min) (point-max))

        ;; Adjust margin and fringe when a scroll bar is needed
        (if lo
            (if (eq ellipsis 'fast)
                (setq-local right-margin-width (/ bw fw)
                            right-fringe-width nil)
              (setq-local right-fringe-width bw
                          right-margin-width nil))
          (setq-local right-margin-width nil
                      right-fringe-width nil))

        (insert (string-join
                 (cl-loop for i from 0 to (1- (length lines))
                          collect
                          (concat
                           ;; prepend scroll bar so it doesn't get truncated
                           ;; when resizing
                           (when (and lo (<= lo i (+ lo bar)))
                             sbar)
                           (pop lines)))
                 "\n"))

        (goto-char (point-min))))))

(defun corfu-pixel-perfect--set-frame-position (frame pos off)
  "Set FRAME position to POS - OFF.

POS is the result of `posn-at-point'.  OFF is the number of
pixels on graphical displays or columns on the terminal to offset
the top-left corner of the frame to the left."
  (let* ((lh (default-line-height))
         (win (frame-root-window frame))
         (content-height (with-selected-window win
                           (cdr (window-text-pixel-size))))
         (ch (with-current-buffer (window-buffer win)
               (default-line-height)))
         (pos (posn-x-y pos))
         ;; XXX HACK: Minimum popup height must be at least 1 line of the
         ;; parent frame (gh:minad/corfu#261).
         (height (max lh content-height))
         (edge (window-body-pixel-edges))
         (border (alist-get 'internal-border-width corfu--frame-parameters))
         (x (max 0 (min (+ (car edge) (- (or (car pos) 0) off border))
                        (- (frame-native-width) (frame-native-width frame)))))
         (yb (+ (cadr edge) (window-tab-line-height) (or (cdr pos) 0) lh))
         (y (if (> (+ yb (* corfu-count ch) lh lh) (frame-native-height))
                (- yb height lh border border)
              yb)))

    (pcase-let ((`(,px . ,py) (frame-position frame)))
      (unless (and (= x px) (= y py))
        (set-frame-position frame x y)))

    frame))

(defun corfu-pixel-perfect--guess-width ()
  "Estimate a width covering most of the completion candidates.

Assuming the widths of the completion candidate strings form a
normal distribution, this function samples at most 1/10 of the
population to calculate an average of averages and an average of
standard deviations. The value 3 standard deviations greater than
the mean of means is returned, which should be greater than
99.86% of the widths."
  (pcase-let* ((n 30)
               (N (max 3 (/ corfu--total n 10)))
               (`(,mean . ,stddev)
                (cl-loop repeat N
                         with M = 0
                         with S = 0
                         do
                         (let* ((samples (cl-loop repeat n collect (seq-random-elt corfu--candidates)))
                                (samples (corfu-pixel-perfect--prepare-candidates samples))
                                (lengths
                                 (cl-loop for s in samples
                                          collect
                                          (thread-last
                                            (corfu-pixel-perfect--add-face-to-triple 'corfu-current s)
                                            (mapcar 'corfu-pixel-perfect--string-pixel-width)
                                            (apply '+))))
                                (x-bar (/ (apply '+ lengths) (float n)))
                                (s (sqrt (/ (cl-loop for l in lengths
                                                     sum (* (- l x-bar) (- l x-bar)))
                                            (float (- n 1))))))
                           (setq M (+ M x-bar)
                                 S (+ S s)))
                         finally return (cons (/ M N) (/ S N)))))
    (ceiling (+ mean (* 3 stddev)))))

(defun corfu-pixel-perfect--candidates-popup (pos)
  "Show candidates popup at POS."
  (if (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (corfu-pixel-perfect--refresh-popup corfu--frame pos t)
    (corfu--compute-scroll)
    (let* ((curr (- corfu--index corfu--scroll))
           (cands (corfu-pixel-perfect--prepare-candidates
                   (take corfu-count (nthcdr corfu--scroll corfu--candidates))))
           (pw (corfu-pixel-perfect--column-pixel-width cands 'prefix))
           (fw (default-font-width))
           ;; Disable the left margin if there are prefixes
           (ml (if (> pw 0) 0 corfu-left-margin-width))
           (ml (max 0 (ceiling (* fw ml))))
           (mr (max 0 (ceiling (* fw corfu-right-margin-width))))
           (offset (+ pw ml))
           (corfu-min-width
            (min corfu-max-width
                 (max corfu-min-width
                      ;; 90 because it's a multiple of 30 that we rarely get
                      ;; unless programming in Elisp using a completion style
                      ;; such as flex, orderless, prescient or flx. 90 also
                      ;; seems like a number that performance degradation
                      ;; becomes perceivable.
                      (if (> corfu--total 90)
                          (/ (corfu-pixel-perfect--guess-width) (float fw))
                        corfu-min-width))))
           (cands (corfu-pixel-perfect--truncate-from-annotation-maybe cands))
           (cands (corfu-pixel-perfect--truncate-proportionally-maybe cands))
           (cands (corfu-pixel-perfect--hide-annotation-maybe cands curr))
           (lines (corfu-pixel-perfect--format-candidates cands curr ml mr)))
      (corfu--popup-show pos offset nil lines curr))))

(cl-defmethod corfu--popup-show :around (pos off _ lines
                                             &context (corfu-pixel-perfect-mode (eql t))
                                             &optional _ _ _)
  "Show LINES in a popup at POS - OFF.
OFF is the number of pixels on graphical displays or columns in
the terminal to offset the popup to the left."
  (pcase-let* ((`(,content-width . ,content-height)
                (corfu-pixel-perfect--string-pixel-size (string-join lines "\n")))
               (lh (default-line-height))
               (ellipsis (buffer-local-value 'corfu-pixel-perfect-ellipsis (current-buffer)))
               (cw (default-font-width))
               (bw (if (corfu-pixel-perfect--show-scroll-bar-p)
                       (corfu-pixel-perfect--bar-pixel-width)
                     0))
               (width (+ bw
                         (if (memq ellipsis '(nil fast))
                             ;; -4 because of margins and some additional safety
                             (min (* cw (min (- (frame-width) 4) corfu-max-width)) content-width)
                           ;; anything else the content is already truncated
                           content-width)))
               (height (max lh content-height)))

    ;; Enable ellipsis when the window text area is shorter than
    ;; the content-width
    (setf (alist-get 'no-special-glyphs corfu--frame-parameters)
          (if (eq ellipsis 'fast)
              (>= (- width bw) content-width)
            t))

    (with-current-buffer (corfu--make-buffer " *corfu*")
      (corfu-pixel-perfect--refresh-buffer (current-buffer) lines)
      (setq corfu--frame (corfu--make-frame corfu--frame 0 0 width height)))

    (make-frame-visible (corfu-pixel-perfect--set-frame-position corfu--frame pos off))))

(defun corfu-pixel-perfect--refresh-popup (frame-or-window &optional pos fit-height)
  "Refresh popup content.

If FRAME-OR-WINDOW is a frame, the buffer of its root window is
the target, otherwise FRAME-OR-WINDOW must be a window and the
target is the buffer in it.

The popup frame is refreshed if and only if POS is non-nil or if
its size has changed.

When FIT-HEIGHT is non-nil, resize the height of the frame to fit
the height of the content."
  (let ((frame (cond ((framep frame-or-window) frame-or-window)
                     ((windowp frame-or-window) (window-frame frame-or-window)))))

    (when (and (frame-live-p frame)
               (eq frame corfu--frame)
               (or (frame-size-changed-p frame) pos))

      (pcase-let* ((inhibit-redisplay t)
                   (ellipsis (buffer-local-value 'corfu-pixel-perfect-ellipsis (current-buffer)))
                   (corfu-count (frame-text-lines frame))
                   (corfu--scroll corfu--scroll)
                   (corfu--scroll (corfu--compute-scroll))
                   (curr (- corfu--index corfu--scroll))
                   (cands (corfu-pixel-perfect--prepare-candidates
                           (take corfu-count (nthcdr corfu--scroll corfu--candidates))))
                   (fw (default-font-width))
                   (pw (corfu-pixel-perfect--column-pixel-width cands 'prefix))
                   (ml (if (> pw 0) 0 corfu-left-margin-width))
                   (ml (max 0 (ceiling (* fw ml))))
                   (mr (max 0 (ceiling (* fw corfu-right-margin-width))))
                   (bw (if (corfu-pixel-perfect--show-scroll-bar-p)
                           (corfu-pixel-perfect--bar-pixel-width)
                         0))
                   ;; keeping this in floating point for precision
                   (corfu-max-width (/ (- (frame-text-width frame) bw ml mr) (float fw)))
                   (corfu-min-width corfu-max-width)
                   (cands (corfu-pixel-perfect--truncate-from-annotation-maybe cands))
                   (cands (corfu-pixel-perfect--truncate-proportionally-maybe cands))
                   (cands (corfu-pixel-perfect--hide-annotation-maybe cands curr))
                   (lines (corfu-pixel-perfect--format-candidates cands curr ml mr))
                   (`(,content-width . ,content-height)
                    (corfu-pixel-perfect--string-pixel-size (string-join lines "\n")))
                   (frame-buffer (window-buffer (frame-root-window frame))))

        (corfu-pixel-perfect--refresh-buffer frame-buffer lines)
        (set-window-buffer (frame-root-window frame) frame-buffer)
        (set-frame-parameter frame 'no-special-glyphs
                             (if (eq ellipsis 'fast)
                                 (>= (- (frame-text-width frame) bw) content-width)
                               t))

        (when fit-height
          (set-frame-height frame (max (default-line-height) content-height) nil t))

        (when pos
          (corfu-pixel-perfect--set-frame-position frame pos (+ pw ml)))))

    frame))

;; NOTE: on macOS, when resizing a frame from top left, the position doesn't
;; change. Fix won't be available until Emacs 31.
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=74074
(defun corfu-pixel-perfect--reposition-corfu-popupinfo-frame (frame-or-window)
  "Synchronize the positions of the completion and the info popup frames.

If FRAME-OR-WINDOW is a frame, the buffer of its root window is
the target, otherwise FRAME-OR-WINDOW must be a window and the
target is the buffer in it."
  (let ((frame (cond ((framep frame-or-window) frame-or-window)
                     ((windowp frame-or-window) (window-frame frame-or-window)))))
    (when (and corfu-popupinfo-mode
               (frame-live-p frame)
               (frame-live-p corfu-popupinfo--frame)
               (eq frame corfu--frame)
               (frame-size-changed-p frame))
      (pcase-let* ((`(,x . ,y) (frame-position frame))
                   (width (frame-native-width frame))
                   (border (alist-get 'internal-border-width corfu--frame-parameters)))
        (with-selected-frame corfu-popupinfo--frame
          (set-frame-position corfu-popupinfo--frame (+ x width (- border)) y))))))

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
        (advice-add #'corfu--make-buffer :around #'corfu-pixel-perfect--make-buffer-advice)
        (advice-add #'corfu--make-frame :around #'corfu-pixel-perfect--make-frame-advice)
        (advice-add #'corfu--format-candidates :override #'corfu-pixel-perfect--format-candidates)
        (advice-add #'corfu--candidates-popup :override #'corfu-pixel-perfect--candidates-popup))
    (setq corfu--frame-parameters corfu-pixel-perfect--corfu--frame-parameters)
    (advice-remove #'corfu--make-buffer #'corfu-pixel-perfect--make-buffer-advice)
    (advice-remove #'corfu--make-frame #'corfu-pixel-perfect--make-frame-advice)
    (advice-remove #'corfu--format-candidates #'corfu-pixel-perfect--format-candidates)
    (advice-remove #'corfu--candidates-popup #'corfu-pixel-perfect--candidates-popup)))

(provide 'corfu-pixel-perfect)

;;; corfu-pixel-perfect.el ends here
