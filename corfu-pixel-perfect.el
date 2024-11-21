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
(require 'corfu-popupinfo)
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

(defgroup corfu-pixel-perfect-faces nil
  "Faces used by `corfu-pixel-perfect'."
  :group 'corfu-pixel-perfect
  :group 'faces)

(defface corfu-pixel-perfect-mouse
  '((t :inherit highlight))
  "Face used when hovering on a candidate with a mouse."
  :group 'corfu-pixel-perfect-faces)

(define-fringe-bitmap 'corfu-pixel-perfect-scroll-bar [])

(defconst corfu-pixel-perfect--display-table
  (let ((dt (make-display-table)))
    (set-display-table-slot dt 'truncation ?\x2026)
    dt)
  "Truncation ellipsis when `corfu-pixel-perfect-ellipsis' is `fast'")

(defun corfu-pixel-perfect-select-and-insert (event)
  "Select the candidate under the pointer and insert it.

EVENT is a mouse click event."
  (interactive "e")
  (pcase-let* ((posn (event-end event))
               (win (posn-window posn))
               (area (posn-area posn))
               (`(,_ . ,row) (posn-actual-col-row posn)))
    (when (and (windowp win) (not area) (numberp row))
      (corfu--goto (+ corfu--scroll row))
      (corfu-insert))))

(defvar-keymap corfu-pixel-perfect-mouse-map
  :doc "Allow mouse click for candidate selection."
  :parent corfu--mouse-ignore-map
  "<mouse-1>" #'corfu-pixel-perfect-select-and-insert
  "C-M-<mouse-1>" #'corfu-pixel-perfect-select-and-insert)

(defvar-local corfu-pixel-perfect-mouse-highlight-ov nil
  "The overlay used to highlight a row on mouse hover.")

(defun corfu-pixel-perfect--mouse-highlight-row (row)
  "Highlight ROW in the popup buffer."
  (with-current-buffer (window-buffer (frame-root-window corfu--frame))
    (save-excursion
      (goto-char (point-min))
      (forward-line row)
      (let ((bol (pos-bol))
            (eol (pos-eol))
            (ov corfu-pixel-perfect-mouse-highlight-ov))
        (cond ((and (overlayp ov)
                    (or (/= bol (overlay-start ov))
                        (/= eol (overlay-end ov))))
               (move-overlay ov (pos-bol) (pos-eol)))

              ((not ov)
               (setq-local corfu-pixel-perfect-mouse-highlight-ov (make-overlay bol eol))
               (overlay-put corfu-pixel-perfect-mouse-highlight-ov 'mouse-face 'corfu-pixel-perfect-mouse)))))))

(defun corfu-pixel-perfect--setup-mouse-timer (frame)
  "Set up a timer in the popup FRAME to highlight rows on hover."
  (run-with-timer
   (/ 1 (float 60)) (/ 1 (float 60))
   (lambda ()
     (when (and (frame-live-p frame)
                (frame-visible-p frame))
       (pcase-let* ((`(,f ,x . ,y) (mouse-pixel-position)))
         (when (eq f frame)
           (pcase-let* ((posn (posn-at-x-y x y f))
                        (win (posn-window posn))
                        (area (posn-area posn))
                        (`(,_ . ,row) (posn-actual-col-row posn)))
             ;; highlight current line
             (when (and (windowp win) (not area) (numberp row))
               (corfu-pixel-perfect--mouse-highlight-row row)))))))))

(defconst corfu-pixel-perfect--buffer-name " *corfu-pixel-perfect*")

(defun corfu-pixel-perfect--make-buffer (fn &rest args)
  "Set up buffer local variables for pixel perfection."
  (let* ((orig-frame (selected-frame))
         (orig-win (selected-window))
         (orig-buf (current-buffer))
         (new-buffer (apply fn args)))
    (when (equal (car args) corfu-pixel-perfect--buffer-name)
      (with-current-buffer new-buffer
        (setq-local buffer-display-table corfu-pixel-perfect--display-table
                    left-fringe-width nil
                    right-fringe-width nil
                    global-hl-line-mode nil)
        (add-to-invisibility-spec 'corfu-pixel-perfect)
        (use-local-map corfu-pixel-perfect-mouse-map)
        (keymap-local-set "<remap> <self-insert-command>"
                          (lambda (n &optional c)
                            (interactive (list (prefix-numeric-value current-prefix-arg) last-command-event))
                            (with-selected-frame orig-frame
                              (with-selected-window orig-win
                                (with-current-buffer orig-buf
                                  (self-insert-command n c))))))
        (setf (alist-get #'corfu-pixel-perfect-mode minor-mode-overriding-map-alist) corfu-map)
        (setq-local mwheel-scroll-up-function #'corfu-next)
        (setq-local mwheel-scroll-down-function #'corfu-previous)
        (add-hook 'window-size-change-functions #'corfu-pixel-perfect--refresh-popup nil 'local)
        (add-hook 'window-size-change-functions #'corfu-pixel-perfect--reposition-corfu-popupinfo-frame nil 'local)
        (add-hook 'pre-command-hook #'corfu--prepare nil 'local)
        (add-hook 'post-command-hook #'corfu--post-command nil 'local)))

    new-buffer))

(defun corfu-pixel-perfect--make-frame (fn &rest args)
  "Ensure buffer local variables take effect in FRAME."
  (make-frame-visible
   ;; Setting the fringe on the frame via buffer local vars is just crazy...
   (let* ((left-fringe-width 0)
          (right-fringe-width 0)
          (default-minibuffer-frame (selected-frame))
          ;; (x-pointer-shape (if (boundp 'x-pointer-hand1) x-pointer-hand1))
          ;; (x-sensitive-text-pointer-shape x-pointer-shape)
          (frame (cl-letf (((symbol-function 'make-frame-visible) (symbol-function 'ignore)))
                   (apply fn args)))
          (win (frame-root-window frame))
          (buf (window-buffer win)))

     ;; Virtually no perf hit here, I have no idea why upstream is guarding it
     ;; with some complicated frame parameter diff.
     (set-window-buffer win buf)

     frame)))

(defconst corfu-pixel-perfect--advised-functions
  '(corfu--update
    corfu--range-valid-p
    corfu--continue-p
    corfu--exhibit
    corfu--auto-post-command
    corfu--auto-complete-deferred
    corfu--auto-tick
    corfu--replace
    corfu--done
    corfu--insert
    corfu-pixel-perfect--refresh-popup))

(defun corfu-pixel-perfect--wrap-functions (fns)
  "Add wrapper to the list of functions FNS.

The wrapper will ensure when the functions are called, the
selected frame, window and the current buffer will always be ones
that triggered the popup."
  (let* ((frame (selected-frame))
         (win (selected-window))
         (buf (current-buffer))
         (advice (lambda (fn &rest args)
                   (when (and (frame-live-p frame)
                              (frame-visible-p frame)
                              (window-live-p win)
                              (buffer-live-p buf))
                     (with-selected-frame frame
                       (with-selected-window win
                         (with-current-buffer buf
                           (apply fn args))))))))
    (while fns
      (advice-add (pop fns) :around advice '((name . "corfu-pixel-perfect-wrapper"))))))

(defun corfu-pixel-perfect--unwrap-functions (fns)
  "Remove wrapper from the list of functions FNS."
  (while fns
    (advice-remove (pop fns) "corfu-pixel-perfect-wrapper")))

(defun corfu-pixel-perfect--commands (keymap)
  "Return a list of all the commands in KEYMAP."
  (cl-loop for _ being the key-codes of keymap
           using (key-bindings b)
           append
           (cond ((keymapp b)
                  (corfu-pixel-perfect--commands b))
                 ((commandp b) (list b)))))

(defun corfu-pixel-perfect--window-change (window)
  "Window and buffer change hook which quits Corfu.

Quits the pop up if WINDOW is not the origin buffer window or the
popup frame window."
  (if (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame)
           (not (eq (selected-window) (frame-root-window corfu--frame)))
           (not (eq (selected-window) window)))
      (corfu-quit)
    (corfu--window-change window)))

(defun corfu-pixel-perfect--setup (beg end table pred)
  "Setup Corfu completion state.
See `completion-in-region' for the arguments BEG, END, TABLE, PRED."
  (setq beg (if (markerp beg) beg (copy-marker beg))
        end (if (and (markerp end) (marker-insertion-type end)) end (copy-marker end t))
        completion-in-region--data (list beg end table pred completion-extra-properties))

  (corfu-pixel-perfect--wrap-functions
   (append corfu-pixel-perfect--advised-functions
           (corfu-pixel-perfect--commands corfu-map)))

  (completion-in-region-mode 1)
  (activate-change-group (setq corfu--change-group (prepare-change-group)))
  (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) corfu-map)
  (add-hook 'pre-command-hook #'corfu--prepare nil 'local)
  (add-hook 'window-selection-change-functions #'corfu-pixel-perfect--window-change nil 'local)
  (add-hook 'window-buffer-change-functions #'corfu-pixel-perfect--window-change nil 'local)
  (remove-hook 'post-command-hook #'completion-in-region--postch)
  (add-hook 'post-command-hook #'corfu--post-command nil 'local)
  (add-hook 'completion-in-region-mode-hook #'corfu-pixel-perfect--teardown nil 'local))

(defun corfu-pixel-perfect--teardown ()
  "Teardown Corfu completion state."
  (unless completion-in-region-mode

    (corfu-pixel-perfect--unwrap-functions
     (append corfu-pixel-perfect--advised-functions
             (corfu-pixel-perfect--commands corfu-map)))

    (remove-hook 'window-selection-change-functions #'corfu-pixel-perfect--window-change 'local)
    (remove-hook 'window-buffer-change-functions #'corfu-pixel-perfect--window-change 'local)
    (remove-hook 'completion-in-region-mode-hook #'corfu-pixel-perfect--teardown 'local)
    (remove-hook 'post-command-hook #'corfu--post-command 'local)
    (funcall 'corfu--teardown (current-buffer))))

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
     (string-join (mapcar col-fn cands) "\n"))))

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
              (substring prefix)
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

(defun corfu-pixel-perfect--bar-pixel-width (ellipsis)
  "Scroll bar width in pixels.

ELLIPSIS is the file buffer's local value of
`corfu-pixel-perfect-ellipsis'."
  (let* ((cw (default-font-width))
         (bw (max 0 (min 16 (ceiling (* cw corfu-bar-width))))))

    (if (eq ellipsis 'fast)
        (* cw (ceiling corfu-bar-width))
      bw)))

(defun corfu-pixel-perfect--refresh-buffer (buffer lines ellipsis)
  "Rerender BUFFER with LINES and refresh scroll bar position.

ELLIPSIS is the file buffer's local value of
`corfu-pixel-perfect-ellipsis'."
  (pcase-let* ((`(,lo ,bar) (corfu-pixel-perfect--scroll-bar-range))
               (fw (default-font-width))
               (bw (corfu-pixel-perfect--bar-pixel-width ellipsis))
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

(defun corfu-pixel-perfect--compute-frame-position (parent-frame pos off width height)
  "Compute the popup frame position.

PARENT-FRAME is the parent frame of the popup child frame.
POS is the position at point.
OFF the is number of pixels on graphical displays or columns in the terminal to
offset the top left position of the popup.
WIDTH is the width of the popup frame's text area.
HEIGHT is the height of the popup frame's text area."
  (pcase-let* ((lh (with-current-buffer (window-buffer (get-mru-window parent-frame))
                     (default-line-height)))
               (ch (default-line-height))
               (`(,pos-x . ,pos-y) (posn-x-y pos))
               (`(,left-edge ,top-edge) (window-body-pixel-edges))
               (border (alist-get 'internal-border-width corfu--frame-parameters))
               (x (max 0 (min (+ left-edge (- (or pos-x 0) off border))
                              (- (frame-native-width parent-frame) width))))
               (yb (+ top-edge (window-tab-line-height) (or pos-y 0) lh))
               (y (if (> (+ yb (* corfu-count ch) lh lh) (frame-native-height parent-frame))
                      (- yb height lh border border)
                    yb)))
    (cons x y)))

(defun corfu-pixel-perfect--guess-width ()
  "Estimate a width covering most of the completion candidates.

Using the central limit theorem, this function samples at most
the larger of 100 or 1/10 of the population to calculate an
average of averages and an average of standard deviations. The
value 3 standard deviations above the mean of means is returned,
which should be greater than 99.86% of the widths."
  (pcase-let* ((n 25)
               (N (max 4 (/ corfu--total n 10)))
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
                                            s
                                            (corfu-pixel-perfect--add-face-to-triple 'corfu-current)
                                            string-join
                                            corfu-pixel-perfect--string-pixel-width)))
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
      (corfu-pixel-perfect--refresh-popup corfu--frame pos)
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
                      ;; 100 because it's a multiple of 25 that we rarely get
                      ;; unless programming in Elisp using a completion style
                      ;; such as flex, orderless, prescient or flx. 100 also
                      ;; seems like a number that performance degradation
                      ;; becomes perceivable.
                      (if (> corfu--total 100)
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
               (ellipsis corfu-pixel-perfect-ellipsis)
               (cw (default-font-width))
               (bw (if (corfu-pixel-perfect--show-scroll-bar-p)
                       (corfu-pixel-perfect--bar-pixel-width ellipsis)
                     0))
               (width (+ bw
                         (if (memq ellipsis '(nil fast))
                             ;; -4 because of margins and some additional safety
                             (min (* cw (min (- (frame-width) 4) corfu-max-width)) content-width)
                           ;; anything else the content is already truncated
                           content-width)))
               ;; XXX HACK: Minimum popup height must be at least 1 line of the
               ;; parent frame (gh:minad/corfu#261).
               (height (max lh content-height)))

    ;; Enable ellipsis when the window text area is shorter than
    ;; the content-width
    (setf (alist-get 'no-special-glyphs corfu--frame-parameters)
          (if (eq ellipsis 'fast)
              (>= (- width bw) content-width)
            t))

    (with-current-buffer (corfu--make-buffer corfu-pixel-perfect--buffer-name)
      (corfu-pixel-perfect--refresh-buffer (current-buffer) lines ellipsis)
      (pcase-let ((`(,x . ,y)
                   (corfu-pixel-perfect--compute-frame-position
                    (selected-frame) pos off width height)))
        (setq corfu--frame (corfu--make-frame corfu--frame x y width height))))

    (set-frame-parameter
     corfu--frame 'corfu-pixel-perfect--mouse-timer
     (corfu-pixel-perfect--setup-mouse-timer corfu--frame))

    corfu--frame))

(defun corfu-pixel-perfect--refresh-popup (frame-or-window &optional pos force)
  "Refresh popup content.

If FRAME-OR-WINDOW is a frame, the buffer of its root window is
the target, otherwise FRAME-OR-WINDOW must be a window and the
target is the buffer in it.

The popup frame is refreshed if and only if POS is non-nil or if
its size has changed."
  (let* ((frame (cond ((framep frame-or-window) frame-or-window)
                      ((windowp frame-or-window) (window-frame frame-or-window))))
         (win (frame-root-window frame))
         (buf (window-buffer win)))

    (when (and (frame-live-p frame)
               (eq frame corfu--frame)
               (or (frame-size-changed-p frame) pos force))

      (pcase-let* ((inhibit-redisplay t)
                   (ellipsis corfu-pixel-perfect-ellipsis)
                   (cands (take corfu-count (nthcdr corfu--scroll corfu--candidates)))
                   (corfu-count (max (frame-text-lines frame) (length cands)))
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
                           (corfu-pixel-perfect--bar-pixel-width ellipsis)
                         0))
                   ;; keeping this in floating point for precision
                   (corfu-max-width (/ (- (frame-text-width frame) bw ml mr) (float fw)))
                   (corfu-min-width corfu-max-width)
                   (cands (corfu-pixel-perfect--truncate-from-annotation-maybe cands))
                   (cands (corfu-pixel-perfect--truncate-proportionally-maybe cands))
                   (cands (corfu-pixel-perfect--hide-annotation-maybe cands curr))
                   (lines (corfu-pixel-perfect--format-candidates cands curr ml mr))
                   (`(,content-width . ,content-height)
                    (corfu-pixel-perfect--string-pixel-size (string-join lines "\n"))))

        (corfu-pixel-perfect--refresh-buffer buf lines ellipsis)
        (set-frame-parameter frame 'no-special-glyphs
                             (if (eq ellipsis 'fast)
                                 (>= (- (frame-text-width frame) bw) content-width)
                               t))

        (when pos
          (pcase-let* ((parent-frame (frame-parent frame))
                       (lh (with-selected-frame parent-frame
                             (default-line-height)))
                       (height (max lh content-height))
                       (width (frame-native-width frame))
                       (`(,x . ,y)
                        (with-current-buffer buf
                          (corfu-pixel-perfect--compute-frame-position
                           parent-frame pos (+ pw ml) width height)))
                       (`(,px . ,py) (frame-position frame)))
            (set-frame-height frame height nil t)
            (unless (and (= x px) (= y py))
              (set-frame-position frame x y))))))

    (set-window-buffer win buf)

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

(defun corfu-pixel-perfect--hide-frame-deferred (frame)
  "Set focus back to the parent of the popup FRAME."
  (when (eq frame corfu--frame)
    (when-let ((timer (frame-parameter corfu--frame 'corfu-pixel-perfect--mouse-timer)))
      (cancel-timer timer)
      (set-frame-parameter corfu--frame 'corfu-pixel-perfect--mouse-timer nil))
    (select-frame-set-input-focus (frame-parent frame))))

(defvar corfu-pixel-perfect--corfu--frame-parameters nil)

;;;###autoload
(define-minor-mode corfu-pixel-perfect-mode
  "Corfu in pixel perfect alignment."
  :global t
  :group 'corfu-pixel-perfect
  (when-let ((buf (get-buffer " *corfu*")))
    (kill-buffer buf))
  (if corfu-pixel-perfect-mode
      (progn
        (cl-pushnew 'mwheel-scroll corfu-continue-commands)
        (cl-pushnew 'handle-switch-frame corfu-continue-commands)
        (setq corfu-pixel-perfect--corfu--frame-parameters (copy-tree corfu--frame-parameters))
        (setf (alist-get 'no-accept-focus corfu--frame-parameters nil t) nil)
        (advice-add #'corfu--make-buffer :around #'corfu-pixel-perfect--make-buffer)
        (advice-add #'corfu--make-frame :around #'corfu-pixel-perfect--make-frame)
        (advice-add #'corfu--format-candidates :override #'corfu-pixel-perfect--format-candidates)
        (advice-add #'corfu--candidates-popup :override #'corfu-pixel-perfect--candidates-popup)
        (advice-add #'corfu--setup :override #'corfu-pixel-perfect--setup)
        (advice-add #'corfu--hide-frame-deferred :after #'corfu-pixel-perfect--hide-frame-deferred))
    (when-let ((buf (get-buffer corfu-pixel-perfect--buffer-name)))
      (kill-buffer buf))
    (cl-delete 'mwheel-scroll corfu-continue-commands)
    (cl-delete 'handle-switch-frame corfu-continue-commands)
    (setq corfu--frame-parameters corfu-pixel-perfect--corfu--frame-parameters)
    (advice-remove #'corfu--make-buffer #'corfu-pixel-perfect--make-buffer)
    (advice-remove #'corfu--make-frame #'corfu-pixel-perfect--make-frame)
    (advice-remove #'corfu--format-candidates #'corfu-pixel-perfect--format-candidates)
    (advice-remove #'corfu--candidates-popup #'corfu-pixel-perfect--candidates-popup)
    (advice-remove #'corfu--setup #'corfu-pixel-perfect--setup)
    (advice-remove #'corfu--hide-frame-deferred #'corfu-pixel-perfect--hide-frame-deferred)))

(provide 'corfu-pixel-perfect)

;;; corfu-pixel-perfect.el ends here
