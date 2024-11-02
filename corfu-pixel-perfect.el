;;; corfu-pixel-perfect.el --- Corfu in pixel precision -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jimmy Yuen Ho Wong

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Created: 2024-10-30
;; Version: 0.1
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
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup corfu-pixel-perfect nil
  "Corfu Pixel Perfect."
  :group 'corfu
  :prefix "corfu-pixel-perfect")

(defcustom corfu-pixel-perfect-ellipsis nil
  "Whether to show ellipsis.

If t, the bar is shown on the fringe with no truncation indicator.
If nil, the bar is shown on the margin with ellipsis as the truncation
indicator."
  :type 'boolean)

(define-fringe-bitmap 'corfu-pixel-perfect-scroll-bar [])

(defconst corfu-pixel-perfect--display-table
  (let ((dt (make-display-table)))
    (set-display-table-slot dt 'truncation ?\x2026)
    dt)
  "Truncation ellipsis when `corfu-pixel-perfect-ellipsis' is t")

(defun corfu-pixel-perfect--make-buffer-advice (buffer)
  "Put a display table with an ellipsis on BUFFER."
  (with-current-buffer buffer
    (setq-local buffer-display-table corfu-pixel-perfect--display-table
                left-fringe-width nil
                right-fringe-width nil
                fringe-indicator-alist nil))
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

(defun corfu-pixel-perfect--replace-newlines (cands)
  "Replace newlines in candidates CANDS."
  (cl-loop for c in cands do
           (cl-loop for s in-ref c do
                    (setf s (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s))))
  cands)

(defun corfu-pixel-perfect--format-candidates (cands curr ml mr)
  "Format annotated CANDS.
CURR is index of the currently selected candidate.
ML is the left margin padding in pixels on graphical displays or columns on the
terminal.
MR is the left margin padding in pixels on graphical displays or columns on the
terminal."
  (let* ((cw (string-pixel-width (string-join (cl-loop for x in cands collect (car x)) "\n")))
         (pw (string-pixel-width (string-join (cl-loop for x in cands collect (cadr x)) "\n")))
         (sw (string-pixel-width (string-join (cl-loop for x in cands collect (caddr x)) "\n")))
         (fw (default-font-width))
         (width (max (+ pw cw sw) (* fw corfu-min-width)))
         (marginl (if (> ml 0) (propertize " " 'display `(space :width (,ml)))))
         (marginr (if (> mr 0) (propertize " " 'display `(space :width (,mr))))))

    (cl-loop for (cand prefix suffix) being the elements of cands
             using (index i)
             do
             ;; `corfu-current' may affect frame-width too
             (when (= i curr)
               (cl-loop for s in (list cand prefix suffix)
                        do (add-face-text-property 0 (length s) 'corfu-current t s)))

             ;; do not use relative space pixel params as they may lead to wrong
             ;; `string-pixel-width' results
             collect
             (concat
              (if (and (= i curr) marginl)
                  (let ((marginl (substring marginl)))
                    (add-face-text-property 0 (length marginl) 'corfu-current t marginl)
                    marginl)
                marginl)
              prefix
              (propertize " " 'display `(space :align-to (,(+ ml pw))))
              cand
              (propertize " " 'display `(space :align-to (,(+ ml pw cw
                                                              (- width (+ pw cw sw))
                                                              (- sw (string-pixel-width suffix))))))
              suffix
              (if (and (= i curr) marginr)
                  (let ((marginr (substring marginr)))
                    (add-face-text-property 0 (length marginr) 'corfu-current t marginr)
                    marginr)
                marginr)))))

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
    (pcase-let* ((curr (- corfu--index corfu--scroll))
                 (dcands (corfu--affixate
                          (cl-loop repeat corfu-count
                                   for c in (nthcdr corfu--scroll corfu--candidates)
                                   collect (funcall corfu--hilit (substring c)))))
                 (rcands (corfu-pixel-perfect--replace-newlines dcands))
                 (prefix-pixel-width (string-pixel-width
                                      (string-join (cl-loop for c in rcands collect (cadr c)) "\n")))
                 (fw (default-font-width))
                 ;; Disable the left margin if there are prefixes
                 (ml (if (> prefix-pixel-width 0) 0 corfu-left-margin-width))
                 (ml (max 0 (ceiling (* fw ml))))
                 ;; Adjust right margin width according to scroll bar width
                 (bw (max 0 (* fw corfu-bar-width)))
                 (mr (max 0 (* fw corfu-right-margin-width)))
                 (mr (floor (- (max mr bw) (min mr bw))))
                 (offset (+ prefix-pixel-width ml))
                 (lines (corfu-pixel-perfect--format-candidates rcands curr ml mr))
                 (content-width (string-pixel-width (string-join lines "\n"))))
      (apply 'corfu--popup-show pos offset content-width lines curr
             (corfu-pixel-perfect--scroll-bar-range)))))

(cl-defmethod corfu--popup-show :around (pos off content-width lines
                                             &context (corfu-pixel-perfect-mode (eql t))
                                             &optional curr lo bar)
  "Show LINES as popup at POS - PW.
OFF is the number of pixels on graphical displays or columns in the terminal to
move the popup to the left.
CONTENT-WIDTH is the number of pixels on graphical displays or columns in the
terminal required to display the longest line in LINES.
The current candidate CURR is highlighted.
A scroll bar is displayed from LO to LO+BAR."
  (let ((lh (default-line-height)))
    (with-current-buffer (corfu--make-buffer " *corfu*")
      (let* ((ch (default-line-height))
             (cw (default-font-width))
             (mr (max 0 (* cw corfu-right-margin-width)))
             (bw (max 0 (* cw corfu-bar-width)))
             (bw (ceiling (min mr bw)))
             (bw (if lo (if corfu-pixel-perfect-ellipsis cw bw) 0))
             (width (+ bw
                       ;; -4 because of margins and some additional safety
                       (min (* cw (min (- (frame-width) 4) corfu-max-width)) content-width)))
             (sbar (propertize " " 'display
                               (if corfu-pixel-perfect-ellipsis
                                   `((margin right-margin)
                                     ,(propertize " " 'face 'corfu-bar))
                                 '(right-fringe corfu-pixel-perfect-scroll-bar corfu-bar))))
             (pos (posn-x-y pos))
             ;; XXX HACK: Minimum popup height must be at least 1 line of the
             ;; parent frame (gh:minad/corfu#261).
             (height (max lh (* (length lines) ch)))
             (edge (window-inside-pixel-edges))
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
              (if corfu-pixel-perfect-ellipsis
                  (setq-local right-margin-width 1
                              right-fringe-width nil)
                (setq-local right-fringe-width bw
                            right-margin-width nil))
            (setq-local right-margin-width nil
                        right-fringe-width nil))

          ;; Enable ellipsis when the window text area is shorter than
          ;; the content-width
          (setf (alist-get 'no-special-glyphs corfu--frame-parameters)
                (if corfu-pixel-perfect-ellipsis
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
                            (when (eq i curr)
                              (add-face-text-property
                               0 (length line) 'corfu-current 'append line))
                            collect line)
                   "\n"))
          (goto-char (point-min)))

        (with-current-buffer (current-buffer)
          (setq corfu--frame (corfu--make-frame corfu--frame x y width height)))))))

(cl-defmethod corfu--exhibit :around (&context (corfu-pixel-perfect-mode (eql t)) &optional auto)
  (cl-flet ((corfu--popup-show
              (pos off _width lines &optional curr lo bar)
              (corfu--popup-show pos off (string-pixel-width (car lines)) lines curr lo bar)))
    (cl-call-next-method auto)))

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
