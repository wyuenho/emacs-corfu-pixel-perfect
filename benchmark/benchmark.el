;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'corfu)
(require 'corfu-pixel-perfect)
(require 'kind-icon)

(setq corfu-margin-formatters '(kind-icon-margin-formatter))

(corfu-mode)
;; (corfu-pixel-perfect-mode)

(let* ((completion-extra-properties '(
                                      :company-kind (lambda (_) 'method)
                                      :annotation-function
                                      (lambda (_)
                                        ;; 60 chars
                                        "suffixsuffixsuffixsuffixsuffixsuffixsuffixsuffixsuffixsuffix")))
       (corfu--metadata completion-extra-properties)
       (completion-in-region--data `(nil nil nil nil ,completion-extra-properties))
       (corfu-count 100) ;; show a 100 rows in the popup
       (corfu--index 0)
       (corfu--scroll 0)
       (corfu--total 100) ;; 100 candidates
       (corfu--candidates
        (cl-loop repeat corfu-count
                 collect
                 ;; 40 chars
                 "0123456789😀😃😄😀😃😄😀😃😄😀0123456789😀😃😄😀😃😄😀😃😄😀0123456789😀😃😄😀😃😄😀😃😄😀0123456789😀😃😄😀😃😄😀😃😄😀")))
  (benchmark-run 720 (corfu--candidates-popup (posn-at-point))))

;; (let ((results '((corfu-pixel-perfect 
;;                   ((8.221643 22 1.8849680000000006)
;;                    (8.270672 22 1.883226999999998)
;;                    (8.470725999999999 23 1.9575109999999967)
;;                    (8.516521 23 1.9783120000000025)
;;                    (8.524778 23 2.0041219999999953)
;;                    (8.528776 23 2.009339000000004)
;;                    (8.552294 23 2.028520999999998)
;;                    (8.559337000000001 23 2.034281)
;;                    (8.573954 23 2.0515390000000053)
;;                    (8.579111 23 2.055924999999995)))

;;                  (corfu
;;                   ((7.876859 31 2.768153999999999)
;;                    (7.895206 31 2.7722999999999995)
;;                    (7.964758 31 2.839691000000002)
;;                    (8.005801 32 2.904587000000003)
;;                    (8.084365 32 2.8155789999999987)
;;                    (8.091056 31 2.972280999999999)
;;                    (8.157283999999999 32 2.874342)
;;                    (8.199778 32 2.912965)
;;                    (8.199891000000001 32 2.9194259999999996)
;;                    (8.232091 32 2.9391169999999995))))))

;;   (cl-loop for (mode runs) in results
;;            collect
;;            (let* ((elapsed-time (apply '+ (mapcar 'car runs)))
;;                   (mean-time (/ elapsed-time (length runs)))
;;                   (stddev (/ (cl-loop for (et _ __) in runs sum (* (- et mean-time) (- et mean-time)))
;;                              (length runs)))
;;                   (gc-time (apply '+ (mapcar 'caddr runs)))
;;                   (fps (/ 1 (/ (/ elapsed-time (length runs)) 720))))

;;              (format "mode: %s, elapsed time: %f, stddev: %f, gc time: %f, fps: %f" mode elapsed-time stddev gc-time fps))))
;; (
;;  "mode: corfu-pixel-perfect, elapsed time: 84.797812, stddev: 0.014654, gc time: 19.887745, fps: 84.907851"
;;  "mode: corfu, elapsed time: 80.707089, stddev: 0.015130, gc time: 28.718442, fps: 89.211494"
;; )
