;;; -*- lexical-binding: t -*-

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
