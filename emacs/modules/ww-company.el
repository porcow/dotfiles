(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.3)
  (company-show-numbers t)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t))
(provide 'ww-company)
