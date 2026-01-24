;; (use-package company
;;   :hook (after-init . global-company-mode)
;;   :custom
;;   (company-idle-delay 0.2)
;;   (company-show-numbers t)
;;   (company-tooltip-limit 10)
;;   (company-minimum-prefix-length 2)
;;   (company-tooltip-align-annotations t)
;;   ;; invert the navigation direction if the the completion popup-isearch-match
;; ;; is displayed on top (happens near the bottom of windows)
;;   (company-tooltip-flip-when-above t))

;; Completion UI：Corfu（based on CAPF）
(use-package corfu
  :ensure t
  :pin "gnu"
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt))
(provide 'ww-completion)
