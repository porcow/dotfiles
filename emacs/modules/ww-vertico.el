(use-package savehist
  :init (savehist-mode 1))

(use-package vertico
  :init (vertico-mode 1)
  :custom
  (vertico-count 15)
  (vertico-resize t)
  (vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind (("C-x b" . consult-buffer)))


(provide 'ww-vertico)
;;; ww-vertico.el ends here
