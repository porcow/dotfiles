;; -*- lexical-binding: t; -*-
(use-package xterm-color
  :ensure nil  ; Not available on ELPA repos
  :config
  ;; Enable ANSI escape handling in eshell
  (with-eval-after-load 'eshell
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))

    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
    (setenv "TERM" "xterm-256color"))

  ;; Enable ANSI escape handling in M-x compile
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun dw/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'dw/advice-compilation-filter))

(use-package eat
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(use-package vterm
  :ensure t)

(provide 'ww-system)
