;; -*- lexical-binding: t; -*-
(defun ww/info-reading-setup ()
  (visual-line-mode 1)
  (setq-local scroll-margin 2
              scroll-conservatively 101
              scroll-step 1)
  (hl-line-mode 1))

(add-hook 'Info-mode-hook #'ww/info-reading-setup)

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))


(provide 'ww-info)
