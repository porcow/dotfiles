;; -*- lexical-binding: t; -*-

(setq erc-server "irc.libera.chat"
      erc-nick "OxNil"
      erc-user-full-name "OxNil Void"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("Libera.Chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury
      erc-fill-column 80
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      erc-track-enable-keybindings t
      erc-use-sasl t
      erc-sasl-mechanism 'plain)

(defun ww/erc-libera ()
  "Connect to Libera.Chat via TLS without prompting."
  (interactive)
  (erc-tls :server "irc.libera.chat"
           :port 6697
           :nick "OxNil"
           :user "OxNil"
           :full-name "OxNil Void"))

(global-set-key (kbd "C-c i") #'ww/erc-libera)

(add-hook 'erc-mode-hook
          (lambda () (display-line-numbers-mode -1)))


(use-package erc-hl-nicks
  :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

(use-package erc-image
  :ensure t
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))

(use-package emojify
  :ensure t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(use-package jabber
  :custom
  (jabber-chat-buffer-format "Jabber: %n")
  :config
  ;; Unset the global key binding
  (global-set-key (kbd "C-x C-j") #'dired-jump))

(provide 'ww-chat)
