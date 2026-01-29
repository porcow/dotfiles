;; -*- lexical-binding: t; -*-

;; Auto-encrypt/decrypt *.gpg files
(require 'epa-file)
(epa-file-enable)

;; Cache passphrase (optional, less annoying)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Use `pass` as an auth-source
(when (file-exists-p "~/.password-store")
  (auth-source-pass-enable))

(setq auth-sources
      '(password-store "~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))

;; Enable GPG passphrase entry
(use-package pinentry
  :demand t
  :init
  ;; Use the Emacs pinentry, not the external one (recommended)
  ;; On macOS especially, you should force Emacs pinentry
  (setq epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(provide 'ww-auth)
