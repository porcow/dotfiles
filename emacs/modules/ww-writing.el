;; -*- lexical-binding: t; -*-

(use-package denote
  :demand t
  :preface
  (defun ww/denote-markdown ()
    "Create a new Denote note as Markdown."
    (interactive)
    (let ((denote-file-type 'markdown-toml))
      (call-interactively #'denote)))

  (defun ww/encrypt-current-note ()
    "Encrypt current note by rewriting it to FILE.gpg (not just renaming)."
    (interactive)
    (unless buffer-file-name
      (user-error "Current buffer is not visiting a file"))
    (when (string-suffix-p ".gpg" buffer-file-name)
      (user-error "Already encrypted: %s" buffer-file-name))

    ;; Ensure latest plaintext is in the buffer
    (save-buffer)

    (let* ((old buffer-file-name)
           (new (concat old ".gpg"))
           (bak (concat old ".bak")))
      (when (file-exists-p new)
        (user-error "Target already exists: %s" new))

      ;; Keep a backup of the plaintext file until encryption succeeds
      (rename-file old bak t)

      (unwind-protect
          (progn
            ;; Now write encrypted content to NEW
            (set-visited-file-name new t t)
            (set-buffer-modified-p t)   ;; force write even if contents unchanged
            (basic-save-buffer)         ;; triggers epa-file encryption for .gpg
            ;; If we get here, encryption succeeded; remove backup
            (when (file-exists-p bak)
              (delete-file bak))
            (message "Encrypted note: %s" (file-name-nondirectory new)))
        ;; If something went wrong, try to restore original file name
        (when (and (file-exists-p bak) (not (file-exists-p old)))
          (ignore-errors (rename-file bak old t))))))

  (defun ww/decrypt-current-note ()
    "Decrypt current note by rewriting it to FILE without .gpg extension."
    (interactive)
    (unless buffer-file-name
      (user-error "Current buffer is not visiting a file"))
    (unless (string-suffix-p ".gpg" buffer-file-name)
      (user-error "Not a .gpg file: %s" buffer-file-name))

    ;; Buffer already contains decrypted text (Emacs decrypts on open)
    (save-buffer)

    (let* ((old buffer-file-name) ;; ends with .gpg
           (new (string-remove-suffix ".gpg" old))
           (bak (concat old ".bak")))
      (when (file-exists-p new)
        (user-error "Target already exists: %s" new))

      ;; Keep a backup of the encrypted file until plaintext write succeeds
      (rename-file old bak t)

      (unwind-protect
          (progn
            ;; Now write plaintext content to NEW
            (set-visited-file-name new t t)
            (set-buffer-modified-p t)   ;; force write
            (basic-save-buffer)         ;; writes plaintext (no .gpg handler)
            ;; Success: remove encrypted backup
            (when (file-exists-p bak)
              (delete-file bak))
            (message "Decrypted note: %s" (file-name-nondirectory new)))
        ;; Restore on failure
        (when (and (file-exists-p bak) (not (file-exists-p old)))
          (ignore-errors (rename-file bak old t))))))

  :bind (("C-c n l" . denote-link-or-create)
         ("C-c n o" . denote-open-or-create)
         ("C-c n r" . denote-rename-file-using-front-matter)
         ("C-c n m" . ww/denote-markdown)
         ("C-c n e" . ww/encrypt-current-note)
         ("C-c n d" . ww/decrypt-current-note))
  :custom
  (denote-directory (file-truename "~/Documents/Notes"))
  (denote-rename-buffer-format "Denote: %t (%k)")
  ;; (denote-infer-keywords nil)
  (denote-known-keywords
   '("programming" "health" "emacs" "wxw" "kid" "diary" "private"))

  :config
   ;; Rename buffers with the note name
  (denote-rename-buffer-mode 1)

  ;; Buttonize all denote links in text buffers
  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
  (add-hook 'dired-mode-hook #'denote-dired-mode))


(use-package consult-notes
  :ensure nil
  :commands (consult-notes)
  :config
  (consult-notes-denote-mode 1))

(defun dw/setup-markdown-mode ()
  (center-document-mode 1)
  (display-line-numbers-mode 0))

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "marked")
  :config
  (add-hook 'markdown-mode-hook #'dw/setup-markdown-mode)
  (dolist (face '((markdown-header-face-1 . 1.2)
                  (markdown-header-face-2 . 1.1)
                  (markdown-header-face-3 . 1.0)
                  (markdown-header-face-4 . 1.0)
                  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

(defun dw/orgalist-text-mode-hook ()
  (unless (derived-mode-p 'org-mode)
    (orgalist-mode 1)))

(use-package orgalist
  :init
  (add-hook 'text-mode-hook 'dw/orgalist-text-mode-hook))

(defun dw/howm-set-buffer-name ()
  (when (and buffer-file-name
             (howm-subdirectory-p howm-directory buffer-file-name))
    (howm-mode-set-buffer-name)))

(use-package howm
  :ensure nil
  :bind* ("C-c ; ;" . howm-menu)
  :init
  (setq howm-prefix (kbd "C-c ;")
        howm-view-use-grep t
        howm-buffer-name-format "howm: %s"
        howm-buffer-name-limit 100
        howm-buffer-name-total-limit 100
        howm-directory "~/Documents/Notes"
        howm-keyword-file (expand-file-name ".howm-keys" howm-directory)
        howm-history-file (expand-file-name ".howm-history" howm-directory)
        howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org"
        howm-view-title-header "*"
        howm-dtime-format "<%Y-%m-%d %a %H:%M>"
        howm-template "* %title%cursor\n\n%date %file\n\n")
  :config
  (add-hook 'org-mode-hook 'howm-mode)
  (add-hook 'howm-mode-hook 'dw/howm-set-buffer-name)
  (add-hook 'after-save-hook 'dw/howm-set-buffer-name)

  ;; Adapted from this GitHub issue:
  ;; https://github.com/protesilaos/modus-themes/issues/117#issuecomment-2337993946
  (modus-themes-with-colors
    (custom-set-faces
     `(howm-menu-key-face ((,c :inherit help-key-binding)))
     `(howm-mode-keyword-face (( )))
     `(howm-mode-ref-face ((,c :inherit link)))
     `(howm-mode-title-face ((,c :inherit modus-themes-heading-0)))
     `(howm-mode-wiki-face ((,c :inherit link)))
     `(howm-reminder-deadline-face ((,c :foreground ,red-warmer :underline nil)))
     `(howm-reminder-late-deadline-face ((,c :inherit bold :foreground ,date-deadline :underline nil)))
     `(howm-reminder-defer-face ((,c :foreground ,date-scheduled :underline nil)))
     `(howm-reminder-schedule-face ((,c :foreground ,date-scheduled :underline nil)))
     `(howm-reminder-done-face ((,c :foreground ,prose-done :underline nil)))
     `(howm-reminder-todo-face ((,c :foreground ,prose-todo :underline nil)))
     `(howm-reminder-normal-face ((,c :foreground ,date-common :underline nil)))
     `(howm-reminder-today-face ((,c :inherit bold :foreground ,bg-main :background ,yellow-warmer :underline nil)))
     `(howm-reminder-tomorrow-face ((,c :inherit bold :foreground ,date-scheduled :underline nil)))
     `(howm-simulate-todo-mode-line-face ((,c :inherit bold)))
     `(howm-view-empty-face (( )))
     `(howm-view-hilit-face ((,c :inherit match)))
     `(howm-view-name-face ((,c :inherit bold)))
     `(iigrep-counts-face1 ((,c :foreground ,rainbow-1)))
     `(iigrep-counts-face2 ((,c :foreground ,rainbow-2)))
     `(iigrep-counts-face3 ((,c :foreground ,rainbow-3)))
     `(iigrep-counts-face4 ((,c :foreground ,rainbow-4)))
     `(iigrep-counts-face5 ((,c :foreground ,rainbow-5))))))

(provide 'ww-writing)
