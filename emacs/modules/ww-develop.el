;; -*- lexical-binding: t; -*-
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ;; ("M-i" . symbol-overlay-put)
         ;; ("M-I" . symbol-overlay-remove-all))
         ))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; --- eldoc config ------------------------------------------------------------
(global-set-key (kbd "C-c d") #'eldoc-doc-buffer)
;; Better eldoc display
(setq eldoc-echo-area-use-multiline-p t)
;; Optional: keep docs visible longer
(setq eldoc-idle-delay 0.2)
;; --- eldoc end ---------------------------------------------------------------

;; Minimal pairing for programming: only (), [], {}
(defun my/electric-pair-inhibit-quotes (char)
  "Inhibit pairing for ' and \", otherwise use the default predicate."
  (or (memq char '(?\" ?\'))
      (funcall #'electric-pair-default-inhibit char)))

(defun my/electric-pair-prog-setup ()
  "Pair only (), [], {} in programming buffers; never pair quotes."
  (electric-pair-local-mode 1)
  ;; Only structural pairs:
  (setq-local electric-pair-pairs
              '((?\( . ?\))
                (?\[ . ?\])
                (?\{ . ?\})))
  ;; No text pairs (quotes):
  (setq-local electric-pair-text-pairs nil)
  ;; Hard block quotes even if something tries to add them back:
  (setq-local electric-pair-inhibit-predicate #'my/electric-pair-inhibit-quotes))

(add-hook 'prog-mode-hook #'my/electric-pair-prog-setup)

;; project config --------------------------------------------------------------
(use-package project
  :ensure nil
  :bind (:map project-prefix-map
              ("t" . eat-project))
  :custom
  (project-switch-use-entire-map t))

(defun dw/project-compilation-buffer-name-function (name-of-mode)
  (if-let* ((project (project-current nil))
            (name (project-name project)))
      (format "*compilation: %s*" name)
    (format "*%s*" name-of-mode)))

(setq project-compilation-buffer-name-function 'dw/project-compilation-buffer-name-function)

(defun dw/ensure-project-in-tab (project-dir)
  "Ensure we're in a tab named after the project."
  (let* ((project-name (file-name-nondirectory (directory-file-name project-dir)))
         (tab-names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
         (existing-tab (seq-find (lambda (name) (equal name project-name)) tab-names)))
    (unless existing-tab
      (tab-new)
      (tab-rename project-name))
    (unless (equal (alist-get 'name (tab-bar--current-tab)) project-name)
      (tab-bar-select-tab-by-name project-name))))

(defun dw/project-prompter ()
  "Prompt for project and ensure it opens in the correct tab."
  (let ((project-dir (project-prompt-project-dir)))
    (dw/ensure-project-in-tab project-dir)
    project-dir))

;; Use our custom prompter for all project commands
(setq project-prompter #'dw/project-prompter)

(use-package yasnippet
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package eglot
  :ensure nil
  :hook ((js-mode
          js-ts-mode
          typescript-mode
          typescript-ts-mode
          c-mode
          c-ts-mode
          java-mode
          java-ts-mode
          python-mode
          python-ts-mode
          zig-mode
          go-mode
          go-ts-mode
          ) . eglot-ensure)
  :bind
  (("M-RET" . eglot-code-actions))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-send-changes-idle-time 0.25)
  (setq eglot-report-progress nil)

  ;; ---- Server mappings (explicit & reliable) ----

  ;; Python: pyright
  (add-to-list 'eglot-server-programs
	       `((python-mode python-ts-mode)
	         . ,(eglot-alternatives '(("pyright-langserver" "--stdio")))))

  ;; Java: jdtls
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) .
                 ("jdtls"
                  :initializationOptions
                  (:extendedClientCapabilities (:classFileContentsSupport t)))))

  ;; Rust: rust-analyzer
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))

  ;; C/C++: clangd (Homebrew LLVM, Apple Silicon)
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode) . ("/opt/homebrew/opt/llvm/bin/clangd")))

  ;; JS/TS: typescript-language-server
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode)
                 . ("typescript-language-server" "--stdio")))

  ;; Zig: zls
  (add-to-list 'eglot-server-programs
               '((zig-mode zig-ts-mode) . ("zls")))

  ;; Go: gopls
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls"))))


;;; --- Zig --------------------------------------------------------------------
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

;;; --- Racket Mode ------------------------------------------------------------
(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'"
  :hook ((racket-mode . racket-xp-mode)          ;; extra analysis: defs/uses, etc.
         (racket-mode . electric-pair-mode))     ;; helps keep parens balanced
  :config
  ;; If Emacs can't find `racket` automatically, set it explicitly:
  ;; (setq racket-program "/Applications/Racket v8.x/bin/racket")
  )

;;; --- OCaml ------------------------------------------------------------------
;; Configure Flymake for verbose diagnostics
(use-package flymake
  :ensure t
  :pin gnu
  :config
  (setq flymake-diagnostic-format-alist
        '((t . (origin code message)))))

;; Configure Tuareg
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

;; Configure OCaml-eglot
(use-package ocaml-eglot
  :ensure t
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure)
  (ocaml-eglot . (lambda () (add-hook #'before-save-hook #'eglot-format nil t)))
  :config
  (setq ocaml-eglot-syntax-checker 'flymake))

;; Additional modes configuration
(use-package dune
  :ensure t)

(use-package opam-switch-mode
  :ensure t
  :hook
  (tuareg-mode . opam-switch-mode))

(use-package ocp-indent
  :ensure t
  :config
  (add-hook 'ocaml-eglot-hook 'ocp-setup-indent))

;;; --- Python -----------------------------------------------------------------
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;;; --- JS / TS ----------------------------------------------------------------
(use-package js-mode
  :ensure nil
  :mode ("\\.jsx?\\'")
  :config
  (setq-default js-indent-level 2))

(use-package typescript-mode
  :mode ("\\.tsx?\\'")
  :config
  (setq-default typescript-indent-level 2))

;;;--- Rust --------------------------------------------------------------------
;; Use tree-sitter Rust mode when available
(when (fboundp 'rust-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

;; Eglot + rust-analyzer
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . ("rust-analyzer")))
  (setq eglot-autoshutdown t))

(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)

;; Format on save (rustfmt via rust-analyzer)
(defun my/rust-format-on-save ()
  (when (and (derived-mode-p 'rust-ts-mode 'rust-mode)
             (eglot-managed-p))
    (ignore-errors (eglot-format))))
(add-hook 'before-save-hook #'my/rust-format-on-save)

;; Cargo helpers (built-in compile)
(defun my/rust--project-root ()
  (locate-dominating-file default-directory "Cargo.toml"))

(defun my/rust--compile (cmd)
  (let ((default-directory (or (my/rust--project-root) default-directory)))
    (compile cmd)))

(global-set-key (kbd "C-c r c") (lambda () (interactive) (my/rust--compile "cargo check")))
(global-set-key (kbd "C-c r t") (lambda () (interactive) (my/rust--compile "cargo test")))
(global-set-key (kbd "C-c r r") (lambda () (interactive) (my/rust--compile "cargo run")))
(global-set-key (kbd "C-c r l") (lambda () (interactive) (my/rust--compile "cargo clippy")))

;;; --- Java -----------------------------------------------------------------------------
(defun ww/jdt-file-name-handler (operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* (
         (uri (car args))
         (root (ignore-errors (project-root (project-current t))))
         ;; use project's root dir as jdtls cache dir, or ~/.emacs.d/.jdtls-cache
         (cache-dir (expand-file-name ".jdtls-cache" (or root user-emacs-directory)))
         (source-file
          (expand-file-name
           (file-name-concat
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . ww/jdt-file-name-handler))


(provide 'ww-develop)
