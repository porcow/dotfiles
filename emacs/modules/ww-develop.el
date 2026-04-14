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

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; --- eldoc config ------------------------------------------------------------
(global-set-key (kbd "C-c d") #'eldoc-doc-buffer)
;; Keep terminal redisplay stable; allow richer echo-area docs in GUI.
(setq eldoc-echo-area-use-multiline-p (display-graphic-p))

;; Optional: keep docs visible longer
(setq eldoc-idle-delay 0.2)

(with-eval-after-load 'eglot
  (defun my/eglot-eldoc-setup ()
    (setq-local eldoc-documentation-functions
                (if (display-graphic-p)
                    '(eglot-signature-eldoc-function
                      eglot-hover-eldoc-function)
                  '(eglot-signature-eldoc-function)))
    (setq-local eldoc-echo-area-use-multiline-p (display-graphic-p)))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-eldoc-setup))

;; ---HTML escapes -------------------------------
(defvar rb--eldoc-html-patterns
  '(("&nbsp;" " ")
    ("&lt;" "<")
    ("&gt;" ">")
    ("&amp;" "&")
    ("&quot;" "\"")
    ("&apos;" "'"))
  "List of (PATTERN . REPLACEMENT) to replace in eldoc output.")

(defun rb--string-replace-all (patterns in-string)
  "Replace all cars from PATTERNS in IN-STRING with their pair."
  (mapc (lambda (pattern-pair)
          (setq in-string
                (string-replace (car pattern-pair) (cadr pattern-pair) in-string)))
        patterns)
  in-string)

(defun rb--eldoc-preprocess (orig-fun &rest args)
  "Preprocess the docs to be displayed by eldoc to replace HTML escapes."
  (let ((doc (car args)))
    ;; The first argument is a list of (STRING :KEY VALUE ...) entries
    ;; we replace the text in each such string
    ;; see docstring of `eldoc-display-functions'
    (when (listp doc)
      (setq doc (mapcar
                 (lambda (doc) (cons
                                (rb--string-replace-all rb--eldoc-html-patterns (car doc))
                                (cdr doc)))
                 doc
                 ))
      )
    (apply orig-fun (cons doc (cdr args)))))

(advice-add 'eldoc-display-in-buffer :around #'rb--eldoc-preprocess)
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

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

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
          c-or-c++-mode
          c-ts-mode
          c++-mode
          c-or-c++-ts-mode
          c++-ts-mode
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
  ;; Use Company via CAPF
  ;; (company-capf is enabled automatically when company-mode is on)
  (setq company-backends '(company-capf))

  ;; ---- Server mappings (explicit & reliable) ----

  ;; Python: pyright
  (add-to-list 'eglot-server-programs
	       `((python-mode python-ts-mode)
	         . ,(eglot-alternatives '(("basedpyright-langserver" "--stdio")))))

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
               '((c-mode c-or-c++-mode c-ts-mode c++-mode c-or-c++-ts-mode c++-ts-mode)
                 . ("/opt/homebrew/opt/llvm/bin/clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion=iwyu")))

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
  :mode "\\.zig\\'"
  :config
  ;; --- project root ----------------------------------------------------------
  (defun my/zig--project-root ()
    (locate-dominating-file default-directory "build.zig"))

  ;; --- project commands ------------------------------------------------------
  (defun my/zig--compile (cmd)
    "Run a Zig compile command from the nearest Zig project root."
    (interactive)
    (let ((default-directory (or (my/zig--project-root) default-directory)))
      (compile cmd)))

  (global-set-key (kbd "C-c z b") (lambda () (interactive) (my/zig--compile "zig build"))))

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

;;; --- JS / TS / JSON----------------------------------------------------------------
(use-package js-mode
  :ensure nil
  :mode ("\\.jsx?\\'")
  :config
  (setq-default js-indent-level 2))

(use-package typescript-mode
  :mode ("\\.tsx?\\'")
  :config
  (setq-default typescript-indent-level 2))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;;;--- Go ----------------------------------------------------------------------

;; --- project root ------------------------------------------------------------
(defun my/go--project-root ()
  (locate-dominating-file default-directory "go.mod"))

;; --- project commands --------------------------------------------------------
(defun my/go--compile (cmd)
  "Run a Go compile command from the nearest Go project root."
  (interactive)
  (let ((default-directory (or (my/go--project-root) default-directory)))
    (compile cmd)))

(global-set-key (kbd "C-c g b") (lambda () (interactive) (my/go--compile "go build ./...")))
(global-set-key (kbd "C-c g t") (lambda () (interactive) (my/go--compile "go test ./...")))
(global-set-key (kbd "C-c g r") (lambda () (interactive) (my/go--compile "go run .")))

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

;; --- formatting --------------------------------------------------------------
(defun my/rust-format-on-save ()
  (when (and (derived-mode-p 'rust-ts-mode 'rust-mode)
             (eglot-managed-p))
    (ignore-errors (eglot-format))))
(add-hook 'before-save-hook #'my/rust-format-on-save)

;; --- project root ------------------------------------------------------------
(defun my/rust--project-root ()
  (locate-dominating-file default-directory "Cargo.toml"))

;; --- project commands --------------------------------------------------------
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

;;; C/C++ --------------------------------------------------------------------------------

;; --- project root ------------------------------------------------------------
(defun my/cmake-project-root ()
  "Return the nearest CMake project root."
  (locate-dominating-file default-directory "CMakeLists.txt"))

;; --- editing setup -----------------------------------------------------------
(defun my/cpp-mode-common-setup ()
  "Common setup for C and C++ buffers."
  (setq-local c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil)
  (c-set-style "stroustrup"))

(add-hook 'c-mode-hook #'my/cpp-mode-common-setup)
(add-hook 'c++-mode-hook #'my/cpp-mode-common-setup)

(with-eval-after-load 'treesit
  (when (fboundp 'c-or-c++-ts-mode)
    (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))
  (when (fboundp 'c++-ts-mode)
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))
  (when (fboundp 'c-ts-mode)
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))))

(defun my/c-ts-mode-common-setup ()
  "Common setup for tree-sitter C and C++ buffers."
  (setq-local c-ts-mode-indent-offset 2
              tab-width 2
              indent-tabs-mode nil))

(add-hook 'c-ts-mode-hook #'my/c-ts-mode-common-setup)
(add-hook 'c++-ts-mode-hook #'my/c-ts-mode-common-setup)

(add-to-list 'auto-mode-alist '("\\.h\\'"   . c-or-c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

;; --- formatting --------------------------------------------------------------
(defun my/c-family-eglot-format-buffer-on-save ()
  "Enable Eglot format-on-save in the current buffer."
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

(defun my/c-family-format-setup ()
  "Set up formatting for C/C++ buffers managed by Eglot."
  (when (bound-and-true-p eglot-managed-mode)
    (my/c-family-eglot-format-buffer-on-save)))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
              (my/c-family-format-setup))))

;; --- project commands --------------------------------------------------------
(defun my/project-compile ()
  "Run `compile' from the current project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'compile)))

(defun my/project-cmake-configure-debug ()
  "Configure the current project with a Debug CMake build."
  (interactive)
  (let ((default-directory (or (my/cmake-project-root) default-directory)))
    (compile "cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON && ln -sf build/compile_commands.json compile_commands.json")))

(defun my/project-cmake-build ()
  "Build the current project with CMake."
  (interactive)
  (let ((default-directory (or (my/cmake-project-root) default-directory)))
    (compile "cmake --build build")))

(defun my/project-cmake-configure-release ()
  "Configure the current project with a Release CMake build."
  (interactive)
  (let ((default-directory (or (my/cmake-project-root) default-directory)))
    (compile "cmake -S . -B build-release -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON && ln -sf build-release/compile_commands.json compile_commands.json")))

(defun my/project-cmake-build-release ()
  "Build the current project with the Release CMake build directory."
  (interactive)
  (let ((default-directory (or (my/cmake-project-root) default-directory)))
    (compile "cmake --build build-release")))

(defun my/project-ctest ()
  "Run project tests with CTest."
  (interactive)
  (let ((default-directory (or (my/cmake-project-root) default-directory)))
    (compile "ctest --test-dir build --output-on-failure")))

(with-eval-after-load 'project
  (keymap-set project-prefix-map "c" #'my/project-compile)
  (keymap-set project-prefix-map "m" #'my/project-cmake-configure-debug)
  (keymap-set project-prefix-map "M" #'my/project-cmake-configure-release)
  (keymap-set project-prefix-map "b" #'my/project-cmake-build)
  (keymap-set project-prefix-map "B" #'my/project-cmake-build-release)
  (keymap-set project-prefix-map "T" #'my/project-ctest))

(with-eval-after-load 'eglot
  (keymap-set eglot-mode-map "C-c f" #'eglot-format-buffer))


(provide 'ww-develop)
