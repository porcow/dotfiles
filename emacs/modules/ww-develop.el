;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(global-set-key (kbd "C-c d") #'eldoc-doc-buffer)
;; Better eldoc display
(setq eldoc-echo-area-use-multiline-p t)
;; Optional: keep docs visible longer
(setq eldoc-idle-delay 0.2)

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

;; Jump to the source of 3rd party libraries
(use-package flycheck
  :ensure t
  :hook ((java-mode java-ts-mode) . flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot))

(use-package yasnippet
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;; (defun my-java-mode-hook ()
;;   (auto-fill-mode 1)
;;   (flycheck-mode 1)
;;   (flymake-mode -1)
;;   (subword-mode 1)
;;   (yas-minor-mode 1)

;;   ;; Fix indentation for anonymous classes
;;   (c-set-offset 'substatement-open 0)
;;   (if (assoc 'inexpr-class c-offsets-alist)
;;       (c-set-offset 'inexpr-class 0))

;;   ;; Indent arguments on the next line as indented body.
;;   (c-set-offset 'arglist-intro '++))
;; (add-hook 'java-mode-hook 'my-java-mode-hook)
;; (add-hook 'java-ts-mode-hook 'my-java-mode-hook)

(defun ww/eglot-java-flycheck ()
    (when (derived-mode-p 'java-mode 'java-ts-mode)
      (flymake-mode -1)
      (flycheck-mode 1)
      (auto-fill-mode)
      (subword-mode 1)
      (yas-minor-mode 1)

      ;; Fix indentation for anonymous classes
      (c-set-offset 'substatement-open 0)
      (if (assoc 'inexpr-class c-offsets-alist)
          (c-set-offset 'inexpr-class 0))

      ;; Indent arguments on the next line as indented body.
      (c-set-offset 'arglist-intro '++)
      (when (fboundp 'flycheck-eglot-mode)
        (flycheck-eglot-mode 1))))

(use-package eglot  :ensure nil
  :hook ((js-mode
          typescript-mode
          java-mode
          java-ts-mode
          python-mode
          python-ts-mode
          zig-mode) . eglot-ensure)
  :bind
  (("M-RET" . eglot-code-actions))
  :config
  (setq eglot-report-progress nil)
  (add-to-list 'eglot-server-programs
	       `((python-mode python-ts-mode)
	         . ,(eglot-alternatives '(("pyright-langserver" "--stdio")))))
  (add-hook 'eglot-managed-mode-hook #'ww/eglot-java-flycheck))

;; Zig
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

;; OCaml
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

;; Python
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; JS
(use-package js-mode
  :ensure nil
  :mode ("\\.jsx?\\'")
  :config
  (setq-default js-indent-level 2))

;; TS
(use-package typescript-mode
  :mode ("\\.tsx?\\'")
  :config
  (setq-default typescript-indent-level 2))

;; Java
(defun ww/jdt-file-name-handler (operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir "/tmp/.eglot")
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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode) .
                 ("jdtls"
                  :initializationOptions
                  (:extendedClientCapabilities (:classFileContentsSupport t))))))

(provide 'ww-develop)
