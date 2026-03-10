;;; --- Lua major mode ---------------------------------------------------------
(use-package lua-mode
  :mode "\\.lua\\'")

(require 'cl-lib)

;; pico-8 setup
;; Highlight operators PICO-style
(defface pico8-operator-face
  '((t (:foreground "#fff1e8")))
  "Face for PICO-8 operators.")

(defconst pico8--operator-regex
  (rx
   (group
    (or "!="
        "=="
        "<="
        ">="
        "%"
        "=" "+" "-" "*" "/" "<" ">")))
  "Operator regex for PICO-8.")

(defun pico8--highlight-operators ()
  (font-lock-add-keywords
   nil
   `((,pico8--operator-regex 1 'pico8-operator-face))
   'append))

(defun ww/pico8-setup-font ()
  ;; PICO-8 font + byte-safe round-trip P8SCII editing.
  (face-remap-add-relative
   'default
   :family "PICO-8-0.2.5"
   :weight 'normal
   :height 120)
  (setq-local line-spacing 2))

(use-package pico8-mode
  :vc (:url "https://github.com/porcow/pico8-mode.git"
       :rev :newest)
  :mode "\\.p8\\'"
  :custom
  (pico8-executable-path "/Applications/PICO-8.app/Contents/MacOS/pico8")
  (pico8-documentation-file "/Users/porco/Downloads/games/DevTool/pico-8/pico-8_manual.txt")
  :hook
  (;; (pico8-mode . ww/pico8-setup-font)
  (pico8-mode . pico8--highlight-operators)))

(use-package eglot
  :ensure nil
  :hook ((lua-mode
          pico8-mode) . eglot-ensure)

  :config
  ;; ---- Server mappings (explicit & reliable) ----

  ;; Lua language server
  (add-to-list 'eglot-server-programs
               '(lua-mode . ("lua-language-server")))

  ;; Pico-8 language server
  (add-to-list 'eglot-server-programs
               '(pico8-mode . ("node" "/Users/porco/Downloads/games/DevTool/pico-8/pico8-ls/server/out/server.js" "--stdio"))))

(provide 'ww-pico8)
