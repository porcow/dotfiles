;;; --- Lua major mode ---------------------------------------------------------
(use-package lua-mode
  :mode "\\.lua\\'")

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

;;; ------------------------------------------------------------
;;; PICO-8 / P8SCII: avoid Apple Color Emoji in pico8-mode
;;; - Convert VS16 (FE0F) -> VS15 (FE0E) (fixes ⬅️➡️⬆️⬇️🅾️ etc.)
;;; - Force TEXT presentation for some single-codepoint emoji (e.g. ❎)
;;; - Optionally remap emoji-only codepoints (🐱 😐) to PICO-8-friendly glyphs
;;; ------------------------------------------------------------

(defconst ww/pico8--vs16 "\uFE0F") ;; emoji presentation selector
(defconst ww/pico8--vs15 "\uFE0E") ;; text presentation selector

;; 1) FE0F -> FE0E (your working fix for arrow/🅾️ sequences)
(defun ww/pico8--normalize-variation-selectors (&optional beg end)
  (when (derived-mode-p 'pico8-mode)
    (save-excursion
      (save-restriction
        (when (and beg end)
          (setq beg (max (point-min) (- beg 32)))
          (setq end (min (point-max) (+ end 32)))
          (narrow-to-region beg end))
        (goto-char (point-min))
        (while (search-forward ww/pico8--vs16 nil t)
          (replace-match ww/pico8--vs15 t t))))))

(defun ww/pico8--after-change-normalize-vs (beg end _len)
  (ww/pico8--normalize-variation-selectors beg end))

;; 2) Force “text presentation” for specific single-codepoint emoji that
;; macOS treats as emoji (e.g. ❎) by appending FE0E if not already present.
(defconst ww/pico8--force-text-presentation-chars
  ;; Add more if needed (these work well as text when fonts support them):
  ;; ❎ U+274E, ★ U+2605, ♥ U+2665, etc.
  '(#x274E)  ; ❎
  "Codepoints that should be forced into text presentation using VS15 (FE0E).")

(defun ww/pico8--force-text-presentation (&optional beg end)
  (when (derived-mode-p 'pico8-mode)
    (save-excursion
      (save-restriction
        (when (and beg end)
          (setq beg (max (point-min) (- beg 64)))
          (setq end (min (point-max) (+ end 64)))
          (narrow-to-region beg end))
        (goto-char (point-min))
        (dolist (cp ww/pico8--force-text-presentation-chars)
          (goto-char (point-min))
          (let ((ch (char-to-string cp)))
            ;; Replace "CH" not followed by FE0E/FE0F -> "CH" + FE0E
            (while (search-forward ch nil t)
              (let ((next (char-after (point))))
                (unless (or (eq next #xFE0E) (eq next #xFE0F))
                  (insert ww/pico8--vs15))))))))))

(defun ww/pico8--after-change-force-text (beg end _len)
  (ww/pico8--force-text-presentation beg end))

;; 3) Optional: remap emoji-only codepoints to PICO-8-friendly glyphs.
;; PICO-8 doesn’t truly use Unicode 🐱 😐; if you paste them, they’ll
;; typically become emoji anyway. Replace them with a safe ASCII/P8 glyph.
(defcustom ww/pico8-emoji-replacements
  '((#x1F431 . "🐱")  ;; If your PICO-8 font actually has it, keep it.
    (#x1F610 . "😐")) ;; Same: keep if your font covers it.
  "Alist of (codepoint . replacement-string) applied in pico8-mode.
Set to map emoji to PICO-8 equivalents if your font doesn't contain them."
  :type '(alist :key-type integer :value-type string))

(defun ww/pico8--replace-emoji-only (&optional beg end)
  (when (derived-mode-p 'pico8-mode)
    (save-excursion
      (save-restriction
        (when (and beg end)
          (setq beg (max (point-min) (- beg 64)))
          (setq end (min (point-max) (+ end 64)))
          (narrow-to-region beg end))
        (dolist (pair ww/pico8-emoji-replacements)
          (goto-char (point-min))
          (let ((from (char-to-string (car pair)))
                (to   (cdr pair)))
            (while (search-forward from nil t)
              (replace-match to t t))))))))

(defun ww/pico8--after-change-replace-emoji (beg end _len)
  (ww/pico8--replace-emoji-only beg end))

;; 4) Font preference for P8SCII codepoints (helps when glyph exists in PICO-8 font)
(defcustom ww/pico8-font-family "PICO-8-0.2.5"
  "Font family name for PICO-8 glyphs."
  :type 'string)

(defconst ww/p8scii-unicode-chars
  '(#x1F431 #x1F610 #x274E #x1F17E #x2B05 #x2B06 #x2B07 #x27A1) ;; include key ones
  "A minimal list; extend with your full P8SCII list if you want.")

(defun ww/pico8--prefer-pico8-font ()
  (let ((pico (font-spec :family ww/pico8-font-family)))
    (dolist (cp ww/p8scii-unicode-chars)
      (set-fontset-font t (cons cp cp) pico nil 'prepend))))

;; Fix P8SCII "웃" (U+ C6C3): CoreText classifies it as Hangul, so it picks a
;; Korean font. Force it to use your PICO-8 font via fontset mapping.
(defun ww/pico8-fix-hangul-us ()
  "Force U+ C6C3 (웃) to render with the PICO-8 font."
  (interactive)
  (let ((pico (font-spec :family ww/pico8-font-family)))
    (set-fontset-font t (cons #xC6C3 #xC6C3) pico nil 'prepend)))

;; ------------------------------------------------------------
;; Force Japanese punctuation to render with PICO-8-0.2.5
;; Characters: 「 」 、 。  (U+300C U+300D U+3001 U+3002)
;; ------------------------------------------------------------
(defconst ww/pico8--jp-punct
  '(#x300C #x300D #x3001 #x3002)
  "JP punctuation used by P8SCII that we want to render with the PICO-8 font.")

(defun ww/pico8-fix-jp-punct ()
  "Force JP punctuation to render with the PICO-8 font."
  (interactive)
  (let ((pico (font-spec :family ww/pico8-font-family)))
    (dolist (cp ww/pico8--jp-punct)
      (set-fontset-font t (cons cp cp) pico nil 'prepend))))

(defun my/pico8-setup-font ()
    ;; PICO-8 font
    (face-remap-add-relative
     'default
     ;; :family "ComicShannsMono Nerd Font Mono"
     :family "PICO-8-0.2.5"
     :weight 'normal
     :height 120)
    (setq-local line-spacing 2)
    ;; Prefer PICO-8 font where possible
    (ww/pico8--prefer-pico8-font)

    ;; Normalize once on open
    (ww/pico8--normalize-variation-selectors)
    (ww/pico8--force-text-presentation)
    (ww/pico8--replace-emoji-only)
    (ww/pico8-fix-hangul-us)
    (ww/pico8-fix-jp-punct)

    ;; Keep normalized during edits/paste
    (add-hook 'after-change-functions #'ww/pico8--after-change-normalize-vs nil t)
    (add-hook 'after-change-functions #'ww/pico8--after-change-force-text nil t)
    (add-hook 'after-change-functions #'ww/pico8--after-change-replace-emoji nil t)
    )

(use-package pico8-mode
  :vc (:url "https://github.com/porcow/pico8-mode.git"
       :rev :newest)
  :mode "\\.p8\\'"
  :custom
  (pico8-executable-path "/Applications/PICO-8.app/Contents/MacOS/pico8")
  (pico8-documentation-file "/Users/porco/Downloads/games/DevTool/pico-8/pico-8_manual.txt")
  :hook
  (pico8-mode . my/pico8-setup-font)
  (pico8-mode . pico8--highlight-operators))

(use-package eglot
  :ensure nil
  :hook ((lua-mode
          pico8-mode) . eglot-ensure)
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

  ;; Lua language server
  (add-to-list 'eglot-server-programs
               '(lua-mode . ("lua-language-server")))

  ;; Pico-8 language server
  (add-to-list 'eglot-server-programs
               '(pico8-mode . ("node" "/Users/porco/Downloads/games/DevTool/pico-8/pico8-ls/server/out/server.js" "--stdio"))))

(provide 'ww-pico8)
