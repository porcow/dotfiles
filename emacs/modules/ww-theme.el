;; -*- lexical-binding: t; -*-

(defun dw/apply-ayu-dark-style ()
  (interactive)
  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-common-palette-overrides
          `((bg-main "#0F111B")
            (bg-active bg-main)
            (fg-main "#C3CCDF")
            (fg-active fg-main)
            (fringe unspecified)
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
            (fg-mode-line-active "#B3B1AD")
            (bg-mode-line-active "#171B27")
            (fg-mode-line-inactive "#65737E")
            (bg-mode-line-inactive "#1C1F29")
            (bg-tab-bar "#1C1F29")
            (bg-tab-current bg-main)
            (bg-tab-other "#171B27")
            (fg-prompt "#F6C177")
            (bg-prompt unspecified)
            (bg-hover-secondary "#65737E")
            (bg-completion "#2f447f")
            (fg-completion "#ffffff")
            (bg-region "#2B2E36")
            (fg-region "#ffffff")
            (fg-heading-0 "#81A1C1")
            (fg-heading-1 "#81A1C1")
            (fg-heading-2 "#F6C177")
            (fg-heading-3 "#FFB974")
            (fg-heading-4 "#C792EA")
            (fg-prose-verbatim "#A3BE8C")
            (bg-prose-block-contents "#171B27")
            (fg-prose-block-delimiter "#65737E")
            (bg-prose-block-delimiter "#171B27")
            (accent-1 "#7FDBCA")
            (keyword "#F6C177")
            (builtin "#81A1C1")
            (comment "#65737E")
            (string "#A3BE8C")
            (fnname "#7FDBCA")
            (type "#C792EA")
            (variable "#FFB974")
            (docstring "#8996A2")
            (constant "#F07178"))))

(defun dw/apply-palenight-style ()
  (interactive)
  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-common-palette-overrides
          `((bg-main "#292D3E")
            (bg-active bg-main)
            (fg-main "#EEFFFF")
            (fg-active fg-main)
            (fringe unspecified)
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
            (fg-mode-line-active "#A6Accd")
            (bg-mode-line-active "#232635")
            (fg-mode-line-inactive "#676E95")
            (bg-mode-line-inactive "#282c3d")
            (bg-tab-bar "#242837")
            (bg-tab-current bg-main)
            (bg-tab-other bg-active)
            (fg-prompt "#c792ea")
            (bg-prompt unspecified)
            (bg-hover-secondary "#676E95")
            (bg-completion "#2f447f")
            (fg-completion white)
            (bg-region "#3C435E")
            (fg-region white)
            (fg-heading-0 "#82aaff")
            (fg-heading-1 "#82aaff")
            (fg-heading-2 "#c792ea")
            (fg-heading-3 "#bb80b3")
            (fg-heading-4 "#a1bfff")
            (fg-prose-verbatim "#c3e88d")
            (bg-prose-block-contents "#232635")
            (fg-prose-block-delimiter "#676E95")
            (bg-prose-block-delimiter bg-prose-block-contents)
            (accent-1 "#79a8ff")
            (keyword "#89DDFF")
            (builtin "#82aaff")
            (comment "#676E95")
            (string "#c3e88d")
            (fnname "#82aaff")
            (type "#c792ea")
            (variable "#ffcb6b")
            (docstring "#8d92af")
            (constant "#f78c6c"))))

(defun dw/apply-pico8-style ()
  "Apply the PICO-8 VS Code palette to Modus themes via `modus-themes-common-palette-overrides`."
  (interactive)
  (setopt modus-themes-mode-line '(accented borderless)
          modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-fringes 'subtle
          modus-themes-tabs-accented t
          modus-themes-org-blocks 'tinted-background
          modus-themes-scale-headings t
          modus-themes-region '(bg-only)
          modus-themes-common-palette-overrides
          `((bg-main "#1D2B53")
            (bg-active bg-main)
            (fg-main "#c2c3c7")
            (fg-active fg-main)
            (cursor "#ff004d")
            (fringe unspecified)
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
            (bg-mode-line-active "#83769c")
            (fg-mode-line-active "#fff1e8")
            (bg-mode-line-inactive "#83769c")
            (fg-mode-line-inactive "#fff1e8")
            (bg-tab-bar "#fff1e8")
            (bg-tab-current "#ff77a8")
            (bg-tab-other "#fff1e8")
            ;; (bg-hl-line "#2463B1")
            (bg-line-number-inactive "#2463B1")
            (bg-line-number-active "#2463B1")
            (fg-line-number-inactive "#1D2B53")
            (fg-line-number-active fg-main)
            (fg-prompt "#ff77a8")
            (bg-prompt unspecified)
            (bg-hover-secondary "#5f574f")
            (bg-completion "#000000")
            (fg-completion "#fff1e8")
            (bg-region "#ffec27")
            (fg-region "#000000")
            (fg-heading-0 "#00e436")
            (fg-heading-1 "#29adff")
            (fg-heading-2 "#ffec27")
            (fg-heading-3 "#ffa300")
            (fg-heading-4 "#ff77a8")
            (fg-prose-verbatim "#29adff")
            (bg-prose-block-contents "#000000")
            (fg-prose-block-delimiter "#83769c")
            (bg-prose-block-delimiter bg-prose-block-contents)
            (accent-1 "#ff004d")
            (keyword "#ff77a8")
            (builtin "#00e436")
            (comment "#83769c")
            (string "#29adff")
            (fnname "#c2c3c7")
            (type "#fff1e8")
            (variable "#ffccaa")
            ;; (docstring "#4bb1b1")
            (docstring "#008751")
            (constant "#29adff")
            (preprocessor "#ff77a8"))))

(defun ww/apply-pico8-style-faces ()
  "Apply PICO-8 specific face overrides after the theme loads."
  (modus-themes-with-colors
    (set-face-attribute 'tab-bar nil
                        :background bg-tab-bar
                        :foreground "#83769c")
    (set-face-attribute 'tab-bar-tab nil
                        :background bg-tab-current
                        :foreground "#fff1e8"
                        :weight 'bold
                        :box nil)
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :background bg-tab-other
                        :foreground "#83769c"
                        :box nil)
    (set-face-attribute 'display-time-date-and-time nil
                        :foreground "#83769c"
                        :inherit nil)))


(defun dw/apply-fallout2-style ()
  (interactive)
  (setopt modus-themes-mode-line '(accented borderless)
          modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-fringes 'subtle
          modus-themes-tabs-accented t
          modus-themes-org-blocks 'tinted-background
          modus-themes-scale-headings t
          modus-themes-region '(bg-only)
          modus-themes-common-palette-overrides
          `((bg-main "#282828")
            (bg-dim "#0c0c0c")
            (bg-active bg-main)
            (fg-main "#7df54a")
            (fg-alt "#BDB9B3")
            (fg-active fg-main)
            (cursor "#7df54a")
            (fringe unspecified)
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
            (fg-mode-line-active "#ececec")
            (bg-mode-line-active "#6f6652")
            (fg-mode-line-inactive "#BDB9B3")
            (bg-mode-line-inactive "#6f6652")
            (bg-tab-bar "#6f6652")
            (bg-tab-current "#383838")
            (bg-tab-other "#6f6652")
            (bg-hl-line "#383838")
            (bg-line-number-inactive "#202020")
            (bg-line-number-active "#202020")
            (fg-prompt "#b49d40")
            (bg-prompt unspecified)
            (bg-hover-secondary "#171C15")
            (bg-completion "#1c1c1c")
            (fg-completion "#b49d40")
            (bg-region "#383838")
            (fg-region "#f7de89")
            (fg-heading-0 "#b49d40")
            (fg-heading-1 "#b49d40")
            (fg-heading-2 "#eea059")
            (fg-heading-3 "#f7de89")
            (fg-heading-4 "#f7de89")
            (fg-prose-verbatim "#BDB9B3")
            (bg-prose-block-contents "#0c0c0c")
            (fg-prose-block-delimiter "#565850")
            (bg-prose-block-delimiter "#1c1c1c")
            (fg-prose-code "#f7de89")
            (fg-link "#eea059")
            (underline-link "#eea059")
            (accent-1 "#A0824C")
            (keyword "#A0824C")
            (builtin "#f7de89")
            (comment "#565850")
            (string "#BDB9B3")
            (fnname "#7df54a")
            (type "#ececec")
            (variable "#49a12e")
            (docstring "#BDB9B3")
            (constant "#b49d40")
            (preprocessor "#6f6652")
            (rx-backslash "#eea059"))))

(defgroup dw/theme nil
  "Theme style configuration."
  :group 'faces)

(defcustom ww/theme-style 'fallout2
  "Theme style to apply."
  :type '(choice
          (const :tag "PICO-8" pico8)
          (const :tag "Ayu Dark" ayu-dark)
          (const :tag "Palenight" palenight)
          (const :tag "fallout2" fallout2))
  :group 'dw/theme)

(defun ww/apply-theme-style ()
  "Apply the configured theme style."
  (pcase ww/theme-style
    ('pico8 (dw/apply-pico8-style))
    ('ayu-dark (dw/apply-ayu-dark-style))
    ('palenight (dw/apply-palenight-style))
    ('fallout2 (dw/apply-fallout2-style))
    (_ (error "Unknown theme style: %S" ww/theme-style))))

(defun ww/apply-theme-style-extras ()
  "Apply extra face adjustments for the configured theme style."
  (pcase ww/theme-style
    ('pico8
     (ww/apply-pico8-style-faces))
    (_ nil)))

(provide 'ww-theme)
