;;; install claude-code-ide
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup) ; Optionally enable Emacs MCP tools

  ;; Open Claude on the right with custom width
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 100)

  ;; Don't automatically focus the Claude window
  ;; (setq claude-code-ide-focus-on-open nil)

  ;; Use eat instead of vterm
  (setq claude-code-ide-terminal-backend 'eat)

  ;; vterm Rendering Optimization
  ;; Enable/disable vterm anti-flicker optimization (enabled by default)
  (setq claude-code-ide-vterm-anti-flicker t)
  ;; Adjust the render delay for batching updates (default is 0.005 seconds)
  (setq claude-code-ide-vterm-render-delay 0.01)  ; Increase for smoother but less responsive
  )

(provide 'ww-ai)
