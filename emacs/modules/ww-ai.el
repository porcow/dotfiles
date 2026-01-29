;;; install agent-shell
(use-package agent-shell
    :ensure t
    :ensure-system-package
    ;; Add agent installation configs here
    ((claude . "brew install claude-code")
     (claude-code-acp . "npm install -g @zed-industries/claude-code-acp")
     (codex . "npm i -g @openai/codex")
     (codex-acp . "npm install -g @zed-industries/codex-acp")))

(provide 'ww-ai)
