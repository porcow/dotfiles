;;; install agent-shell
(use-package agent-shell
  :ensure t
  :ensure-system-package
  ;; Add agent installation configs here
  ((claude . "curl -fsSL https://claude.ai/install.sh | bash")
   (claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp")
   (codex . "npm i -g @openai/codex")
   (codex-acp . "npm install -g @zed-industries/codex-acp")
   (pi . "npm install -g @mariozechner/pi-coding-agent")
   (pi-acp . "npm install -g pi-acp")))

(provide 'ww-ai)
