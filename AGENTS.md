# Repository Guidelines

## Project Structure & Module Organization
- `emacs/init.el` is the main entry point; it adds `emacs/modules` to the load path and pulls in modular configs.
- `emacs/modules/*.el` contains feature-focused modules (for example, `dw-workflow.el`). Module files use `dw-*.el` or `ww-*.el` names and provide matching symbols.
- `neovim/` and `zsh/` currently hold placeholder `README.org` files; add configs there if you introduce those editors/shell settings.

## Build, Test, and Development Commands
- No build system or test runner is configured in this repo.
- Quick Emacs sanity check from the repo root:
  - `emacs -Q -l emacs/init.el`
  - First run may download packages from MELPA via `use-package`.

## Coding Style & Naming Conventions
- Emacs Lisp uses 2-space indentation and no tabs (`indent-tabs-mode` is disabled).
- Keep `lexical-binding: t` headers in `.el` files.
- Prefix custom functions/variables with `dw/` or `ww/`; keep file names in kebab case (for example, `dw-present.el`).
- Trailing whitespace is stripped on save; keep lines tidy.

## Testing Guidelines
- There are no automated tests. Validate changes manually by loading Emacs and exercising the updated module.
- If you touch a module with external integrations (auth, notes, etc.), verify paths and credentials are still correct.

## Commit & Pull Request Guidelines
- Commit messages are short, imperative, and capitalized (for example, "Update load path for Emacs configuration modules").
- PRs should include a brief summary, affected area (`emacs/`, `neovim/`, `zsh/`), and verification notes. Add screenshots only for UI-facing changes.

## Configuration & Security Notes
- The configuration expects the repo at `~/.dotfiles` (see the load-path in `emacs/init.el`).
- Auth uses `pass` when `~/.password-store` exists; keep secrets out of this repo.
- Some modules reference `~/Notes`; document any path changes you make.
