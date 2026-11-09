# Emacs Configuration

A modern, performance-oriented Emacs configuration for **frontend/web, Python, and general LSP-based development**.
It is built around `eglot` + Tree-sitter, a Vertico/Orderless/Corfu completion stack, native `tab-bar` tabs, and `vterm` — with as little UI noise as possible.

Works on **Linux and macOS**, both in GUI and in the terminal.

---

## Table of Contents

1. [Highlights](#1-highlights)
2. [Requirements](#2-requirements)
3. [Install Emacs Config](#3-install-emacs-config)
4. [External Toolchain (required for full functionality)](#4-external-toolchain)
   - [Node.js / TypeScript tooling](#41-nodejs--typescript-tooling)
   - [Python tooling](#42-python-tooling)
   - [System packages](#43-system-packages)
5. [Tree-sitter Grammars](#5-tree-sitter-grammars)
6. [Verify the Setup](#6-verify-the-setup)
7. [Repository Layout](#7-repository-layout)
8. [Module Tour](#8-module-tour)
9. [Key Bindings Cheat Sheet](#9-key-bindings-cheat-sheet)
10. [Installed Elpa Packages](#10-installed-elpa-packages)
11. [Performance Notes](#11-performance-notes)
12. [Troubleshooting](#12-troubleshooting)

---

## 1. Highlights

| Area | Stack |
| --- | --- |
| Completion (minibuffer) | Vertico + Orderless + Marginalia + Consult |
| Completion (in-buffer) | Corfu + Cape (auto, with popup docs) |
| Completion actions | Embark + embark-consult |
| Parsing / highlighting | Tree-sitter (`*-ts-mode` everywhere) |
| IDE features | `eglot` + [`rassumfrassum`](https://github.com/joaotavora/rassumfrassum) (`rass`) to multiplex several servers per project |
| Python | `basedpyright` + `ruff` (LSP + format-on-save) + `uv-mode` |
| TypeScript / TSX | `tsc --lsp` + `tailwindcss-language-server` (multiplexed) |
| Git | Magit |
| Terminal | `vterm` |
| Windows / frames | Native `tab-bar-mode` with numeric tabs (`C-z` prefix) |
| Theme | `modus-vivendi` (accessible, high contrast) |

Deliberately avoided: Helm, Ivy, `lsp-mode`, `doom-emacs`, `spacemacs`, `company-mode`.
(`ivy` is present only as an install-time dependency of `swiper`.)

---

## 2. Requirements

### Emacs

- **Emacs 30 minimum, Emacs 31.1 recommended** (this config is developed and tested on 31.1).
  - Consult, Vertico, Corfu and friends require ≥ 29.1.
  - `init.el` does `(require 'use-package)` without bootstrapping it, so `use-package` must be built in — that means ≥ 30.
- Built with **Tree-sitter** (`--with-tree-sitter`) — check with:

  ```elisp
  M-: (treesit-available-p) RET   ;; => t
  ```

- Built with **native compilation** (`--with-native-compilation`) — strongly recommended for startup and responsiveness.
- Built with **JSON, module, and (for `vterm`) dynamic-module** support.

```bash
emacs --version
```

### Other host requirements

- `git` (Magit, plus grammar installation)
- A C compiler and `make` (`gcc`/`clang`, `build-essential` on Debian/Ubuntu, `base-devel` on Arch) — needed for `vterm`'s native module and for compiling Tree-sitter grammars
- `zsh` — `init-core.el` sets `shell-file-name`/`explicit-shell-file-name` to `/bin/zsh`

---

## 3. Install Emacs Config

```bash
# Back up any existing config
mv ~/.emacs.d ~/.emacs.d.bak   # optional

git clone https://github.com/Kyohei-Prj/.emacs.d.git ~/.emacs.d
```

Start Emacs once. Package archives are preconfigured (GNU ELPA → NonGNU ELPA → MELPA, in that priority order) and every `use-package` form uses `:ensure t`, so packages are downloaded and installed automatically on first launch.

```bash
emacs        # GUI
emacs -nw    # terminal
```

Notes:

- `elpa/`, `eln-cache/`, `tree-sitter/`, `transient/`, session files and `custom.el` are gitignored — the clone is source-only.
- `custom.el` is loaded if present and is machine-specific; keep your personal tweaks there, not in `lisp/`.
- Startup garbage collection is relaxed in `early-init.el` and restored after startup (`gc-cons-threshold` 100 MB → 32 MB).

---

## 4. External Toolchain

The Elisp side installs *no* external binaries. Every language server, formatter, and CLI helper below must be on your `PATH`.

`lisp/init-core.el` adds these directories to `exec-path` and `$PATH` at startup, so install tools into one of them:

1. `~/.bun/bin`
2. `~/.local/bin`  ← `uv tool install` lands here
3. `~/.cargo/bin`
4. `/usr/local/bin`

### 4.1 Node.js / TypeScript tooling

The recommended runtime is **Bun** (fast installs, and `~/.bun/bin` is already on the Emacs `PATH`).

```bash
# Install Bun
curl -fsSL https://bun.sh/install | bash
exec -l $SHELL      # reload PATH
```

Install the language servers and formatters used by `lisp/init-languages.el`:

```bash
bun install -g \
  typescript@latest \
  typescript-language-server \
  @tailwindcss/language-server \
  vscode-langservers-extracted \
  yaml-language-server \
  bash-language-server \
  dockerfile-language-server-nodejs \
  sql-language-server \
  prettier
```

Equivalent with npm/nvm if you prefer Node:

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
nvm install --lts
npm install -g typescript typescript-language-server @tailwindcss/language-server \
  vscode-langservers-extracted yaml-language-server bash-language-server \
  dockerfile-language-server-nodejs sql-language-server prettier
```

What each package provides:

| Binary on `PATH` | npm package | Used for |
| --- | --- | --- |
| `tsc` | `typescript` (≥ 7.0, native tsc) | `tsc --lsp --stdio` — TS/TSX/JS server |
| `typescript-language-server` | `typescript-language-server` | fallback server, `--stdio` |
| `tailwindcss-language-server` | `@tailwindcss/language-server` | Tailwind classes (multiplexed with `tsc`) |
| `vscode-html-language-server`, `vscode-css-language-server`, `vscode-json-language-server` | `vscode-langservers-extracted` | HTML / CSS / JSON |
| `yaml-language-server` | `yaml-language-server` | YAML |
| `bash-language-server` | `bash-language-server` | Shell (`bash-ts-mode`) |
| `docker-langserver` | `dockerfile-language-server-nodejs` | Dockerfile |
| `sql-language-server` | `sql-language-server` | SQL |
| `prettier` | `prettier` | project formatter on the CLI (run via `M-x compile` / `M-x async-shell-command`); `eglot-format` is used inside Emacs |

> `tsc --lsp` requires the **native TypeScript 7+** compiler. On older TypeScript (5.x/6.x) use `typescript-language-server --stdio` instead, i.e. edit the `eglot-server-programs` entry in `lisp/init-languages.el`.

Project-local servers are also picked up: keep `node_modules/.bin` in your project and `eglot`/`rass` will resolve local binaries.

### 4.2 Python tooling

```bash
# Install uv (also provides `uv tool` for isolated tool installs)
curl -LsSf https://astral.sh/uv/install.sh | sh
exec -l $SHELL

uv tool install basedpyright      # provides basedpyright-langserver
uv tool install ruff              # provides `ruff server` + formatter
uv tool install rassumfrassum     # provides `rass`, the LSP multiplexer
```

- `uv-mode` is a global minor mode that puts the active uv virtualenv in the modeline and adds `.venv/bin` to `exec-path`/`PATH` (`C-c C-s` `uv-mode-set`, `C-c C-u` `uv-mode-unset`, or `M-x uv-mode`).
  See the note in §12 about `python-ts-mode` and auto-activation.
- `ruff-format-on-save-mode` formats Python buffers on save.
- Python LSP runs as **one** Eglot connection multiplexed by `rass`:

  ```
  rass -- basedpyright-langserver --stdio -- ruff server
  ```

### 4.3 System packages

| Tool | Purpose | Debian/Ubuntu | Arch | macOS (brew) |
| --- | --- | --- | --- | --- |
| `ripgrep` | `consult-ripgrep` / `M-s g` project search | `sudo apt install ripgrep` | `sudo pacman -S ripgrep` | `brew install ripgrep` |
| `pandoc` | `markdown-command` for Markdown export/preview | `sudo apt install pandoc` | `sudo pacman -S pandoc` | `brew install pandoc` |
| `marksman` | Markdown LSP | `sudo apt install marksman` | `sudo pacman -S marksman` | `brew install marksman` |
| `libvterm` + headers, `cmake` | compile `vterm`'s native module | `sudo apt install libvterm-dev cmake build-essential` | `sudo pacman -S libvterm cmake base-devel` | `brew install libvterm cmake` |

`editorconfig-mode` is pure Elisp — no `editorconfig-core-c` binary needed.

---

## 5. Tree-sitter Grammars

Grammars are **not** committed (`.gitignore`d `tree-sitter/`); build them once per machine. They require `git`, a C compiler, and network access.

Interactively:

```
M-x treesit-install-language-grammar RET tsx RET
```

Or all of them in one shot from a shell:

```bash
emacs -Q --batch \
  --eval '(progn
            (require (quote treesit))
            (load (expand-file-name "lisp/init-treesit.el" user-emacs-directory))
            (dolist (lang (quote (bash css html javascript json python
                                   typescript tsx yaml)))
              (condition-case err
                  (treesit-install-language-grammar lang)
                (error (message "FAILED %s: %s" lang err)))))'
```

Configured languages (`lisp/init-treesit.el`): `bash`, `css`, `html`, `javascript`, `json`, `python`, `typescript`, `tsx`, `yaml`.
`typescript`/`tsx` are pinned to the `master` branch of `tree-sitter-typescript`, with the grammar in the `typescript/src` and `tsx/src` subdirectories.

Mode mapping is automatic: `.ts → typescript-ts-mode`, `.tsx → tsx-ts-mode`, `.js/.jsx/.mjs/.cjs → js-ts-mode`, `.py → python-ts-mode`, `.json → json-ts-mode`, `.ya?ml → yaml-ts-mode`, `.sh → bash-ts-mode`, `.zsh → sh-mode`.
Indent defaults: Python 4, JS/TS 2, CSS 2.

---

## 6. Verify the Setup

```bash
which tsc rass basedpyright-langserver ruff rg
```

Inside Emacs:

```
M-x eglot                    # in a .ts / .tsx / .py file: "EGLOT (... running)" in the modeline
M-x consult-ripgrep          # project-wide search
M-x vterm                    # native terminal (compiles the module on first use)
M-: (treesit-language-available-p 'tsx)   ;; => t once the grammar is built
```

To read the LSP handshake log, note that `init-lsp.el` sets `eglot-events-buffer-size` to `0`:

```
M-: (setq eglot-events-buffer-size 100000) RET
M-x eglot-reconnect RET
M-x eglot-events-buffer RET
```

If `M-x eglot` reports `Searching for program: No such file or directory`, the server binary is missing from `PATH` or not on `exec-path` (see §4).

---

## 7. Repository Layout

```
~/.emacs.d/
├── early-init.el          # before package/UI init: GC tuning, no tool/menu bar
├── init.el                # archives, package-init, theme, custom.el, module loader
├── custom.el              # machine-specific, gitignored
├── lisp/
│   ├── init-core.el        # defaults, backups, PATH (bun/uv/cargo/local), zsh
│   ├── init-ui.el          # Modus Vivendi, modeline, line numbers, which-key
│   ├── init-completion.el  # Vertico, Orderless, Marginalia, Corfu, Cape, Embark
│   ├── init-navigation.el  # Consult, project.el, mwim, swiper, windmove
│   ├── init-treesit.el     # grammar sources + ts-mode remaps + indentation
│   ├── init-lsp.el         # eglot auto-start, C-c l map, Flymake diagnostics
│   ├── init-languages.el   # per-language servers, uv/ruff, markdown, dockerfile
│   ├── init-git.el         # Magit
│   ├── init-terminal.el    # tab-bar (C-z) + vterm
│   └── init-editing.el     # multiple-cursors, vundo, envrc, editorconfig, ws-butler
├── elpa/                  # installed packages (gitignored)
├── eln-cache/             # native compilation cache (gitignored)
└── tree-sitter/           # compiled grammars, libtree-sitter-*.so (gitignored)
```

Branches: `master` is the stable branch, `develop` is where work happens.

---

## 8. Module Tour

- **early-init.el** — disables `package-enable-at-startup` (init.el owns package loading), hides menu/tool bars, prevents frame resizing, and raises `gc-cons-threshold` to 100 MB during startup.
- **init-core.el** — 2-space indent, 88-column `fill-column`, no backups in-place (they go to `~/.emacs.d/backups/`), no lockfiles, `delete-selection-mode`, `electric-pair-mode`, `show-paren-mode`, `global-auto-revert-mode`, `save-place-mode`, `recentf-mode`, `winner-mode`, `use-short-answers`, and the `PATH` additions described in §4.
- **init-ui.el** — loads `modus-vivendi`, trimmed modeline (buffer / position / modes), line numbers in `prog-mode`, no blinking cursor or visible bell, `which-key` after 0.5 s.
- **init-completion.el** — Vertico in the minibuffer, Orderless matching (`partial-completion` for files), Marginalia annotations, global Corfu with 0.2 s auto-trigger after 1 char plus documentation popups, Cape sources (`cape-file`, `cape-dabbrev`, `cape-keyword`), Embark for context actions.
- **init-lsp.el** — `eglot-ensure` on every `prog-mode`, async connect (`eglot-sync-connect nil`), auto-shutdown, extend to cross-references, Flymake diagnostics every 0.5 s, one-line Eldoc.
- **init-languages.el** — server table for Python, TS/TSX/JS, SQL, Markdown, Dockerfile, Bash, HTML, CSS, JSON, YAML; `markdown-mode` (`.md`/`.mdx`, `pandoc` backend, native code-block fontification); `dockerfile-mode`; `json-ts-mode` for `.json`.
- **init-terminal.el** — `tab-bar-mode` with 0-indexed numeric tab labels and a `C-z` prefix; `vterm` with `C-c ESC` forwarding a literal Escape to the shell.

---

## 9. Key Bindings Cheat Sheet

### Navigation & search

| Key | Action |
| --- | --- |
| `C-s` / `C-r` | `swiper` / `swiper-backward` (isearch-style buffer search) |
| `C-x b` | `consult-buffer` (buffers, files, projects, bookmarks) |
| `C-x C-r` | `consult-recent-file` |
| `M-y` | `consult-yank-pop` (kill-ring browser) |
| `M-s l` / `M-s L` | `consult-line` / `consult-line-multi` |
| `M-s g` | `consult-ripgrep` (project grep) |
| `C-c h` / `C-c m` / `C-c k` | history / set marker / global markers |
| `C-a` / `C-e` | `mwim-beginning-of-code-or-line` / `mwim-end-of-code-or-line` |
| `S-<arrow>` | `windmove` window switching |
| `C-c p` | `project-prefix-map` (`f` files, `g` ripgrep, `b` buffers, `p` switch project) |
| `C-x u` | `vundo` visual undo tree |

### Completion

| Key | Action |
| --- | --- |
| `M-i` | `completion-at-point` (Corfu popup) |
| `TAB` | accept / complete more |
| `<up>`/`<down>`, `M-p`/`M-n` | previous / next candidate |
| `M-g` | candidate location (xref) |
| `M-h` | candidate documentation |
| `TAB` / `M-RET` (minibuffer) | insert candidate / exit minibuffer with the current input |
| `~` suffix, space-separated words (minibuffer) | Orderless patterns: `foo~` fuzzy, `bar baz` matches in any order |
| `C-.` / `C-;` | `embark-act` / `embark-dwim` |
| `C-h B` | `embark-bindings` (searchable keymap help) |

### LSP (prefix `C-c l`)

| Key | Action |
| --- | --- |
| `C-c l r` | `eglot-rename` |
| `C-c l a` | `eglot-code-actions` |
| `C-c l f` | `eglot-format` |
| `C-c l h` | Eldoc / signature help |
| `C-c l d` / `C-c l D` | definitions / references |
| `C-c l q` / `C-c l R` | shutdown / reconnect server |
| `M-g n` / `M-g p` | next / previous Flymake diagnostic (built-in) |
| `C-c C-s` / `C-c C-u` | `uv-mode-set` / `uv-mode-unset` (while `uv-mode` is on) |

### Git

| Key | Action |
| --- | --- |
| `C-x g` | `magit-status` (full frame) |
| `C-c g l` | `magit-log-current` |
| `C-c g b` | `magit-blame` |

### Tabs & terminal (prefix `C-z`)

| Key | Action |
| --- | --- |
| `C-z c` | new tab |
| `C-z k` | close tab |
| `C-z 0` … `C-z 9` | jump to tab 1 … 10 |
| `M-x vterm` | new terminal (tab-bar keeps it in place) |
| `C-c ESC` | send a literal Escape to the shell (in `vterm-mode`) |

### Editing

| Key | Action |
| --- | --- |
| `C-S-c C-S-c` | `mc/edit-lines` |
| `C->` / `C-<` | mark next / previous like this |
| `C-c C-<` | mark all like this |

---

## 10. Installed Elpa Packages

Installed automatically on first launch (`:ensure t` plus `package-selected-packages` in `custom.el`):

- **Completion**: `vertico`, `orderless`, `marginalia`, `corfu`, `cape`, `embark`, `embark-consult`, `consult`
- **UI**: `modus-themes`, `which-key`, `rainbow-delimiters`
- **Editing**: `multiple-cursors`, `vundo`, `ws-butler`, `mwim`, `envrc`, `editorconfig`
- **Languages**: `markdown-mode`, `dockerfile-mode`, `uv-mode`, `ruff-format` (pulls `reformatter`, `pythonic`)
- **Git / terminal**: `magit`, `vterm`

Transitive dependencies (`dash`, `s`, `f`, `cond-let`, `with-editor`, `magit-section`, `inheritenv`, `reformatter`, `pythonic`, `ivy`) are pulled in automatically.

Installed but currently **not referenced** by any module (leftovers you can safely `M-x package-delete`): `eat`, `multi-vterm`. `ivy`/`swiper` are a matched pair — `swiper` is used for in-buffer search, `ivy` is its dependency.

---

## 11. Performance Notes

- Start with a native-compilable Emacs; `eln-cache/` fills up after the first few launches and startup keeps improving.
- `gc-cons-threshold` is 100 MB during init, 32 MB afterwards. Raise it further (e.g. 128 MB) if you work on very large buffers.
- `eglot-events-buffer-size` is 0 (no log buffer churn) and `eglot-sync-connect` is nil (never blocks startup).
- Tree-sitter grammars are compiled once and cached; `*-ts-mode` buffers are cheaper than `treesit`-less modes.
- `modus-themes` is high-contrast but cheap: no runtime color computation, no fringe/box decorations.
- Everything is split into `lisp/init-*.el` modules, so a slow section can be commented out in `init.el` to bisect startup cost — profile with `M-x profile-start` / `profile-report`.

---

## 12. Troubleshooting

| Symptom | Fix |
| --- | --- |
| `Cannot open load file: No such file or directory, use-package` | Emacs < 30 — upgrade, or bootstrap it at the top of `init.el`: `(require 'package) (package-refresh-contents) (package-install 'use-package)`. |
| `tsx-ts-mode is not defined` / no TS highlighting | Grammar not built — §5. |
| `Failed to start LSP server` with `rass` in the message | `uv tool install rassumfrassum`, or replace the `rass ...` wrapper in `lisp/init-languages.el` with the single server you want. |
| No completion in a Python buffer | `M-x eglot-reconnect`; check `M-x eglot-events-buffer` and that `basedpyright-langserver`/`ruff` are on `PATH`. |
| `vterm` prompts to compile and then fails | Install `cmake`, a C/C++ compiler, and `libvterm` headers (§4.3). |
| `consult-ripgrep` fails with `Searching for program: No such file or directory` | Install `ripgrep`. |
| Markdown export does nothing | Install `pandoc`, or set `markdown-command` to something available. |
| Fonts/icons look wrong in the terminal | This config uses no icon fonts on purpose; pick a Nerd Font only if you add icon packages. |
| Emacs is slow on first run after `git pull` | Native recompilation in progress; check the `*Native-Compilation Errors*` / `*Warnings*` buffers and let the queue drain. |
| `uv` venv not activated in `.py` files | `uv-mode-auto-activate-hook` only fires for buffers deriving from `python-mode`, but `.py` opens `python-ts-mode`. Run `M-x uv-mode-set` manually (or change `auto-mode-alist` in `lisp/init-treesit.el` to use `python-mode`). |

---

## License

Personal configuration — do with it what you like. Upstream packages keep their own licenses.
