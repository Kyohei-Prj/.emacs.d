# Emacs Configuration

A modern, performance-oriented Emacs configuration for **frontend/web, Python, and general LSP-based development**.
It is built around `eglot` + Tree-sitter, a Vertico/Orderless/Corfu completion stack, native `tab-bar` tabs, and `vterm`/`multi-vterm` — with as little UI noise as possible.

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
| Terminal | `vterm` + `multi-vterm` (several terminals, `C-c t` prefix) |
| Windows / frames | Native `tab-bar-mode` with numeric tabs (`C-z` prefix) |
| Theme | `modus-vivendi` (accessible, high contrast) |

Deliberately avoided: Helm, Ivy, `lsp-mode`, `doom-emacs`, `spacemacs`, `company-mode`.
Buffer search is the built-in incremental isearch; `swiper` was dropped (it pulls
`ivy` in and does not use Vertico, Orderless or Embark) in favour of Consult's
`consult-line` / `consult-ripgrep`.

---

## 2. Requirements

### Emacs

- **Emacs 30 minimum, Emacs 31.1 recommended** (this config is developed and tested on 31.1).
  - Consult, Vertico, Corfu and friends require ≥ 29.1.
  - `init.el` does `(require 'use-package)` without bootstrapping it, and `use-package`
    has been built into Emacs since 29.1.
  - Emacs 31 renders Corfu's popups in the terminal too (TTY child frames,
    `(featurep 'tty-child-frames)`); on Emacs 30 you would need `corfu-terminal`.
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
- `zsh` (optional) — `init-core.el` resolves it with `(executable-find "zsh")` and uses it
  for shell commands when installed; if it is missing, Emacs' default shell is kept

---

## 3. Install Emacs Config

```bash
# Back up any existing config
mv ~/.emacs.d ~/.emacs.d.bak   # optional

git clone https://github.com/Kyohei-Prj/.emacs.d.git ~/.emacs.d
```

Start Emacs once. Package archives are preconfigured (GNU ELPA → NonGNU ELPA → MELPA, in that priority order) and `use-package-always-ensure` is enabled, so packages are downloaded and installed automatically on first launch; built-in packages (`project`, `tab-bar`, `eglot`) opt out with `:ensure nil`.

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

`lisp/init-core.el` extends `exec-path` and `$PATH` at startup — only directories that
exist are added, and existing duplicates are removed so neither a restart nor an
inherited shell `PATH` can grow it. Directories are prepended in reverse, so the
effective search order puts your own tools ahead of system ones (a stale
`/usr/local/bin` copy can no longer shadow a `uv tool install` build):

1. `~/.bun/bin`  ← highest priority
2. `~/.local/bin`  ← `uv tool install` lands here
3. `~/.cargo/bin`
4. `/opt/homebrew/bin`  ← macOS Homebrew (GUI Emacs does not inherit your shell `PATH`)
5. `/usr/local/bin`  ← lowest priority

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

> `tsc --lsp` needs the **native TypeScript 7+** compiler. `lisp/init-languages.el` probes `tsc --version` and silently falls back to `typescript-language-server --stdio` on TypeScript 5/6 — install both and nothing else is required.

`my/rass-command` (in `lisp/init-languages.el`) also drops any server whose binary is missing: with no `tailwindcss-language-server` you get plain `tsc --lsp`, and with no `rass` you get the first available server instead of a startup error.

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

- `uv-mode` is a global minor mode that puts the active uv virtualenv in the modeline and puts `.venv/bin` on `exec-path`/`PATH` (`C-c C-s` `uv-mode-set`, `C-c C-u` `uv-mode-unset`, or `M-x uv-mode`).
  `lisp/init-languages.el` enables it automatically for `python-mode` **and** `python-ts-mode` buffers whenever a `.venv` exists at or above the visited file — see `my/uv-mode-activate` in §8.
- `ruff-format-on-save-mode` formats Python buffers on save.
- Python LSP runs as **one** Eglot connection multiplexed by `rass`:

  ```
  rass -- basedpyright-langserver --stdio -- ruff server
  ```

  Without `rass`, Eglot just starts `basedpyright-langserver` (the missing binary is detected at connect time).

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

On Emacs 31 you usually do not have to: `treesit-auto-install-grammar` defaults to `ask`,
so the first time you open a file whose grammar is missing Emacs offers to fetch and
build it (`never` silences the prompt, `always` builds without asking).

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

Mode mapping is explicit (`auto-mode-alist`) rather than through Emacs 31's
`treesit-enabled-modes`, so it also works on Emacs 30, covers only the languages
configured here, and keeps `.zsh` on `sh-mode` — enabling every ts-mode would remap
shell files to `bash-ts-mode`.

The mapping in effect: `.ts → typescript-ts-mode`, `.tsx → tsx-ts-mode`, `.js/.jsx/.mjs/.cjs → js-ts-mode`, `.py → python-ts-mode`, `.json → json-ts-mode`, `.ya?ml → yaml-ts-mode`, `.sh → bash-ts-mode`, `.zsh → sh-mode`.
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
C-c t t                      # same, but through multi-vterm: named *vterm<1>*, cycle with C-c t n/p
M-: (treesit-language-available-p 'tsx)   ;; => t once the grammar is built
```

To read the LSP handshake log, note that `init-lsp.el` disables it via `eglot-events-buffer-config`:

```
M-: (setq eglot-events-buffer-config '(:size 200000 :format full)) RET
M-x eglot-reconnect RET
M-x eglot-events-buffer RET
```

If `M-x eglot` reports `Searching for program: No such file or directory`, the server binary is missing from `PATH` or not on `exec-path` (see §4).

Emacs 31 already ships defaults for several servers (`docker-langserver`,
`bash-language-server`, `yaml-language-server`, and the Markdown/HTML/CSS/JSON
candidates), so `init-languages.el` lists only servers with no default or ones it pins
to a single program deliberately; compare with `C-h v eglot-server-programs`.

---

## 7. Repository Layout

```
~/.emacs.d/
├── early-init.el          # before package/UI init: GC tuning, no tool/menu bar
├── init.el                # archives, package-init, theme, custom.el, module loader
├── custom.el              # machine-specific, gitignored
├── lisp/
│   ├── init-core.el        # defaults, backups/auto-saves, savehist, PATH, zsh
│   ├── init-ui.el          # Modus Vivendi, modeline, line numbers, which-key
│   ├── init-completion.el  # Vertico, Orderless, Marginalia, Corfu, Cape, Embark
│   ├── init-navigation.el  # Consult, project.el, mwim, windmove
│   ├── init-treesit.el     # grammar sources + ts-mode remaps + indentation
│   ├── init-lsp.el         # eglot auto-start, C-c l map (managed buffers), Flymake
│   ├── init-languages.el   # per-language servers, uv/ruff, markdown, dockerfile
│   ├── init-git.el         # Magit
│   ├── init-terminal.el    # tab-bar (C-z) + vterm + multi-vterm (C-c t)
│   └── init-editing.el     # multiple-cursors, vundo, envrc, editorconfig, ws-butler
├── elpa/                  # installed packages (gitignored)
├── eln-cache/             # native compilation cache (gitignored)
└── tree-sitter/           # compiled grammars, libtree-sitter-*.so (gitignored)
```

Branches: `master` is the stable branch, `develop` is where work happens.

---

## 8. Module Tour

- **early-init.el** — disables `package-enable-at-startup` (init.el owns package loading), hides menu/tool bars, prevents frame resizing, and raises `gc-cons-threshold` to 100 MB during startup.
- **init-core.el** — 2-space indent, 88-column `fill-column`, no lockfiles, numbered
  backups in `~/.emacs.d/backups/` and auto-saves (`#file#`) in `~/.emacs.d/auto-save/`
  instead of next to your files, trailing whitespace highlighted in `prog-mode`,
  `delete-selection-mode`, `electric-pair-mode`, `show-paren-mode`,
  `global-auto-revert-mode`, `save-place-mode`, `recentf-mode`, `winner-mode`,
  `savehist-mode` (keeps the minibuffer/Vertico history across sessions),
  `use-short-answers`, `tab-always-indent 'complete`, and the `PATH` additions described in §4.
- **init-ui.el** — loads `modus-vivendi`, trimmed modeline set with `setq-default`
  (modification marker / buffer / position / VC branch / modes / `mode-line-misc-info`,
  which is where Eglot shows its status), line numbers in `prog-mode`, blinking cursor
  switched off with `(blink-cursor-mode -1)`, no visible bell, no hollow cursor in
  inactive windows, `which-key` after 0.5 s.
- **init-completion.el** — Vertico in the minibuffer, Orderless matching
  (`partial-completion` for files), Marginalia annotations, global Corfu with 0.2 s
  auto-trigger after 1 char plus documentation popups and `M-i` for
  `completion-at-point`, Cape sources registered with `add-hook` in precedence order
  (`cape-file` → `cape-keyword` → `cape-dabbrev`; the first Capf that returns a result
  wins, and buffer-local sources such as Eglot's always take precedence), Embark for
  context actions.
- **init-lsp.el** — `eglot-ensure` on every `prog-mode`, async connect
  (`eglot-sync-connect nil`), auto-shutdown, extend to cross-references, LSP traffic
  logging off (`eglot-events-buffer-config '(:size 0 :format full)`), Flymake
  diagnostics every 0.5 s with `M-n`/`M-p` bound to next/previous error, one-line Eldoc.
  The `C-c l` prefix lives in `eglot-mode-map`, so it exists only in buffers with a
  running server (and leaves `C-c l` free for `outline-minor-mode`). Eglot enables
  Flymake itself, so nothing re-enables it here.
- **init-languages.el** — server table for Python, TS/TSX/JS, SQL, Markdown, Dockerfile, Bash, HTML, CSS, JSON, YAML; `markdown-mode` (`.md`/`.mdx`, `pandoc` backend, native code-block fontification); `dockerfile-mode`; `json-ts-mode` for `.json`. Two helpers keep the server list resilient:
  - `my/rass-command` — filters SERVERS down to what is on `exec-path`, wraps several survivors in `rass`, returns a lone survivor directly, and returns the first entry when nothing is installed so Eglot's normal "program not found" error is shown.
  - `my/tsc-supports-lsp-p` — returns non-nil only for `tsc` ≥ 7, which is what selects `tsc --lsp` over `typescript-language-server`.
  - `my/uv-mode-activate` — Python hook that turns on `uv-mode` and activates `.venv` in both `python-mode` and `python-ts-mode` (upstream `uv-mode-auto-activate-hook` only checks `python-mode`).
- **init-navigation.el** — Consult bindings (`consult-buffer`, `consult-line`,
  `consult-ripgrep`, `consult-history`, `consult-imenu`, `consult-flymake`,
  `consult-goto-line` via remaps), `consult-xref` for xref/definition popups,
  `project.el` on `C-c p` with `g` switched to `consult-ripgrep`, `mwim` on `C-a`/`C-e`,
  `windmove` on Shift+arrows. Buffer search stays on the built-in isearch.
- **init-terminal.el** — `tab-bar-mode` with 0-indexed numeric tab labels and a `C-z`
  prefix; `vterm` with `C-c ESC` forwarding a literal Escape to the shell and `C-z`
  listed in `vterm-keymap-exceptions` so the tab prefix is not sent to the shell;
  `multi-vterm` for several terminals at once (`C-c t` prefix, buffers named
  `*vterm<1>*`, `*vterm<2>*`, …, plus a project terminal and a dedicated bottom window),
  and `my/vterm-new-tab` (`C-z t`) which opens a terminal in a new tab and names that
  tab after the project or directory it starts in — `tab-bar` would otherwise label it
  with the `*vterm<N>*` buffer name. `multi-vterm-buffer-name` controls those buffer
  names, and `multi-vterm-dedicated-window-height-percent` (30) the bottom window's size.

---

## 9. Key Bindings Cheat Sheet

### Navigation & search

| Key | Action |
| --- | --- |
| `C-s` / `C-r` | `isearch-forward` / `isearch-backward` (built-in incremental buffer search) |
| `C-x b` | `consult-buffer` (buffers, files, projects, bookmarks) |
| `C-x C-r` | `consult-recent-file` |
| `M-y` | `consult-yank-pop` (kill-ring browser) |
| `M-s l` / `M-s L` | `consult-line` / `consult-line-multi` |
| `M-s g` | `consult-ripgrep` (project grep) |
| `M-g g` / `M-g i` / `M-g f` | `consult-goto-line` / `consult-imenu` / `consult-flymake` |
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

### LSP (prefix `C-c l`, available in Eglot-managed buffers)

| Key | Action |
| --- | --- |
| `C-c l r` | `eglot-rename` |
| `C-c l a` | `eglot-code-actions` |
| `C-c l f` | `eglot-format` |
| `C-c l h` | Eldoc / signature help |
| `C-c l d` / `C-c l D` | definitions / references |
| `C-c l q` / `C-c l R` | shutdown / reconnect server |
| `M-n` / `M-p` (in `flymake-mode`) | next / previous Flymake diagnostic |
| `M-g n` / `M-g p` | next / previous diagnostic through `next-error` (built-in) |
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
| `C-z t` | new tab holding a single terminal (`my/vterm-new-tab`) |
| `M-x vterm` | new terminal in this window (`multi-vterm` does not track it) |
| `C-c ESC` | send a literal Escape to the shell (in `vterm-mode`) |

`C-z` is listed in `vterm-keymap-exceptions`, so `C-z c` inside a `vterm` buffer creates a
tab instead of sending SIGTSTP to the shell. Remove a key from that list to hand it back
to the shell.

### Terminals (prefix `C-c t`)

| Key | Action |
| --- | --- |
| `C-c t t` | new terminal (`multi-vterm`), fills the current window |
| `C-c t n` / `C-c t p` | next / previous terminal |
| `C-c t P` | project terminal (`multi-vterm-project`), toggles a split at the project root |
| `C-c t d` | dedicated terminal in a bottom window (30% of its rows), toggle |
| `C-c t r` | rename the terminal under point |

Buffers are named `*vterm<1>*`, `*vterm<2>*`, … (`multi-vterm-buffer-name`), the project
one `*vterm - <project>*`. They are ordinary `vterm-mode` buffers, so everything above —
the keymap exceptions, `C-c ESC`, the `C-z` tab prefix — applies to all of them. The
`C-c t` prefix works inside a terminal too: `vterm-mode-map` only binds `C-c` as a prefix
(`C-c C-c`, `C-c C-l`, `C-c C-y`), and a sequence that prefix cannot complete falls through
to the global keymap.

### Editing

| Key | Action |
| --- | --- |
| `C-S-c C-S-c` | `mc/edit-lines` |
| `C->` / `C-<` | mark next / previous like this |
| `C-c C-<` | mark all like this |

### Terminal caveats

Most of the config works in `emacs -nw`, but a plain TTY cannot report these modifier
combinations, so the bindings are effectively GUI-only:

| Key | Bound to | Terminal-friendly alternative |
| --- | --- | --- |
| `C-.` / `C-;` | `embark-act` / `embark-dwim` | `M-x embark-act`, or add e.g. `("C-c :" . embark-act)` |
| `C->` / `C-<`, `C-S-c C-S-c`, `C-c C-<` | `multiple-cursors` | `M-x mc/mark-next-like-this`, or rebind under `C-c` |
| `S-<arrow>` | `windmove` | `C-x o`; some terminals need xterm-style modifier support |

Corfu's popup needs child frames, which Emacs 31 supports on TTY frames too
(`(featurep 'tty-child-frames)`); on Emacs 30 install `corfu-terminal` if you want the
popup in the terminal. `M-i` (`completion-at-point`) and everything in the minibuffer
work in the terminal as is.

---

## 10. Installed Elpa Packages

Installed automatically on first launch (`use-package-always-ensure` plus
`package-selected-packages` in `custom.el`):

- **Completion**: `vertico`, `orderless`, `marginalia`, `corfu`, `cape`, `embark`, `embark-consult`, `consult`
- **UI**: `modus-themes`, `which-key`, `rainbow-delimiters`
- **Editing**: `multiple-cursors`, `vundo`, `ws-butler`, `mwim`, `envrc`, `editorconfig`
- **Languages**: `markdown-mode`, `dockerfile-mode`, `uv-mode`, `ruff-format` (pulls `reformatter`, `pythonic`)
- **Git / terminal**: `magit`, `vterm`, `multi-vterm`

Transitive dependencies (`dash`, `s`, `f`, `cond-let`, `with-editor`, `magit-section`, `inheritenv`, `reformatter`, `pythonic`, `llama`) are pulled in automatically.

`which-key` and `editorconfig` are used by the config but are **built into Emacs 31**, so they never appear in `elpa/`.

`ivy`/`swiper` are gone (buffer search is plain isearch plus Consult); they remain in
`elpa/` on an existing checkout until `M-x package-autoremove` removes the orphans. The
same applies to `eat` if you installed it earlier — `multi-vterm` sits on top of `vterm`.

---

## 11. Performance Notes

- Start with a native-compilable Emacs; `eln-cache/` fills up after the first few launches and startup keeps improving.
- `gc-cons-threshold` is 100 MB during init, 32 MB afterwards. Raise it further (e.g. 128 MB) if you work on very large buffers.
- `eglot-events-buffer-config` is `'(:size 0 :format full)` (no log buffer churn) and `eglot-sync-connect` is nil (never blocks startup).
- Tree-sitter grammars are compiled once and cached; `*-ts-mode` buffers are cheaper than `treesit`-less modes.
- `modus-themes` is high-contrast but cheap: no runtime color computation, no fringe/box decorations.
- Everything is split into `lisp/init-*.el` modules, so a slow section can be commented out in `init.el` to bisect startup cost — profile with `M-x profile-start` / `profile-report`.

---

## 12. Troubleshooting

| Symptom | Fix |
| --- | --- |
| `Cannot open load file: No such file or directory, use-package` | Emacs < 30 — upgrade, or bootstrap it at the top of `init.el`: `(require 'package) (package-refresh-contents) (package-install 'use-package)`. |
| `tsx-ts-mode is not defined` / no TS highlighting | Grammar not built — §5. |
| `Failed to start LSP server` with `rass` in the message | `uv tool install rassumfrassum`. If you don't want aggregation at all, remove the `my/rass-command` wrapper in `lisp/init-languages.el` and give Eglot the single server you want. |
| No completion in a Python buffer | `M-x eglot-reconnect`; check `M-x eglot-events-buffer` and that `basedpyright-langserver`/`ruff` are on `PATH`. |
| `vterm` prompts to compile and then fails | Install `cmake`, a C/C++ compiler, and `libvterm` headers (§4.3). |
| `consult-ripgrep` fails with `Searching for program: No such file or directory` | Install `ripgrep`. |
| Markdown export does nothing | Install `pandoc`, or set `markdown-command` to something available. |
| Fonts/icons look wrong in the terminal | This config uses no icon fonts on purpose; pick a Nerd Font only if you add icon packages. |
| Emacs is slow on first run after `git pull` | Native recompilation in progress; check the `*Native-Compilation Errors*` / `*Warnings*` buffers and let the queue drain. |
| `uv` venv not activated in a Python buffer | `my/uv-mode-activate` needs `uv` on `PATH` **and** a `.venv` directory at or above the file. Otherwise force it with `M-x uv-mode-set` (`M-x uv-mode-unset` to undo). |
| LSP connects to only one of several servers | A server binary was missing from `exec-path`, so `my/rass-command` degraded to a single server. Install it and `M-x eglot-reconnect`. |
| `C-z c` in a `vterm` buffer suspends the shell instead of opening a tab | `C-z` must stay in `vterm-keymap-exceptions` (see §9) |
| `C-c t …` types into the shell instead of running the terminal commands | `C-c` must stay in `vterm-keymap-exceptions`; `multi-vterm` itself binds nothing, so a missing command means the package is not installed (§10) |
| No dabbrev/file completion at point, only LSP | Cape sources must be on the **global** `completion-at-point-functions` — `add-hook`, not `add-to-list`, which writes the startup buffer's local value |

---

## License

Personal configuration — do with it what you like. Upstream packages keep their own licenses.
