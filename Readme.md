# Emacs Configuration

This repository contains a modern, performance-oriented Emacs configuration tailored for **CLI tooling, frontend/web development, Python development, and LSP-based workflows**.
It emphasizes fast startup, minimal UI noise, strong completion, Tree-sitter syntax support, and first-class Language Server Protocol (LSP) integration.

The configuration is designed to work well on **Linux and macOS**, both in GUI and terminal environments.

---

## Table of Contents

1. Design Philosophy
2. System Requirements
3. Setup Instructions
4. Configuration Overview

   * Core System & UI
   * Completion & Navigation
   * Project & Version Control
   * LSP, Tree-sitter & Formatting
   * Language-Specific Tooling
   * Tabs & Window Management
5. Key Bindings Cheat Sheet
6. External Dependencies
7. Performance Notes & Tips

---

## 1. Design Philosophy

This configuration is built around the following principles:

* **Minimal but powerful UI** (no toolbars, no menus, clean modeline)
* **Modern completion stack** (Vertico + Orderless + Corfu)
* **Tree-sitter first** for syntax highlighting and parsing
* **LSP as the primary IDE engine**, optimized for performance
* **Opinionated formatting** using industry-standard tools (Prettier, Ruff)
* **Keyboard-centric navigation**, discoverable via Which-Key

It intentionally avoids older completion stacks (Helm, Ivy, Company) and heavy all-in-one frameworks.

---

## 2. System Requirements

### Emacs

* **Emacs 29 or newer** (required for built-in Tree-sitter support)
* Built with:

  * JSON support
  * Native compilation (recommended but optional)

Verify with:

```bash
emacs --version
```

### Operating System

* Linux or macOS
* Works in:

  * GUI Emacs
  * Terminal Emacs (with limited icon support)

---

## 3. Setup Instructions

### 3.1 Clone and Install

Place `init.el` in your Emacs configuration directory:

```bash
~/.emacs.d/init.el
```

Start Emacs once to allow `use-package` to bootstrap and install packages.

---

### 3.2 Fonts & Icons (Highly Recommended)

This configuration uses **nerd icons** for the modeline and completion UI.

1. Install a Nerd Font (example):

```bash
# Linux (example)
sudo apt install fonts-firacode

# Or manually install a Nerd Font
https://www.nerdfonts.com/
```

2. Install icon fonts inside Emacs:

```elisp
M-x nerd-icons-install-fonts
```

Restart Emacs afterward.

---

### 3.3 Tree-sitter Grammars

Tree-sitter grammars are installed automatically on first use.

When prompted, accept installation.

You can also manually install all supported grammars:

```elisp
M-x treesit-auto-install-all
```

---

## 4. Configuration Overview

### 4.1 Core System & UI

**Startup & Performance**

* Garbage collection threshold increased during startup
* LSP process output size increased
* Optional `emacs-lsp-booster` integration for JSON parsing speed

**UI Cleanup**

* No splash screen
* No toolbar, menu bar, scroll bar
* Visible bell instead of audible
* Column number enabled
* Line numbers enabled globally (disabled in terminal-like modes)

**Quality-of-Life**

* Automatic bracket pairing
* Cursor position remembered per file
* `Shift + Arrow` window navigation
* Which-Key for keybinding discovery

---

### 4.2 Theme & Visual Enhancements

* **Theme:** `doom-one`
* **Modeline:** `doom-modeline`
* **Delimiter coloring:** `rainbow-delimiters`
* **Icons:** `nerd-icons` (GUI only)

---

### 4.3 Completion & Navigation Stack

This configuration uses a **modern, modular completion ecosystem**:

| Component  | Purpose                       |
| ---------- | ----------------------------- |
| Vertico    | Vertical minibuffer UI        |
| Orderless  | Flexible fuzzy matching       |
| Marginalia | Rich annotations              |
| Consult    | Powerful search/navigation    |
| Corfu      | In-buffer completion popup    |
| Cape       | Completion sources/extensions |

Notable behaviors:

* Instant popup completion
* Icons in completion menus (GUI)
* Works in terminal via `corfu-terminal`

---

### 4.4 Project & Version Control

* **Magit** for Git operations
* **diff-hl** for live change indicators in the fringe
* **Projectile** for project management and navigation

---

### 4.5 LSP, Tree-sitter & Formatting

**Tree-sitter**

* Enabled globally
* Automatic mode remapping (e.g. `js-mode` → `js-ts-mode`)

**LSP**

* `lsp-mode` with performance-oriented settings
* Diagnostics via **Flymake**
* UI enhancements via `lsp-ui`
* Corfu used instead of LSP completion

**Formatting**

* `apheleia` for async formatting
* Prettier for:

  * JavaScript / TypeScript
  * TSX
  * HTML / CSS
  * JSON
* Python formatting via Ruff

---

### 4.6 Language-Specific Tooling

#### Frontend / Web

* ESLint LSP integration
* Tailwind CSS LSP
* Automatic `node_modules/.bin` PATH injection

#### Python

* Pyright LSP
* Virtual environment management via `pyvenv`
* `uv` auto-activation
* Ruff formatting on save
* Jupyter notebook integration (`ein`)

#### Markdown

* `markdown-mode`

#### Docker

* `docker` package
* `dockerfile-mode`

---

### 4.7 Tabs & Window Management

* Native **Emacs tab-bar** enabled
* Custom numeric tab names
* Dedicated tab prefix under `C-z`

---

## 5. Key Bindings Cheat Sheet

### General Navigation

| Key             | Action                     |
| --------------- | -------------------------- |
| `C-s`           | Search in buffer (Consult) |
| `C-x b`         | Switch buffer              |
| `M-y`           | Yank history               |
| `C-a`           | Beginning of code or line  |
| `C-e`           | End of code or line        |
| `Shift + Arrow` | Move between windows       |

---

### Completion (Corfu)

| Key   | Action                     |
| ----- | -------------------------- |
| `TAB` | Confirm completion         |
| `M-d` | Toggle documentation popup |

---

### Project & Git

| Key     | Action                 |
| ------- | ---------------------- |
| `C-x g` | Magit status           |
| `C-c p` | Projectile command map |

---

### LSP / Diagnostics

| Key     | Action                  |
| ------- | ----------------------- |
| `M-n`   | Next diagnostic         |
| `M-p`   | Previous diagnostic     |
| `C-c l` | Show diagnostics buffer |

---

### Tabs (`C-z` prefix)

| Key        | Action        |
| ---------- | ------------- |
| `C-z c`    | New tab       |
| `C-z k`    | Close tab     |
| `C-z 0..9` | Switch to tab |

---

## 6. External Dependencies

Install these tools for full functionality:

### TypeScript Tooling

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
nvm install --lts
npm install -g typescript-language-server typescript vscode-langservers-extracted eslint
npm install -D prettier prettier-plugin-tailwindcss
```

### Python Tooling

```bash
npm install -g pyright
curl -LsSf https://astral.sh/uv/install.sh | sh
uv tool install ruff
```

### LSP Performance Booster

For faster LSP JSON parsing:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install emacs-lsp-booster
```

Ensure `emacs-lsp-booster` is in your `PATH`.

### Project wide grep

For project wide search:

```bash
sudo pacman -S ripgrep
```


---

## 7. Performance Notes & Tips

* Native compilation (`--with-native-compilation`) significantly improves responsiveness
* Tree-sitter grammars are cached after first install
* LSP settings are tuned for large projects
* Terminal Emacs is fully supported, with graceful degradation of icons

---
