# scroll-around.el

Scroll the view by paragraph or line while keeping the cursor at the same screen position.

Unlike `forward-paragraph` and `backward-paragraph` which move the cursor, these commands move the view instead.

## Installation

### Doom Emacs

Add to `~/.doom.d/packages.el`:

```elisp
(package! scroll-around
  :recipe (:host github :repo "DamianB-BitFlipper/scroll-around.el"))
```

Add to `~/.doom.d/config.el`:

```elisp
(use-package! scroll-around
  :config
  (scroll-around-mode 1))
```

Then run `doom sync`.

### Vanilla Emacs

Clone the repository:

```bash
git clone https://github.com/DamianB-BitFlipper/scroll-around.el ~/.emacs.d/lisp/scroll-around.el
```

Add to your `init.el`:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/scroll-around.el")
(require 'scroll-around)
(scroll-around-mode 1)
```

### use-package (with straight.el)

```elisp
(use-package scroll-around
  :straight (:host github :repo "DamianB-BitFlipper/scroll-around.el")
  :config
  (scroll-around-mode 1))
```

## Usage

Enable `scroll-around-mode` to activate the default keybindings:

| Key | Command | Description |
|-----|---------|-------------|
| `C-S-<down>` | `scroll-around-forward-paragraph` | Scroll view forward by one paragraph |
| `C-S-<up>` | `scroll-around-backward-paragraph` | Scroll view backward by one paragraph |
| `S-<down>` | `scroll-around-forward-line` | Scroll view forward by one line |
| `S-<up>` | `scroll-around-backward-line` | Scroll view backward by one line |

## License

LGPL-3.0
