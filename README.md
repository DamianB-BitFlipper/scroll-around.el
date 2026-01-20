# scroll-paragraph.el

Scroll the view by one paragraph while keeping the cursor at the same screen position.

Unlike `forward-paragraph` and `backward-paragraph` which move the cursor, these commands move the view instead.

## Installation

### Doom Emacs

Add to `~/.doom.d/packages.el`:

```elisp
(package! scroll-paragraph
  :recipe (:host github :repo "DamianB-BitFlipper/scroll-paragraph.el"))
```

Add to `~/.doom.d/config.el`:

```elisp
(use-package! scroll-paragraph
  :config
  (map! :map 'override
        :desc "Scroll forward paragraph" "C-S-<down>" #'scroll-forward-paragraph
        :desc "Scroll backward paragraph" "C-S-<up>" #'scroll-backward-paragraph))
```

Then run `doom sync`.

### Vanilla Emacs

Clone the repository:

```bash
git clone https://github.com/DamianB-BitFlipper/scroll-paragraph.el ~/.emacs.d/lisp/scroll-paragraph.el
```

Add to your `init.el`:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/scroll-paragraph.el")
(require 'scroll-paragraph)

(global-set-key (kbd "C-S-<down>") #'scroll-forward-paragraph)
(global-set-key (kbd "C-S-<up>") #'scroll-backward-paragraph)
```

### use-package (with straight.el)

```elisp
(use-package scroll-paragraph
  :straight (:host github :repo "DamianB-BitFlipper/scroll-paragraph.el")
  :bind
  ("C-S-<down>" . scroll-forward-paragraph)
  ("C-S-<up>" . scroll-backward-paragraph))
```

## Usage

- `C-S-<down>` (`scroll-forward-paragraph`): Scroll view forward by one paragraph
- `C-S-<up>` (`scroll-backward-paragraph`): Scroll view backward by one paragraph

## License

LGPL-3.0
