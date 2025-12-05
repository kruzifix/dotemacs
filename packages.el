;; Initialize package.el for loading third-party packages.  Also set
;; up package.el to accept packages from the MELPA package archives,
;; the largest package repository for Emacs.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; `repeat-mode' allows you to trigger certain repeat commands by
;; typing the final character without the control modifier.  For
;; example, if you're switching between multiple buffers, you can use
;; "C-x o o o" to swap three times, instead of repeating the full
;; sequence: "C-x o", "C-x o", "C-x o".
(use-package repeat
  :config
  (repeat-mode))

;; When there are conflicting names in your buffer-selector ("C-x b"),
;; `uniquify' disambiguates them by prepending the directory.
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Emacs ships with several completion engines, but none are as
;; flexible as Vertico.  This is the secret sauce that powers the
;; Emacs "Command Palette", enabling tab-completion when using "M-x
;; command", `project-find-file', and other minibuffer commands.
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Persist minibuffer history over Emacs restarts.  Vertico uses this
;; to sort based on history.
(use-package savehist
  :init
  (savehist-mode))

;; Marginalia adds, well, marginalia to the Emacs minibuffer,
;; extending Vertico with a ton of rich information.
(use-package marginalia
  :ensure t
  :after vertico
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the
  ;; binding available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package
  ;; such that the mode gets enabled right away.  Note that this
  ;; forces loading the package.
  (marginalia-mode))

;; When the minibuffer is open and you're searching for some text,
;; Emacs can be very persnickety about the order in which you type.
;; Orderless laxes this behavior so the search is "fuzzier"; you'll
;; see results more often even if you type things in the wrong order.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Replace the default Emacs isearch with Consult, which offers a rich
;; minibuffer results window and integration with Orderless.
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

;; Where Vertico is a completion engine for your Emacs minibuffer,
;; Corfu is a completion engine for your source code.  This package
;; takes the data from things like LSP or Dabbrev and puts those
;; results in a convenient autocomplete.
(use-package corfu
  :ensure t
  ;; Recommended: Enable Corfu globally.  This is recommended since
  ;; Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  ;; You might want to configure these variables to suit your
  ;; preferences.  It's recommended to have some amount of delay if
  ;; you use Corfu with Eglot, otherwise editing performance could
  ;; suffer.
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2))

;; Enhance Corfu with additional completion functions so that it
;; provides more suggestions.  For example, `cape-dabbrev' looks at
;; words in the current buffer as a source for completions.  There are
;; many more completion functions available than those listed, so it's
;; worth reading through the Cape documentation to discover others
;; that may be useful to your workflow.
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Emacs includes Tree-sitter support as of version 29, but does not
;; bundle Tree-sitter grammars via the usual installation methods.
;; That means that if you want to use a Tree-sitter major mode, you
;; must first install the respective language grammar.  `treesit-auto'
;; is a handy package that manages this extra step for you, prompting
;; the installation of Tree-sitter grammars when necessary.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package multiple-cursors
  :ensure t
  :config)

(use-package scad-mode
  :ensure t)
