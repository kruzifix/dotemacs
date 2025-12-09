(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(load-file (expand-file-name "packages.el" user-emacs-directory))
(load-file (expand-file-name "funcs.el" user-emacs-directory))
(load-file (expand-file-name "keybindings.el" user-emacs-directory))

(load-file custom-file)

; Stuff from https://github.com/mgmarlow/start-emacs
;; No auto-resizing of frames.
(setq frame-inhibit-implied-resize t)
;; Bell sounds from my text editor are no fun.
(setq ring-bell-function #'ignore)
;; Auto-revert dired (the directory editor) when revisiting
;; directories, since they may have changed underneath.
(setq dired-auto-revert-buffer t)
;; Tab does quite a bit of stuff in Emacs, so it's helpful to have it
;; attempt completions when it's not doing something else.
(setq tab-always-indent 'complete)
;; Scroll Eshell to the bottom when new output is added.
(setq eshell-scroll-to-bottom-on-input 'this)
;; Make certain mouse commands more intuitive.
(setq mouse-yank-at-point t)

;; Backup files
(let ((backups-dir (expand-file-name "backups" user-emacs-directory)))
  (setq backup-directory-alist `(("." . ,backups-dir)))
  (unless (file-exists-p backups-dir)
    (make-directory backups-dir)))

(setq backup-by-copying t
      version-control t
      kept-new-versions 6
      kept-old-version 2)

;; Autosave files
(let ((auto-saves-dir (expand-file-name "autosaves/" user-emacs-directory)))
  (setq auto-save-file-name-transforms `((".*" ,auto-saves-dir t)))
  (unless (file-exists-p auto-saves-dir)
    (make-directory auto-saves-dir)))

(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)

;; These are super annoying ...
(setq create-lockfiles nil)

;; Emacs Lisp files can be byte-compiled into `.elc' files, which run
;; faster.  By default Emacs prefers `.elc' to `.el' in all cases,
;; causing occasional annoyances if you make a change to an Emacs Lisp
;; file but forget to byte-compile it.  `load-prefer-newer' always
;; prefers the last-edited file, preventing this problem.
(setq load-prefer-newer t)
;; Automatically retain a final newline when saving a file.
(setq require-final-newline t)
;; Avoid tabs when possible.
(setq-default indent-tabs-mode nil)
;; Turn off the default Emacs UI elements, drill those keybindings
;; instead!  You can use "C-h C-q" to pull up a quick-reference sheet
;; that will help you remember the basics.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Respect color escape sequences.  Particularly useful for "M-x
;; compile" with modern programming languages that use colors to
;; convey information.
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; Theme
(load-theme 'gruber-darker :no-confirm)
(global-display-line-numbers-mode)
(global-hl-line-mode)
;;(set-face-background 'hl-line nil)
;;(set-face-foreground 'hl-line nil)
;;(set-face-underline  'hl-line t)
(which-key-mode)
(which-function-mode)

;; Whitespace
(global-whitespace-mode)
(add-hook 'magit-mode-hook
          (lambda () (whitespace-mode -1)))

(global-completion-preview-mode)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'before-save-hook 'eglot-format)

;;; SimpC Mode
;;(load-file (expand-file-name "simpc-mode.el" user-emacs-directory))
;;(require 'simpc-mode)
;;(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;;; Font
(set-face-attribute 'default nil :font "Monospace" :height 160)

;;; Defaults
(setq-default inhibit-splash-screen t
              tab-width 4
              c-basic-offset 4
              cursor-type 'bar
              compilation-scroll-output 'first-error
              mouse-wheel-progressive-speed nil
              which-func-mode t)

;;; Remove title bar
(setq default-frame-alist '((undecorated . t)))

;;; Delete selection when starting to type
(delete-selection-mode 1)

;;; IDO
(ido-mode 1)
(ido-everywhere 1)

;; For 'normal' copy, paste, cut and undo bindings
;;(cua-mode)

;;; Grep Command TODO: REVISIT
(setq grep-command "grep -rnH")



