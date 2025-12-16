;; QoL
(keymap-global-unset "C-x C-c")

(keymap-global-set "C-z" 'undo)
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C-c w" 'whitespace-mode)
(keymap-global-set "C-+" 'text-scale-adjust)
(keymap-global-set "C--" 'text-scale-adjust)

;; F-Keys
(keymap-global-set "<f5>" 'compile)
(keymap-global-set "S-<f5>" 'kill-compilation)
(keymap-global-set "<f6>" 'recompile)
(keymap-global-set "<f7>" 'next-error)
(keymap-global-set "S-<f7>" 'previous-error)
(keymap-global-set "<f9>" 'query-replace)
(keymap-global-set "<f12>" 'xref-find-definitions)

;; Text Manipulation
(keymap-global-set "S-<delete>" 'kill-whole-line)
(keymap-global-set "C-," 'duplicate-line)

;; Navigation
(keymap-global-set "C-<next>" 'other-window)
(keymap-global-set "C-<prior>" 'other-window)
(keymap-global-set "C-x <up>" 'other-window)
(keymap-global-set "C-x <down>" 'other-window)
(keymap-global-set "<mouse-8>" 'previous-buffer)
(keymap-global-set "<mouse-9>" 'next-buffer)
(keymap-global-set "C-a" 'smart-beginning-of-line)
(keymap-global-set "<home>" 'smart-beginning-of-line)
(keymap-global-set "C-s" 'start-searching)
(keymap-global-set "C-d" 'goto-matching-paren)

;; Dired
(eval-after-load 'dired
  '(progn
     (keymap-set dired-mode-map "C-x c" 'dired-create-empty-file)
     )
  )

;; Multiple Cursors
(keymap-global-set "C-S-c C-S-c" 'mc/edit-lines)
;; Yes, its swapped, because in the most common case I dont want to have to also hold shift...
(keymap-global-set "C-<" 'mc/mark-next-like-this)
(keymap-global-set "C->" 'mc/mark-previous-like-this)
(keymap-global-set "C-S-c C-S-a" 'mc/mark-all-like-this)
