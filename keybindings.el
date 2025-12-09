
;;; These are just stupid keybindings ...
(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-c")

(keymap-global-set "<f5>" 'compile)
(keymap-global-set "S-<f5>" 'kill-compilation)
(keymap-global-set "<f6>" 'recompile)
(keymap-global-set "<f9>" 'query-replace)
(keymap-global-set "<f12>" 'xref-find-definitions)

(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C-c w" 'whitespace-mode)
(keymap-global-set "S-<delete>" 'kill-whole-line)
(keymap-global-set "C-+" 'text-scale-adjust)
(keymap-global-set "C--" 'text-scale-adjust)
(keymap-global-set "C-<next>" 'other-window)
(keymap-global-set "C-<prior>" 'other-window)
(keymap-global-set "C-x <up>" 'other-window)
(keymap-global-set "C-x <down>" 'other-window)

(eval-after-load 'dired
  '(progn
     (keymap-set dired-mode-map "C-x c" 'dired-create-empty-file)
     )
  )

(global-set-key (kbd "C-,") 'duplicate-line)

;; Multiple Cursors
(keymap-global-set "C-S-c C-S-c" 'mc/edit-lines)
;; Yes, its swapped, because in the most common case I dont want to have to also hold shift...
(keymap-global-set "C-<" 'mc/mark-next-like-this)
(keymap-global-set "C->" 'mc/mark-previous-like-this)
(keymap-global-set "C-S-c C-S-a" 'mc/mark-all-like-this)
;; This does not work ... it unmaps the normal ESC binding ...
;;(keymap-global-set "<escape>" 'mc/keyboard-quit)

