(defun start-searching ()
  (interactive)
  (if (region-active-p)
      (consult-line (buffer-substring (region-beginning) (region-end)))
      (consult-line)))

(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

;; From https://stackoverflow.com/a/145359
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (handle-shift-selection)
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;;; Automatic formatting hook for SimpC Mode
(setq astyle-cmd (string-join [
"astyle"
"--style=kr"
"--align-pointer=type"
"--squeeze-ws"
"--unpad-brackets"
"--squeeze-lines=1"
"--min-conditional-indent=1"] " "))

(defun astyle-buffer ()
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     astyle-cmd
     nil
     t)
    (goto-line saved-line-number)))

(defun simpc-save-hook ()
  (when (eq major-mode 'simpc-mode)
    (message "Applying Astyle formatting.")
    (astyle-buffer)))

;;(add-hook 'before-save-hook 'simpc-save-hook)

(defvar eglot-modes
  (list 'c++-mode 'c-mode 'c-ts-mode 'c++-ts-mode)
  "Modes for which to start eglot and apply eglot-format in the before-save-hook.")

(defun eglot-modes-save-hook ()
  (when (member major-mode eglot-modes)
    (message "[eglot-modes-save-hook] Invoking eglot-format")
    (eglot-format)))

(defun todo-grep ()
  (interactive)
  (grep (concat grep-command
                " --exclude-dir .git"
                " --exclude-dir imgui"
                " --include=\\*.h"
                " --include=\\*.c"
                " --include=\\*.cpp"
                " TODO")))
