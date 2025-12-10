;; Mode for .cfg files (like the ones used by idStudio/Mötör)

(defvar cfg-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?/  ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defun cfg-font-lock-keywords ()
  '(("^\\([[:alpha:]_]+\\) *\\([[:alpha:][:digit:]._/\\\"]*\\)" . ((1 font-lock-keyword-face) (2 font-lock-constant-face)))
    ("\\(-[[:alpha:]]+\\)" . (1 font-lock-warning-face))
   ))

(define-derived-mode cfg-mode prog-mode "Cfg"
  "Major Mode for editing .cfg files"
  :syntax-table cfg-mode-syntax-table
  (setq-local font-lock-defaults '(cfg-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  )

(provide 'cfg-mode)
