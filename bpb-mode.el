;; Mode for .bpb files (Buschla Parser Bindings)

(defvar bpb-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?#  ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defun bpb-match-binding (limit)
   (let ((case-fold-search t))
     (re-search-forward "^\\(binding\\) *\\([[:alpha:]_-]*\\)" limit t)))

(defun bpb-match-number (limit)
   (let ((case-fold-search t))
     (re-search-forward "\\(number\\)" limit t)))

(defun bpb-font-lock-keywords ()
  `(
    (,'bpb-match-binding . ((1 font-lock-keyword-face) (2 font-lock-constant-face)))
    (,'bpb-match-number . (1 font-lock-warning-face))
   ))

(define-derived-mode bpb-mode prog-mode "Bpb"
  "Major Mode for editing .bpb files"
  :syntax-table bpb-mode-syntax-table
  (setq-local font-lock-defaults '(bpb-font-lock-keywords))
  (setq-local comment-start "### ")
  (setq-local comment-end "")
  )

(provide 'bpb-mode)
