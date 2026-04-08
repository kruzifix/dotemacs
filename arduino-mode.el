; https://www.reddit.com/r/emacs/comments/17n4ti6/how_to_configure_arduino_lsp/
(require 'eglot)
(define-derived-mode arduino-mode c-mode "arduino"
      "Wrapper for c-mode for editing arduino files.")
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
(add-hook 'arduino-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs
             '(arduino-mode . ("arduino-language-server"
                               "-clangd" "/usr/bin/clangd"
                               "-cli" "/usr/local/bin/arduino-cli"
                               "-cli-config" "~/.arduino15/arduino-cli.yaml"
                               "-fqbn" "esp32:esp32:esp32")))
