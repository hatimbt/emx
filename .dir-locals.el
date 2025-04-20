;;; Directory Local Variables         -*- no-byte-compile: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((fundamental-mode . ((eval . (when (string-match "\\elpaca.lock$" (buffer-file-name))
                                (emacs-lisp-mode))))))
