(push 'ggtags melpa-include-packages)
(require-package 'ggtags)

;; lisp setup
(defun ggtags-mode-init ()
  (with-eval-after-load 'ggtags
    (when (executable-find (ggtags-program-path "global"))
      (ggtags-mode))))

(add-hook 'emacs-lisp-mode-hook 'ggtags-mode-init)

(provide 'init-ggtags)