;; @see https://github.com/leoliu/ggtags

(use-package ggtags
  ;; DO NOT enable gtags globally which is not usable under windows from msys2
  :if (not (eq system-type 'windows-nt))

  :defer t

  :init
  (if (executable-find "gtags")
      (progn
        (add-hook 'emacs-lisp-mode-hook 'my-ggtags-mode-setup)
        (add-hook 'python-mode-hook 'my-ggtags-mode-setup))
    (message "Your need install global, ctags, pygments to enable ggtags")))

(defun my-ggtags-mode-setup ()
  "common ggtags mode setup."

  ;; GTAGSLOGGING: Path name to the log file
  ;; FIX: ggtags use "ctags" as default backend which do not support identifier references.
  ;; gtags builtin lables include: default, native, user, ctags, new-ctags, pygments
  ;; Make sure you've installed python and python-pygments from package manager.
  (setq ggtags-process-environment (list "GTAGSLABEL=pygments"))

  (ggtags-mode))


(provide 'init-ggtags)
