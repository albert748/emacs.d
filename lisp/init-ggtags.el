;; @see https://github.com/leoliu/ggtags

(use-package ggtags
  ;; DO NOT enable gtags as global is not usable under windows from msys2
  :if (not (eq system-type 'windows-nt))

  :init
  (unless (executable-find "gtags")
    (message "Your need install global, ctags, pygments to enable ggtags"))

  :config
  (defun ggtags-elisp-mode-hook-setup ()
    "setup ggtags for elisp mode."
    (ggtags-mode)

    (if evil-normal-state-map
        ;; FIX: remove default key binding for `evil-repeat-pop-next' from evil-mode if exist
        (define-key evil-normal-state-map (kbd "M-.") nil))

    ;; GTAGSLOGGING: Path name to the log file
    ;; FIX: ggtags use "ctags" as default backend which do not support identifier references.
    ;; gtags builtin lables include: default, native, user, ctags, new-ctags, pygments
    ;; Make sure you've installed python and python-pygments from package manager.
    (setq-local ggtags-process-environment (list "GTAGSLABEL=pygments")))


  (add-hook 'emacs-lisp-mode-hook 'ggtags-elisp-mode-hook-setup))


(provide 'init-ggtags)
