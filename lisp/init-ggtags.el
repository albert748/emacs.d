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

  ;; due to Gnu global DO NOT SUPPORT compressed source files.
  ;; To enable emacs libary search (assume pwd is /usr/share/emacs)
  ;; 1. extract all *.el.gz files: find . -type f -name "*.el.gz" -exec gunzip {} \;
  ;; 2. prepare gtags.files: find . -type f -name "*.el" > gtags.files
  ;; 2. create tags files on there (may take long time): gtags -v --gtagslabel pygments
  ;; 3. do not forget to add emacs package upgrade hook and re-decompress source files.
  ;; @see https://www.gnu.org/software/global/globaldoc_toc.html#Applied-usage
  ;; FIXME: Add function for adding user-defined lib path
  (push "GTAGSLIBPATH=/usr/share/emacs" ggtags-process-environment)

  (ggtags-mode))


(provide 'init-ggtags)