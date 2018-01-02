(use-package grep
  :ensure nil
  :defer t

  :config
  ;; It's more clear to see the progress of grep process
  (setq grep-scroll-output t)

  ;; FIXME: scroll to first-error after grep process return

  ;; FIXME: add line filter advice function for long line.

  (dolist (v '("auto" "target" "node_modules" "bower_components" ".sass_cache" ".git" ".cvs" ".svn" ".hg"))
    (cl-pushnew v grep-find-ignored-directories))

  (dolist (v '("company-statistics-cache.el"))
    (cl-pushnew v grep-find-ignored-files)))


(provide 'init-grep)
;;; init-grep.el ends here
