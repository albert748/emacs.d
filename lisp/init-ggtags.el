;;; init-ggtags.el --- @see https://github.com/leoliu/ggtags

;;; Commentary:

;;; Code:

(use-package ggtags
  :if (or (executable-find "gtags")
          (progn (message "[Missing] You need install gtags, try: yay -S global universal-ctags-git python-pygments") nil))

  :init
  (defun ggtags-prog-mode-hook-setup ()
    (ggtags-mode)
    ;; ggtags-find-tag-dwim
    (define-key ggtags-mode-map (kbd "M-.") nil)
    ;; ggtags-find-reference
    (define-key ggtags-mode-map (kbd "M-]") nil)
    ;; ggtags-find-tag-regexp
    (define-key ggtags-mode-map (kbd "C-M-.") nil))

  (add-hook 'prog-mode-hook #'ggtags-prog-mode-hook-setup)

  :config
  (add-to-list 'ggtags-process-environment "GTAGSLABEL=pygments")
  (setq ggtags-extra-args '("-v" "--statistics"))

  (defun ggtags-process-string-with-output (program &rest args)
    "Start gtags process asynchronously and show output in another buffer."
    (let ((output-buffer (get-buffer-create "*ggtags create tags*")))
      (with-current-buffer output-buffer
        (erase-buffer)
        (let ((proc (apply #'start-file-process program output-buffer
                           (ggtags-program-path program) args)))
          (display-buffer output-buffer)))))


  (defun ggtags-create-tags-v2 (root)
    "Create tag files (e.g. GTAGS) in directory ROOT.
If file .globalrc or gtags.conf exists in ROOT, it will be used
as configuration file per `ggtags-use-project-gtagsconf'.

If file gtags.files exists in ROOT, it should be a list of source
files to index, which can be used to speed gtags up in large
source trees. See Info node `(global)gtags' for details."
    (interactive "DRoot directory: ")
    (let ((process-environment (copy-sequence process-environment)))
      (when (zerop (length root)) (error "No root directory provided"))
      (setenv "GTAGSROOT" (ggtags-ensure-localname
                           (expand-file-name
                            (directory-file-name (file-name-as-directory root)))))
      (ggtags-with-current-project
        (let ((conf (and ggtags-use-project-gtagsconf
                         (cl-loop for name in '(".globalrc" "gtags.conf")
                                  for full = (expand-file-name name root)
                                  thereis (and (file-exists-p full) full)))))
          (unless (or conf (getenv "GTAGSLABEL")
                      (not (yes-or-no-p "Use `ctags' backend? ")))
            (setenv "GTAGSLABEL" "ctags"))
          (ggtags-with-temp-message "`gtags' in progress..."
            (let ((default-directory (file-name-as-directory root))
                  (args (append (cl-remove-if
                                 #'null
                                 (list (and ggtags-use-idutils "--idutils")
                                       (and ggtags-use-sqlite3
                                            (ggtags-process-succeed-p "gtags" "--sqlite3" "--help")
                                            "--sqlite3")
                                       (and conf "--gtagsconf")
                                       (and conf (ggtags-ensure-localname conf))))
                                ggtags-extra-args)))
              (condition-case err
                  (apply #'ggtags-process-string-with-output "gtags" args)
                (error (if (and ggtags-use-idutils
                                (stringp (cadr err))
                                (string-match-p "mkid not found" (cadr err)))
                           ;; Retry without mkid
                           (apply #'ggtags-process-string-with-output
                                  "gtags" (cl-remove "--idutils" args))
                         (signal (car err) (cdr err)))))))))
      (ggtags-invalidate-buffer-project-root (file-truename root))
      (message "GTAGS generated in `%s'" root)
      root))
  )


(provide 'init-ggtags)
;;; init-ggtags.el ends here