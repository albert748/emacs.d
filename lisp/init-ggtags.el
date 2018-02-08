;;; init-ggtags.el --- @see https://github.com/leoliu/ggtags

;;; Commentary:

;;; Code:

(use-package ggtags
  :if (if (executable-find "gtags")
          t
        (message "You need install gtags(also universal ctags and pygements) to enable ggtags.el package")
        nil)

  :init
  (defcustom ggtags-default-gtagslabel 'pygments
    "the default label used to generate gtags index.
The value passed as --gtagslabel option to gtags command. The
value maybe default, native, user, ctags, new-ctags, pygments,
see man page of gtags.conf.")

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

  (defun ggtags-mode-elisp-setup ()
    "ggtags mode setup for lisp"

    ;; because to Gnu global DO NOT SUPPORT compressed source files.

    ;; 1. you need decompress gziped source files under
    ;; /usr/share/emacs (or just create package for your own by
    ;; --without-compress-install)

    ;; 2. create tags file under emacs install path above (again,
    ;; create this step from your own package, oh, God thanks to
    ;; Arch!)

    ;; 3. with this GTAGSLIBPATH variable, you can find all definitions from within emacs.

    ;; ctags and universal-ctags can't find references, use pygments instead.
    (setq ggtags-process-environment (list "GTAGSLIBPATH=/usr/share/emacs" "GTAGSLABEL=pygments"))

    (ggtags-mode))

  (defun ggtags-mode-python-setup ()
    (setq ggtags-process-environment (list "GTAGSLABEL=pygments"))
    (ggtags-mode))

  (add-hook 'emacs-lisp-mode-hook #'ggtags-mode-elisp-setup)
  (add-hook 'python-mode-hook #'ggtags-mode-python-setup))


(provide 'init-ggtags)
;;; init-ggtags.el ends here