(use-package flymake
  :disabled
  :ensure nil

  :init
  (defun flymake-mode-setup ()
    (flymake-mode))

  (add-hook 'emacs-lisp-mode-hook #'flymake-mode-setup)
  (add-hook 'lua-mode-hook #'flymake-mode-setup)
  (add-hook 'ruby-mode-hook #'flymake-mode-setup)

  :config
  ;; @see http://blog.urth.org/2011/06/02/flymake-versus-the-catalyst-restarter/
  (defun flymake-create-temp-intemp (file-name prefix)
    "Return file name in temporary directory for checking
   FILE-NAME. This is a replacement for
   `flymake-create-temp-inplace'. The difference is that it gives
   a file name in `temporary-file-directory' instead of the same
   directory as FILE-NAME.

   For the use of PREFIX see that function.

   Note that not making the temporary file in another directory
   \(like here) will not if the file you are checking depends on
   relative paths to other files \(for the type of checks flymake
   makes)."
    (unless (stringp file-name)
      (error "Invalid file-name"))
    (or prefix
        (setq prefix "flymake"))
    (let* ((name (concat
                  (file-name-nondirectory
                   (file-name-sans-extension file-name))
                  "_" prefix))
           (ext  (concat "." (file-name-extension file-name)))
           (temp-name (make-temp-file name nil ext))
           )
      (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name))

  ;; do not use elisplint
  (defun flymake-elisp-init ()
    (unless (or (string-match "^ " (buffer-name)) (is-buffer-file-temp))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-intemp))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list
         (expand-file-name invocation-name invocation-directory)
         (list
          "-Q" "--batch" "--eval"
          (prin1-to-string
           (quote
            (dolist (file command-line-args-left)
              (with-temp-buffer
                (insert-file-contents file)
                (condition-case data
                    (scan-sexps (point-min) (point-max))
                  (scan-error
                   (goto-char(nth 2 data))
                   (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                  file (line-number-at-pos)))))))
            ))
          local-file)))))

  (push '("\\.el$" flymake-elisp-init) flymake-proc-allowed-file-name-masks))


(provide 'init-flymake)
;;; init-flymake.el ends here