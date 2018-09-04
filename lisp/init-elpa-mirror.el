;;; init-elpa-mirror.el -- elpa-mirror initialization

;;; Commentary:

;;; Code:

(use-package elpa-mirror
  :config
  (defun my-elpamr-create ()
    (interactive)
    (elpamr-create-mirror-for-installed (file-name-as-directory (expand-file-name "elpa-mirror" my-emacs-private-directory))))

  (defun my-elpamr-extract ()
    (interactive)
    (let ((package-archives
           `(("elpamr" . ,(file-name-as-directory (expand-file-name "elpa-mirror" my-emacs-private-directory))))))
      (package-list-packages)
      (package-menu-mark-upgrades)
      (package-menu-execute t))))


(provide 'init-elpa-mirror)
;;; init-elpa-mirror.el ends here