;;; init-elfeed.el --- https://github.com/skeeto/elfeed

;;; Commentary:

;;; Code:

(use-package elfeed
  :init

  :config

  ;; @see https://github.com/algernon/elfeed-goodies
  ;; (use-package elfeed-goodies
  ;;   :init (elfeed-goodies/setup))

  ;; @see https://github.com/remyhonig/elfeed-org
  (use-package elfeed-org
    :init
    (elfeed-org)

    :config
    (setq rmh-elfeed-org-files
          (list (expand-file-name "elfeed.org" (getenv "MY_EMACS_ORG_DIRECTORY"))))))


(provide 'init-elfeed)
;;; init-elfeed.el ends here
