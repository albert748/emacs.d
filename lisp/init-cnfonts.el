;;; init-cnfonts.el --- see https://github.com/tumashu/cnfonts

;;; Commentary:

;;; Code:

(use-package cnfonts
  :init
  (cnfonts-enable)

  :config
  (setq cnfonts-directory (expand-file-name "cnfonts/" my-emacs-private-directory))
  (setq cnfonts-use-system-type t)
  )

(provide 'init-cnfonts)
;;; init-cnfonts.el ends here