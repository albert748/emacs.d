;;; init-emms.el --- see https://www.gnu.org/software/emms/manual/

;;; Commentary:

;; emms-source-file-default-directory

;;; Code:

(use-package emms
  :defer t
  :config
  (emms-all)

  ;; Because mpg321 do not support mp3 seek, we do not use
  ;; `emms-default-players'
  (setq emms-player-list '(emms-player-vlc)))

(provide 'init-emms)
;;; init-emms.el ends here