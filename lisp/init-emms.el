;;; init-emms.el --- see https://www.gnu.org/software/emms/manual/

;;; Commentary:

;; emms-source-file-default-directory

;;; Code:

(use-package emms
  :defer t
  :config
  (emms-all)

  ;; mpg321 do not support mp3 seek, so do not use the default
  ;; `emms-default-players'

  (if (executable-find "vlc")
      (setq emms-player-list '(emms-player-vlc))
    (message "You need install vlc for EMMS to enable this player"))

  (defun emms-track-simple-description-nondirectory (track)
    "Shorter track name for file type, without directory."

    (let ((type (emms-track-type track)))
      (if (eq 'file type)
          (file-name-nondirectory (emms-track-name track))
        (funcall emms-track-simple-description track))))

  (defun emms-mode-line-icon-nondirectory ()
    "Show emms track info on mode line without directory.

see also `emms-mode-line-icon-function'"
    (concat " "
          emms-mode-line-icon-before-format
          (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (format emms-mode-line-format (emms-track-simple-description-nondirectory (emms-playlist-current-selected-track)))))

  ;; mode line without directory for file type track
  (setq emms-mode-line-mode-line-function 'emms-mode-line-icon-nondirectory))

(provide 'init-emms)
;;; init-emms.el ends here