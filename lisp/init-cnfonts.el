(use-package cnfonts
  :config
  (setq cnfonts-use-system-type t)

  (setq cnfonts-directory (concat my-emacs-private-directory "/cnfonts/"))

  ;; use separate config file to avoid confliction
  (if (eq system-type 'darwin)
      (setq cnfonts-config-filename "cfs-darwin.conf"))

  (cnfonts-enable))

(provide 'init-cnfonts)