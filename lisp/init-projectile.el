;;; init-projectile.el -- projectile-mode initialization

;;; Commentary:

;;; Code:

(eval-when-compile (defvar my-emacs-cache-directory))

(use-package projectile
  :init
  (defun my-projectile-enable ()
    (projectile-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (add-hook 'prog-mode-hook #'my-projectile-enable)
  (add-hook 'org-mode-hook #'my-projectile-enable)

  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" my-emacs-cache-directory))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-emacs-cache-directory)))


(provide 'init-projectile)
;;; init-projectile.el ends here