;;; init-projectile.el -- projectile-mode initialization

;;; Commentary:

;;; Code:

(use-package projectile
  :init
  (defun my-projectile-enable ()
    (projectile-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (add-hook 'prog-mode-hook #'my-projectile-enable)
  (add-hook 'org-mode-hook #'my-projectile-enable))

(provide 'init-projectile)
;;; init-projectile.el ends here