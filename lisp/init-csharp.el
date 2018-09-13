;;; init-csharp.el --- C# programming environment setup

;;; Commentary:
;;; refer: https://github.com/OmniSharp/omnisharp-emacs

;;; Code:

(use-package omnisharp
  :init
  (defun my-csharp-mode-setup ()
    (omnisharp-mode)
    (company-mode)
    (flycheck-mode)

    (setq projectile-indexing-method 'alien)

    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)

    (eval-after-load 'company
      '(setq-local company-backends (cons #'company-omnisharp company-backends)))

    ;; csharp-mode README.md recommends this too
    ;; (electric-pair-mode 1)       ;; Emacs 24
    ;; (electric-pair-local-mode 1) ;; Emacs 25
    )

  (add-hook 'csharp-mode-hook #'my-csharp-mode-setup))

(provide 'init-csharp)
;;; init-csharp.el ends here