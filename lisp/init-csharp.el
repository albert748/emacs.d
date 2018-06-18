;;; init-csharp.el --- C# programming environment setup

;;; Commentary:
;;; refer: https://github.com/OmniSharp/omnisharp-emacs

;;; Code:

(use-package omnisharp
  :after (company flycheck)
  :init
  (defun my-csharp-mode-setup ()
    (omnisharp-mode)
    (company-mode)
    (flycheck-mode)

    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)

    ;; csharp-mode README.md recommends this too
    ;; (electric-pair-mode 1)       ;; Emacs 24
    ;; (electric-pair-local-mode 1) ;; Emacs 25
    )

  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  (add-to-list 'company-backends #'company-omnisharp))

(provide 'init-csharp)
;;; init-csharp.el ends here