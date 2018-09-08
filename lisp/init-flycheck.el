;;; init-flycheck.el --- see http://www.flycheck.org/

;;; Commentary:

;;; Code:

(use-package flycheck
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode-on-safe)

  :config
  (defun flycheck-mode-org-setup ()
    (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'org-src-mode-hook #'flycheck-mode-org-setup))


(provide 'init-flycheck)
;;; init-flycheck.el ends here