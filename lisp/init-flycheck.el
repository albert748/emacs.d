;;; init-flycheck.el --- see http://www.flycheck.org/

;;; Commentary:

;;; Code:

(use-package flycheck
  :init (global-flycheck-mode)

  :config
  (defun flycheck-mode-org-setup ()
    (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'org-src-mode-hook #'flycheck-mode-org-setup))


(provide 'init-flycheck)
;;; init-flycheck.el ends here