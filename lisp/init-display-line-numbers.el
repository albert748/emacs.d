;;; init-display-line-numbers.el --- initialize display-line-numbers-mode

;;; Commentary:

;;; Code:

(use-package display-line-numbers
  :ensure nil
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'org-mode-hook #'display-line-numbers-mode)

  :config
  ;; what's the difference between 'relative and 'visual?
  (setq display-line-numbers-type 'visual))

(provide 'init-display-line-numbers)
;;; init-display-line-numbers ends here