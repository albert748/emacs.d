;; make sure use-package rounded packages always installed
(setq use-package-always-ensure t)

(defun package-install-silent (func pkg &optional dont-select)
  "Install package silently, without asking yes or no if processes need be killed."
  (remove-hook 'kill-buffer-query-functions 'process-kill-buffer-query-function)
  (funcall func pkg dont-select)
  (add-hook 'kill-buffer-query-functions 'process-kill-buffer-query-function))
(advice-add 'package-install :around #'package-install-silent)

(provide 'init-use-package)
