;;; init-lua-mode.el --- lua mode setups

;;; Commentary:

;;; Code:


(use-package lua-mode
  :init
  (defun my-lua-mode-setup ()
    (unless (is-buffer-file-temp)
      (setq-local imenu-generic-expression '(("Variable" "^ *\\([a-zA-Z0-9_.]+\\) *= *{ *[^ ]*$" 1)
                                             ("Function" "function +\\([^ (]+\\).*$" 1)
                                             ("Module" "^ *module +\\([^ ]+\\) *$" 1)
                                             ("Variable" "^ *local +\\([^ ]+\\).*$" 1))))

    ;; @see http://lua-users.org/wiki/LuaStyleGuide
    ;; others use indent 2 spaces by default, to make it more pythonic, use 4 instead.
    (setq-local lua-indent-level 4))

  (add-hook 'lua-mode-hook 'my-lua-mode-setup))


(use-package company-lua
  :init
  (defun my-lua-setup-company ()
    (setq-local company-backends (cons '(company-lua
                                         company-gtags
                                         company-dabbrev-code
                                         company-keywords
                                         company-etags) company-backends)))
  (add-hook 'lua-mode-hook 'my-lua-setup-company))


(provide 'init-lua-mode)
;;; init-lua-mode.el ends here