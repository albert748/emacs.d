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
    (setq-local lua-indent-level 2))

  (add-hook 'lua-mode-hook 'my-lua-mode-setup))

(provide 'init-lua-mode)
;;; init-lua-mode.el ends here