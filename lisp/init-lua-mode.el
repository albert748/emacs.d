;;; init-lua-mode.el --- lua mode setups

;;; Commentary:

;;; Code:

(defun my-lua-mode-setup ()
  (interactive)
  (unless (is-buffer-file-temp)
    (setq-local imenu-generic-expression '(("Variable" "^ *\\([a-zA-Z0-9_.]+\\) *= *{ *[^ ]*$" 1)
                                           ("Function" "function +\\([^ (]+\\).*$" 1)
                                           ("Module" "^ *module +\\([^ ]+\\) *$" 1)
                                           ("Variable" "^ *local +\\([^ ]+\\).*$" 1)))))

;; @see http://lua-users.org/wiki/LuaStyleGuide
;; others use indent 2 spaces by default, to make it more pythonic, use 4 instead.
(setq-default lua-indent-level 4)

(add-hook 'lua-mode-hook 'my-lua-mode-setup)

(provide 'init-lua-mode)
;;; init-lua-mode.el ends here