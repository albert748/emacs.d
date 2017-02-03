;; @see https://github.com/Fuco1/smartparens
;; @see https://ebzzry.github.io/emacs-pairs.html#wrapping

(use-package smartparens
  :init (require 'smartparens-config)
  :config
  ;; set default set of key bindings: paredit or smartparens style
  ;; (sp-use-paredit-bindings)
  (sp-use-smartparens-bindings)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode))

(provide 'init-smartparens)