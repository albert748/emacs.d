;; @see https://github.com/Fuco1/smartparens
;; @see https://ebzzry.github.io/emacs-pairs.html#wrapping

;; To enable smartparens on evil visual state, you'd enable (setq
;; evil-move-beyond-eol t), otherwise sp-forward-sexp may not work as
;; expected.
(use-package smartparens
  :init (require 'smartparens-config)
  :config
  (add-hook 'smartparens-enabled-hook #'show-smartparens-mode)

  ;; set default set of key bindings: paredit or smartparens style
  ;; (sp-use-paredit-bindings)
  (sp-use-smartparens-bindings)

  ;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

  (use-package evil-smartparens
    :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

(provide 'init-smartparens)