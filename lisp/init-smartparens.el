;;; init-smartparens.el --- see https://github.com/Fuco1/smartparens

;;; Commentary:

;; @see https://ebzzry.github.io/emacs-pairs.html#wrapping

;;; Code:

;; To enable smartparens on evil visual state, you'd enable (setq
;; evil-move-beyond-eol t, otherwise sp-forward-sexp may not work as
;; expected.
(use-package smartparens
  :diminish smartparens-mode
  :bind (:map smartparens-mode-map ("M-(" . sp-wrap-with-parentheses))

  :init
  (require 'smartparens-config)
  (smartparens-global-mode)

  :config
  ;; because `evil-smartparens-mode' default is strict, do not add it
  ;; to `smartparens-enabled-hook'
  (use-package evil-smartparens
    :diminish evil-smartparens-mode)

  (sp-use-smartparens-bindings)         ; or (sp-use-paredit-bindings)

  ;; enable additional feature which is not enabled by default
  (add-hook 'smartparens-enabled-hook #'turn-on-show-smartparens-mode)

  (defun smartparens-strict-mode-setup ()
    (evil-smartparens-mode)
    (turn-on-smartparens-strict-mode))

  ;; FIXME: trun-on trict mode for each of sp-lisp-modes
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode-setup)

  (defun sp-wrap-with-parentheses (&optional arg)
    "use `sp-wrap-with-pair' to enclose the sexp next around.
useful feature found in paredit but missing from smartparens"
    (interactive "P")
    (sp-wrap-with-pair "(")))

(provide 'init-smartparens)
;;; init-smartparens.el ends here