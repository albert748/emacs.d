(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------
(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defun elisp-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; (enable-paredit-mode)
    (rainbow-delimiters-mode t)
    (set-up-hippie-expand-for-elisp)
    (checkdoc-minor-mode)))

(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook-setup)

(use-package elisp-mode
  :ensure nil                           ; built-in package
  :defer t
  :config
  ;; eldoc will be setup automatically by emacs
  ;; (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-idle-delay 0.2))

(use-package elisp-def
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-def-mode)))

(provide 'init-elisp)
