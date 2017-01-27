;; {{ avy, jump between texts, like easymotion in vim
;; @see http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/ for more tips
;; dired
(use-package avy
  :commands avy-goto-subword-1

  :config
  (eval-after-load "dired"
    '(progn
       (define-key dired-mode-map (kbd ";") 'avy-goto-subword-1))))
;; }}

;; {{ ace-link
;; (ace-link-setup-default)
;; (global-set-key (kbd "M-o") 'ace-link-addr)
;; ;; }}
(use-package ace-link
  :config (ace-link-setup-default))

;; https://github.com/abo-abo/ace-window
;; `M-x ace-window ENTER m` to swap window
;; (global-set-key (kbd "C-x o") 'ace-window)

;;; FIXME: ace-window depends on avy, the replacement of `ace-jump-mode', which is more advanced for daily usage and should be customized for emacs editing mode instead of evil.
(use-package ace-window
  :bind ("C-x o" . ace-window))


(provide 'init-avy)
