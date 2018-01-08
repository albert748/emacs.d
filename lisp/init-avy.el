(use-package avy
  ;; {{{ avy, jump between texts, like easymotion in vim, replace ace-jump-mode
  ;; @see http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/ for more tips
  ;; @see source code: https://github.com/abo-abo/avy
  :bind ("C-:" . avy-goto-char-2)

  ;; setup the default shortcuts: bind avy-isearch to C-'
  :init (avy-setup-default)

  :config
  ;; enable chinese pinyin jump support
  ;; @see https://github.com/cute-jumper/ace-pinyin
  (use-package ace-pinyin
    :init (ace-pinyin-mode))

  (use-package dired
    :ensure nil
    :bind (:map dired-mode-map
                (";" . avy-goto-char-2)))

  (use-package ibuffer
    :ensure nil
    :bind (:map ibuffer-mode-map
                (";" . avy-goto-char-2))))
;; }}}

;; {{{ ace-link
;; (ace-link-setup-default)
;; (global-set-key (kbd "M-o") 'ace-link-addr)
;; ;; }}}
(use-package ace-link
  :init (ace-link-setup-default))

;; @see https://github.com/abo-abo/ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window))

(provide 'init-avy)
;;; init-avy.el ends here
