;;; init-sessions.el --- initialize desktop and session

;;; Commentary:

;;; Code:

(eval-when-compile (defvar my-emacs-cache-directory))

(use-package desktop
  :ensure nil

  :init (desktop-save-mode)

  :config
  (setq desktop-base-file-name "emacs.desktop"
        desktop-base-lock-name "emacs.desktop.lock"
        desktop-path (list my-emacs-cache-directory))
  ;; (add-to-list 'desktop-path my-emacs-cache-directory)
  (setq desktop-save 'if-exists)
  ;; @see https://github.com/purcell/emacs.d/issues/352
  ;; (setq desktop-restore-frames nil)
  (setq desktop-restore-in-current-display nil)
  ;; save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
        (append '((extended-command-history . 128)
                  (file-name-history        . 128)
                  (ido-last-directory-list  . 128)
                  (ido-work-directory-list  . 128)
                  (ido-work-file-list       . 128)
                  (grep-history             . 128)
                  (compile-history          . 128)
                  (minibuffer-history       . 128)
                  (query-replace-history    . 128)
                  (read-expression-history  . 128)
                  (regexp-history           . 128)
                  (regexp-search-ring       . 128)
                  (search-ring              . 128)
                  (comint-input-ring        . 128)
                  (shell-command-history    . 128)
                  (evil-ex                  . 128)
                  desktop-missing-file-warning
                  register-alist))))


;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(use-package session
  :ensure nil

  :init (add-hook 'after-init-hook 'session-initialize)

  :config
  (setq session-save-file (expand-file-name "emacs.session" my-emacs-cache-directory))

  )

(provide 'init-sessions)
;;; init-sessions.el ends here
