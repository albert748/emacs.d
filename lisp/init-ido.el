;;; init-ido.el --- init ido for minibuffer

;;; Commentary:

;;; Code:

(eval-when-compile (defvar my-emacs-cache-directory))

(use-package ido
  :ensure nil                           ; built-in pacakge
  :init
  (setq ido-save-directory-list-file (expand-file-name "ido.last" my-emacs-cache-directory))

  ;; However, if ARG arg equals ‘files’, remap only commands for
  ;; files, or if it equals ‘buffers’, remap only commands for buffer
  ;; switching. Here we remap keybindings for both 'find-files' and
  ;; 'switch-to-buffer' families of commands
  (ido-mode t)

  :config
  (setq ido-enable-flex-matching t)
  (setq ido-use-virtual-buffers t)

  ;; Allow the same buffer to be open in different frames
  (setq ido-default-buffer-method 'selected-window)

  ;; @see https://github.com/lewang/flx
  (use-package flx-ido
    :init (flx-ido-mode 1)
    :config
    (setq flx-ido-threshold 10000)

    ;; select between ido faces or flx highlights.
    ;; (setq flx-ido-use-faces nil)
    (setq ido-use-faces nil))

  ;; disable ido for certain commands,
  ;; @see http://stackoverflow.com/questions/6771664/disable-ido-mode-for-specific-commands
  (defadvice ido-read-buffer (around ido-read-buffer-possibly-ignore activate)
    "Check to see if use wanted to avoid using ido"
    (if (eq (get this-command 'ido) 'ignore)
        (let ((read-buffer-function nil))
          (run-hook-with-args 'ido-before-fallback-functions 'read-buffer)
          (setq ad-return-value (apply 'read-buffer (ad-get-args 0))))
      ad-do-it))
  (put 'shell 'ido 'ignore)
  (put 'ffap-alternate-file 'ido 'ignore)
  (put 'tmm-menubar 'ido 'ignore)
  (put 'dired-do-copy 'ido 'ignore)
  (put 'dired-do-rename 'ido 'ignore)
  (put 'vc-copy-file-and-rename-buffer 'ido 'ignore)
  (put 'dired-create-directory 'ido 'ignore)
  (put 'copy-file-and-rename-buffer 'ido 'ignore)
  (put 'rename-file-and-buffer 'ido 'ignore)
  (put 'w3m-goto-url 'ido 'ignore)
  (put 'ido-find-file 'ido 'ignore)
  (put 'ido-edit-input 'ido 'ignore)
  (put 'read-file-name 'ido 'ignore)
  (put 'dired-create-directory 'ido 'ignore)
  (put 'minibuffer-completion-help 'ido 'ignore)
  (put 'minibuffer-complete 'ido 'ignore)
  (put 'c-set-offset 'ido 'ignore)
  (put 'rgrep 'ido 'ignore)
  (put 'dired-create-directory 'ido 'ignore)

  (defun ido-imenu ()
    "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
    (interactive)
    (unless (featurep 'imenu)
      (require 'imenu nil t))
    (imenu--make-index-alist)
    (let ((name-and-pos '())
          (symbol-names '()))
      (cl-flet ((addsymbols (symbol-list)
                            (when (listp symbol-list)
                              (dolist (symbol symbol-list)
                                (let ((name nil) (position nil))
                                  (cond
                                   ((and (listp symbol) (imenu--subalist-p symbol))
                                    (addsymbols symbol))

                                   ((listp symbol)
                                    (setq name (car symbol))
                                    (setq position (cdr symbol)))

                                   ((stringp symbol)
                                    (setq name symbol)
                                    (setq position (get-text-property 1 'org-imenu-marker symbol))))

                                  (unless (or (null position) (null name))
                                    (add-to-list 'symbol-names name)
                                    (add-to-list 'name-and-pos (cons name position))))))))
        (addsymbols imenu--index-alist))
      ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
      (let ((symbol-at-point (thing-at-point 'symbol)))
        (when symbol-at-point
          (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
                 (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                       (if (string-match regexp symbol) symbol))
                                                     symbol-names))))
            (when matching-symbols
              (sort matching-symbols (lambda (a b) (> (length a) (length b))))
              (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                    matching-symbols)))))
      (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (push-mark (point))
        (goto-char position)))))


(provide 'init-ido)
;;; init-ido.el ends here