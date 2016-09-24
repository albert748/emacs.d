(push 'ggtags melpa-include-packages)
(require-package 'ggtags)

;; lisp setup
(defun ggtags-mode-init ()
  "setup ggtags mode for available modes."
  (with-eval-after-load 'ggtags
    (if evil-normal-state-map
        ;; FIX: remove default key binding for `evil-repeat-pop-next' from evil-mode if exist
        (define-key evil-normal-state-map (kbd "M-.") nil))
    (when (executable-find (ggtags-program-path "global"))
      (ggtags-mode))))

(add-hook 'emacs-lisp-mode-hook 'ggtags-mode-init)

(provide 'init-ggtags)