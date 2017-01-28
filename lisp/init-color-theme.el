;;; @see https://www.emacswiki.org/emacs/ColorThemes

;; Most downloaded theme from melpa use builtin custom theme support
(use-package custom
  :ensure nil                           ; built-in package
  :config
  (use-package zenburn-theme
    :config (load-theme 'zenburn t)))

(use-package color-theme
  :disabled t
  :config
  (use-package molokai-theme
    :config (color-theme-molokai))

  (defvar my-current-color-theme nil
    "My current color theme.")

  (defun my-toggle-color-theme ()
    "Toggle between the major color theme and fallback theme.
Fallback theme is used only if the console does NOT support 256 colors."
    (interactive)
    (cond
     ((string= my-current-color-theme "molokai")
      ;; fallback color theme from color-theme library
      (unless color-theme-initialized (color-theme-initialize))
      (color-theme-deep-blue)
      (setq my-current-color-theme "fallback"))
     (t
      ;; major color theme we use
      (unless (featurep 'color-theme-molokai)
        (require 'color-theme-molokai))
      (color-theme-molokai)
      (setq my-current-color-theme "molokai"))))

  ;; {{ work around color theme bug
  ;; @see https://plus.google.com/106672400078851000780/posts/KhTgscKE8PM
  (defadvice load-theme (before disable-themes-first activate)
    ;; diable all themes
    (dolist (i custom-enabled-themes)
      (disable-theme i)))
  ;; }}

  ;; turn on the color theme now!
  ;; (my-toggle-color-theme)

  ;; This line must be after color-theme-molokai! Don't know why.
  (setq color-theme-illegal-faces "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock\\|dired-directory\\)"))


(provide 'init-color-theme)
