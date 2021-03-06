;; -*- coding: utf-8 -*-
;(defvar best-gc-cons-threshold gc-cons-threshold "Best default gc threshold value. Should't be too big.")


;;; Code:

;; timestamp messages
;; refer (with modifications): https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun my-current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

(defun my-ad-timestamp-message (format-string &rest args)
  "Advice to run before `message' that prepend a timestamp to each message.

format the FORMAT-STRING with ARGS.

Activate this advice with: (advice-add 'message :before 'my-ad-timestamp-message)"
  (unless (not message-log-max)
    (unless (or (not format-string)
                (string= format-string "")
                (string= (apply #'format-message format-string args) ""))
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (my-current-time-microseconds) " "))))))

(advice-add 'message :before 'my-ad-timestamp-message)

;; org directory setup
(defvar my-emacs-cache-directory
  (or (getenv "MY_EMACS_CACHE_DIRECTORY")
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))
  "Global cache directory for Emacs, used to store cache files.

useful for chinese-pyim cache, keyfreq lock file etc.")

(defvar my-emacs-private-directory
  (or (getenv "MY_EMACS_PRIVATE_DIRECTORY")
      (expand-file-name "emacs/" (or (getenv "XDG_CONFIG_HOME") "~/.config/")))
  "Global personal configuration directory for Emacs.

used to store sensitive personal data like input method user
dict.")

(message "Use %s as cache directory.\nUse %s as private directory." my-emacs-cache-directory my-emacs-private-directory)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value. Should't be too big.")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

;; *Message* buffer should be writable in 24.4+
(defadvice switch-to-buffer (after switch-to-buffer-after-hack activate)
  (if (string= "*Messages*" (buffer-name))
      (read-only-mode -1)))

;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let ((file-name-handler-alist nil))
  (require 'init-autoload)
  (require 'cl-lib)
  (require 'init-compat)
  (require 'init-utils)
  (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

  ;; Windows configuration, assuming that cygwin is installed at "c:/cygwin"
  ;; (condition-case nil
  ;;     (when *win64*
  ;;       ;; (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
  ;;       (setq cygwin-mount-cygwin-bin-directory "c:/cygwin64/bin")
  ;;       (require 'setup-cygwin)
  ;;       ;; better to set HOME env in GUI
  ;;       ;; (setenv "HOME" "c:/cygwin/home/someuser")
  ;;       )
  ;;   (error
  ;;    (message "setup-cygwin failed, continue anyway")
  ;;    ))

  (require 'init-elpa)
  (require 'init-elpa-mirror)
  (require 'init-grep)
  (require 'init-exec-path) ;; Set up $PATH
  (require 'init-frame-hooks)

  (require 'init-use-package)

  ;; any file use flyspell should be initialized after init-spelling.el
  ;; actually, I don't know which major-mode use flyspell.
  (require 'init-spelling)
  (require 'init-xterm)
  (require 'init-gui-frames)
  (require 'init-ido)
  (require 'init-dired)
  (require 'init-uniquify)
  (require 'init-ibuffer)
  (require 'init-flymake)
  (require 'init-flycheck)
  (require 'init-ivy)
  (require 'init-hippie-expand)
  (require 'init-windows)
  (require 'init-sessions)
  (require 'init-git)
  (require 'init-markdown)
  ;; (require 'init-erlang)
  (require 'init-javascript)
  (require 'init-org)
  (require 'init-org2blog)
  (require 'init-css)
  ;; (require 'init-haskell)
  ;; (require 'init-ruby-mode)
  (require 'init-lisp)
  (require 'init-elisp)
  (require 'init-yasnippet)
  ;; Use bookmark instead
  (require 'init-zencoding-mode)
  (require 'init-cc-mode)
  (require 'init-csharp)
  (require 'init-gud)
  (require 'init-linum-mode)
  ;; (require 'init-gist)
  (require 'init-moz)
  ;; init-evil dependent on init-clipboard
  (require 'init-clipboard)
  ;; use evil mode (vi key binding)
  (require 'init-evil)
  (require 'init-general)
  (require 'init-multiple-cursors)
  (require 'init-bbdb)
  (require 'init-gnus)
  (require 'init-lua-mode)
  (require 'init-workgroups2)
  (require 'init-term-mode)
  (require 'init-web-mode)
  (require 'init-slime)
  (require 'init-company)
  (require 'init-projectile)
  (require 'init-pyim)
  (require 'init-cnfonts)
  ;; need statistics of keyfreq asap
  (require 'init-keyfreq)
  (require 'init-httpd)

  (require 'init-python)
  (require 'init-ggtags)

  (require 'init-smartparens)
  (require 'init-folding)
  ;; projectile costs 7% startup time

  ;; misc has some crucial tools I need immediately
  (require 'init-misc)

  ;; comment below line if you want to setup color theme in your own way
  (if (or (display-graphic-p) (string-match-p "256color"(getenv "TERM"))) (require 'init-color-theme))

  (require 'init-emacs-w3m)
  (require 'init-hydra)
  (require 'init-dictionary)
  (require 'init-misc-lazy)
  (require 'init-fonts)
  (require 'init-hs-minor-mode)
  (require 'init-writting)
  (require 'init-pomodoro)
  (require 'init-emacspeak)
  (require 'init-artbollocks-mode)
  (require 'init-semantic)
  (require 'init-emms)

  (require 'init-avy)

  (require 'init-elfeed)

  (when (require 'time-date nil t)
    (message "Emacs startup time: %d seconds."
             (time-to-seconds (time-since emacs-load-start-time))))

  ;; my personal setup, other major-mode specific setup need it.
  ;; It's dependent on init-site-lisp.el
  (let ((my-custom-config (expand-file-name "custom.el" my-emacs-private-directory)))
    (if (file-exists-p my-custom-config)
        (load-file my-custom-config)
      (message "[Error]Could not find custom.el file, use wrong private directory?"))))

;; @see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
;; (setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(setq custom-file (expand-file-name "custom-set-variables.el" my-emacs-private-directory))
(load custom-file 'noerror)

(setq gc-cons-threshold best-gc-cons-threshold)
;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'erase-buffer 'disabled nil)

;;; init.el ends here
