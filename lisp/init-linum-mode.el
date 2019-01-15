;;; init-linum-mode.el --- line number mode initialization

;;; Commentary:

;; display-line-numbers-mode available since Emacs 26, replacement of linum-mode

;;; Code:

(use-package display-line-numbers
  :if (>= emacs-major-version 26)
  :ensure nil
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'org-mode-hook #'display-line-numbers-mode)

  :config
  ;; what's the difference between 'relative and 'visual?
  (setq display-line-numbers-type t))

(use-package linum
  :if (< emacs-major-version 26)
  :ensure nil                           ; built-in package
  :init (global-linum-mode)

  :config
  ;; update line number every second
  (setq linum-delay t)

  (defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 1 nil #'linum-update-current))

  ;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
  (defvar linum-mode-inhibit-modes-list '(eshell-mode
                                        shell-mode
                                        profiler-report-mode
                                        ffip-diff-mode
                                        dictionary-mode
                                        erc-mode
                                        browse-kill-ring-mode
                                        etags-select-mode
                                        dired-mode
                                        help-mode
                                        text-mode
                                        fundamental-mode
                                        jabber-roster-mode
                                        jabber-chat-mode
                                        inferior-js-mode
                                        inferior-python-mode
                                        inferior-scheme-mode
                                        twittering-mode
                                        compilation-mode
                                        weibo-timeline-mode
                                        woman-mode
                                        Info-mode
                                        calc-mode
                                        calc-trail-mode
                                        comint-mode
                                        gnus-group-mode
                                        inf-ruby-mode
                                        gud-mode
                                        org-mode
                                        vc-git-log-edit-mode
                                        log-edit-mode
                                        term-mode
                                        w3m-mode
                                        speedbar-mode
                                        gnus-summary-mode
                                        gnus-article-mode
                                        calendar-mode
                                        ;; @see http://emacs.stackexchange.com/questions/5702/emacs-got-frozen-when-open-pdf-file
                                        doc-view-mode
                                        pdf-view-mode
                                        image-mode
                                        image-dired-thumbnail-mode
                                        image-dired-display-image-mode))

  (defadvice linum-on (around linum-on-inhibit-for-modes)
    "Stop the load of linum-mode for some major modes."
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))
  (ad-activate 'linum-on))


(provide 'init-linum-mode)
;;; init-linum-mode ends here