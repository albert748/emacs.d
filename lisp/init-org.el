;; some cool org tricks
;; @see http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode

;; {{ NO spell check for embedded snippets
(defun org-mode-is-code-snippet ()
  (let (rlt
        (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
        (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
        (old-flag case-fold-search)
        b e)
      (save-excursion
        (setq case-fold-search t)
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t)))
        (setq case-fold-search old-flag))
      (if (and b e (< (point) e)) (setq rlt t))
    rlt))

;; no spell check for property
(defun org-mode-current-line-is-property ()
  (string-match "^[ \t]+:[A-Z]+:[ \t]+" (my-line-str)))

;; Please note flyspell only use ispell-word
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let ((run-spellcheck ad-return-value))
    (if ad-return-value
      (cond
       ((org-mode-is-code-snippet)
        (setq run-spellcheck nil))
       ((org-mode-current-line-is-property)
        (setq run-spellcheck nil))))
    (setq ad-return-value run-spellcheck)))
;; }}

;; Org v8 change log:
;; @see http://orgmode.org/worg/org-8.0.html

;; {{ export org-mode in Chinese into PDF
;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
;; and you need install texlive-xetex on different platforms
;; To install texlive-xetex:
;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
(setq org-latex-to-pdf-process ;; org v7
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-pdf-process org-latex-to-pdf-process) ;; org v8
;; }}

(defun my-setup-odt-org-convert-process ()
  (interactive)
  (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
    (when (and *is-a-mac* (file-exists-p cmd))
      ;; org v7
      (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))
      ;; org v8
      (setq org-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))
    ))

(my-setup-odt-org-convert-process)

(defun narrow-to-region-indirect-buffer-maybe (start end use-indirect-buffer)
  "Indirect buffer could multiple widen on same file."
  (if (region-active-p) (deactivate-mark))
  (if use-indirect-buffer
      (with-current-buffer (clone-indirect-buffer
                            (generate-new-buffer-name
                             (concat (buffer-name) "-indirect-"
                                     (number-to-string start) "-"
                                     (number-to-string end)))
                            'display)
        (narrow-to-region start end)
        (goto-char (point-min)))
      (narrow-to-region start end)))

;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
(defun narrow-or-widen-dwim (&optional use-indirect-buffer)
  "If the buffer is narrowed, it widens.
 Otherwise, it narrows to region, or Org subtree.
If use-indirect-buffer is not nil, use `indirect-buffer' to hold the widen content."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p)
         (narrow-to-region-indirect-buffer-maybe (region-beginning)
                                                 (region-end)
                                                 use-indirect-buffer))
        ((equal major-mode 'org-mode)
         (org-narrow-to-subtree))
        ((equal major-mode 'diff-mode)
         (let* (b e)
           (save-excursion
             (setq b (diff-beginning-of-file))
             (setq e (progn (diff-end-of-file) (point))))
           (when (and b e (< b e))
             (narrow-to-region-indirect-buffer-maybe b e use-indirect-buffer))))
        (t (error "Please select a region to narrow to"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(eval-after-load 'org-clock
  '(progn
     (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
     (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

(defun org-mode-hook-setup ()
  (setq evil-auto-indent nil)
  ;; org-mode's own flycheck will be loaded
  (enable-flyspell-mode-conditionally)

  ;; but I don't want to auto spell check when typing,
  ;; please comment out `(flyspell-mode -1)` if you prefer auto spell check
  (flyspell-mode -1)

  ;; for some reason, org8 disable odt export by default
  (add-to-list 'org-export-backends 'odt)
  (add-to-list 'org-export-backends 'org) ; for org-mime

  ;; org-mime setup, run this command in org-file, than yank in `message-mode'
  (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)

  ;; don't spell check double words
  (setq flyspell-check-doublon nil))

(add-hook 'org-mode-hook 'org-mode-hook-setup)

(defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
  (let ((browse-url-browser-function
         (cond
          ;; open with external browser
          ((equal (ad-get-arg 0) '(4)) 'browse-url-generic)
          ;; open with w3m
          (t 'w3m-browse-url))))
    ad-do-it))

(defadvice org-publish (around org-publish-advice activate)
  "Stop running major-mode hook when org-publish"
  (let ((old load-user-customized-major-mode-hook))
    (setq load-user-customized-major-mode-hook nil)
    ad-do-it
    (setq load-user-customized-major-mode-hook old)))

;; {{ org2nikola set up
(setq org2nikola-output-root-directory "~/.config/nikola")
(setq org2nikola-use-google-code-prettify t)
(setq org2nikola-prettify-unsupported-language
      '(elisp "lisp"
              emacs-lisp "lisp"))
;; }}

(defun org-demote-or-promote (&optional is-promote)
  (interactive "P")
  (unless (region-active-p)
    (org-mark-subtree))
  (if is-promote (org-do-promote)
    (org-do-demote)))

(defun org-mime-html-hook-setup ()
  (org-mime-change-element-style "pre"
                                 "color:#E6E1DC; background-color:#232323; padding:0.5em;")
  (org-mime-change-element-style "blockquote"
                                 "border-left: 2px solid gray; padding-left: 4px;"))
;; org-mime setup
(eval-after-load 'org-mime
  '(progn
     (add-hook 'org-mime-html-hook 'org-mime-html-hook-setup)))

;; {{ @see http://orgmode.org/worg/org-contrib/org-mime.html
;; demo video: http://vimeo.com/album/1970594/video/13158054
(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))
;; }}

(use-package org
  ;; FIXME: the key bind to evilnc-copy-and-comment-lines
  :bind*
  (("C-c c" . org-capture))
  :bind
  (("C-c a" . org-agenda)
   ("C-c C-x C-j" . org-clock-goto))

  ;; FIXME: package not pined to melpa instead of built-in used.
  ;; :pin org

  :config
  ;; ob-async enables asynchronous execution of org-babel src blocks.
  ;; simply add the keyword :async to header args of src block and invoke ob-async-org-babel-execute-src-block
  ;; @see https://github.com/astahlman/ob-async
  (use-package ob-async)

  ;; facilitates images download from browser and screenshots
  ;; @see https://github.com/abo-abo/org-download
  (use-package org-download
    :config
    ;; FIXME: download to special directory within org dir.
    (setq org-download-method 'attach))


  ;; Create graphviz directed graphs from org-mode files
  ;; @ see https://github.com/theodorewiles/org-mind-map

  ;; FIXME: when start emacs and execute export right now, error
  ;; comes: "byte-code: Symbol’s function definition is void:
  ;; org-export-define-derived-backend", the command can be executed
  ;; after few seconds delay, why?
  (use-package org-mind-map
    :commands (org-mind-map-write)
    :if (if (executable-find "unflatten")
            t
          (message "[Missing] Install graphviz to enable org-mind-map")
          nil))

  (add-hook 'org-mode-hook #'(lambda () (setq show-trailing-whitespace t)))

  ;; new or refiled headings always insert from the beginning
  (setq org-reverse-note-order t)

  ;; auto load language before evaluate
  (defun org-babel-execute-src-block-load-lang (&optional arg info params)
    "Load language on-demand"
    (let* ((info (if info (copy-tree info) (org-babel-get-src-block-info)))
           (language (nth 0 info)))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))))
  (advice-add 'org-babel-execute-src-block :before #'org-babel-execute-src-block-load-lang)

  ;; assume all files inside org-directory is safe
  (defun org-confirm-babel-evaluate-safe-directory (lang body)
    "Return nil if file is one of agenda files"
    (not (member buffer-file-name (org-agenda-files))))
  (setq org-confirm-babel-evaluate 'org-confirm-babel-evaluate-safe-directory)

  ;; perfer to use bash
  (setq org-babel-sh-command "bash")

  ;; org mode use truncate-lines as default, reset it to nil.
  ;; we do not want to use word-wrap, beacause it's ugly for chinese.
  ;; FIXME: word-wrap chinese only.
  (setq org-startup-truncated nil)

  ;; Various preferences
  (setq org-log-done t           ; basic logging when move to DONE state
        org-log-into-drawer t      ; advanced state change logging
        ;; org-clock-into-drawer t    ; default, save clock data to LOGBOOK
        org-tags-column 0      ; place tags directly after headline text
        ;; org-agenda-window-setup 'current-window ; use default reorganize-frame
        org-agenda-restore-windows-after-quit t ; restore old state with 'q' or 'x'
        ;; org-agenda-span 14                ; default: week, expand to 2 weeks
        org-agenda-start-on-weekday nil   ; for weekly agendas, start on the current day
        ;; org-agenda-include-diary t        ; default: nil
        org-completion-use-ido t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-fast-tag-selection-single-key 'expert
        org-export-kill-product-buffer-when-displayed t
        ;; org v7
        ;; org-export-odt-preferred-output-format "doc"
        ;; org v8
        ;; org-odt-preferred-output-format "doc"
        ;; org-startup-indented t
        ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
        ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        org-agenda-inhibit-startup t ;; ~50x speedup
        ;; org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        ;; }}
        )

  ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))
  ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
  (setq org-outline-path-complete-in-steps t)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))

  (setq org-imenu-depth 9)

  ;; @see http://irreal.org/blog/1
  (setq org-src-fontify-natively t)

  ;; save all clock history to ~/.emacs.d/org-clock-save.el
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")

  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t))



(provide 'init-org)
