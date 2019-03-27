;;; init-evil.el --- setup evil

;;; Commentary:

;;; Code:


(use-package evil
  :init (evil-mode)

  :config
  ;; evil bind C-v to evil-visual-block, v/V for visual char/line
  ;; still works, but I'd like to use emacs style rectangle for visual
  ;; block instead.
  (unbind-key "C-v" evil-motion-state-map)

  ;; evil-paste-last-insertion
  (unbind-key "C-a" evil-insert-state-map)

  ;; evil-shift-left-line
  (unbind-key "C-d" evil-insert-state-map)

  ;; evil-execute-in-normal-state
  (unbind-key "C-o" evil-insert-state-map)

  ;; evil bind C-e to evil-scroll-line-down
  (unbind-key "C-e" evil-motion-state-map)

  ;; @see https://bitbucket.org/lyro/evil/issue/360/possible-evil-search-symbol-forward
  ;; evil 1.0.8 search word instead of symbol
  (setq evil-symbol-word-search t)

  ;; need to work with smartparens
  (setq evil-move-beyond-eol t)

  (loop for (mode . state) in
        '((diff-mode . emacs)           ; to make 'q' take effect anyway
          (minibuffer-inactive-mode . emacs)
          (ggtags-global-mode . emacs)
          (grep-mode . emacs)
          (Info-mode . emacs)
          (term-mode . emacs)
          (sdcv-mode . emacs)
          (anaconda-nav-mode . emacs)
          (log-edit-mode . emacs)
          (vc-log-edit-mode . emacs)
          (magit-log-edit-mode . emacs)
          (inf-ruby-mode . emacs)
          (direx:direx-mode . emacs)
          (yari-mode . emacs)
          (erc-mode . emacs)
          (neotree-mode . emacs)
          (w3m-mode . emacs)
          (gud-mode . emacs)
          (help-mode . emacs)
          (eshell-mode . emacs)
          (shell-mode . emacs)
          ;;(message-mode . emacs)
          (fundamental-mode . emacs)
          (weibo-timeline-mode . emacs)
          (weibo-post-mode . emacs)
          (sr-mode . emacs)
          (profiler-report-mode . emacs)
          (dired-mode . emacs)
          (compilation-mode . emacs)
          (speedbar-mode . emacs)
          (ivy-occur-mode . emacs)
          (messages-buffer-mode . normal)
          (magit-commit-mode . normal)
          (magit-diff-mode . normal)
          (browse-kill-ring-mode . normal)
          (etags-select-mode . normal)
          (js2-error-buffer-mode . emacs)
          (git-status-mode . emacs)
          (rmail-mode . emacs)
          (rmail-summary-mode . emacs)
          (image-mode . emacs)
          (image-dired-thumbnail-mode . emacs)
          (image-dired-display-image-mode . emacs)
          (inferior-python-mode . emacs)
          (ein:notebooklist-mode . emacs)
          ;; (ein:notebook-multilang-mode . emacs)
          (ein:shared-output-mode . emacs)
          (ein:traceback-mode . emacs)
          (pdf-occur-buffer-mode . emacs)
          (pyim-dm-mode . emacs)
          (xref--xref-buffer-mode . emacs)
          (cnfonts-ui-mode . emacs)
          (process-menu-mode . emacs)
          (flycheck-error-list-mode . emacs)
          (elfeed-search-mode . emacs)
          (elfeed-show-mode . emacs)
          )
        do (evil-set-initial-state mode state))

  ;; {{ @see https://github.com/timcharper/evil-surround for tutorial
  (use-package evil-surround
    :init (global-evil-surround-mode)

    :config
    (defun evil-surround-prog-mode-hook-setup ()
      (push '(47 . ("/" . "/")) evil-surround-pairs-alist)
      (push '(40 . ("(" . ")")) evil-surround-pairs-alist)
      (push '(41 . ("(" . ")")) evil-surround-pairs-alist))

    (add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)

    (defun evil-surround-emacs-lisp-mode-hook-setup ()
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))

    (add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)

    (defun evil-surround-org-mode-hook-setup ()
      (push '(?= . ("=" . "=")) evil-surround-pairs-alist))

    (add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup))
  ;; }}

  ;; {{ https://github.com/syl20bnr/evil-escape
  (use-package evil-escape
    :init (evil-escape-mode 1)

    :config
    (setq-default evil-escape-delay 0.5)
    (setq evil-escape-excluded-major-modes '(dired-mode))
    (setq-default evil-escape-key-sequence "kj"))
  ;; }}

  ;; {{ For example, press `viW*`
  (use-package evil-visualstar
    :init (global-evil-visualstar-mode t)

    :config
    (setq evil-visualstar/persistent t))
  ;; }}

  (use-package evil-numbers
    :bind (:map evil-normal-state-map
                ("+" . evil-numbers/inc-at-pt)
                ("-" . evil-numbers/dec-at-pt)))

  (use-package evil-matchit
    :init (global-evil-matchit-mode))

  ;; Emacs's internal newcomment.el functionality is enough for me
  (use-package evil-nerd-commenter
    :disabled
    ;; still want to use inline comment, so try not use the default
    ;; key bindings provided.

    ;; :init (evilnc-default-hotkeys)
    :bind (("M-;" . evilnc-comment-or-uncomment-lines)
           ("C-c l" . evilnc-quick-comment-or-uncomment-to-the-line)
           ;; ("C-c c" . evilnc-copy-and-comment-lines)
           ("C-c p" . evilnc-comment-or-uncomment-paragraphs))

    :config
    (eval-after-load 'evil
      '(progn
         (define-key evil-normal-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
         (define-key evil-normal-state-map ",cl" 'evilnc-quick-comment-or-uncomment-to-the-line)
         (define-key evil-normal-state-map ",ll" 'evilnc-quick-comment-or-uncomment-to-the-line)
         (define-key evil-normal-state-map ",cc" 'evilnc-copy-and-comment-lines)
         (define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
         (define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)
         (define-key evil-normal-state-map ",cv" 'evilnc-toggle-invert-comment-line-by-line))))

  ;; {{ evil-exchange
  (use-package evil-exchange
    ;; press gx twice to exchange, gX to cancel
    ;; change default key bindings (if you want) HERE
    ;; (setq evil-exchange-key (kbd "zx"))
    :init (evil-exchange-install))
  ;; }}

  (use-package evil-mark-replace))

;; load undo-tree and ert
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil/lib")

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
    '(progn
       (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
       ;; force update evil keymaps after git-timemachine-mode loaded
       (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

(adjust-major-mode-keymap-with-evil "git-timemachine")
(adjust-major-mode-keymap-with-evil "browse-kill-ring")
(adjust-major-mode-keymap-with-evil "etags-select")

;; {{ multiple-cursors
;; step 1, select thing in visual-mode (OPTIONAL)
;; step 2, `mc/mark-all-like-dwim' or `mc/mark-all-like-this-in-defun'
;; step 3, `ace-mc-add-multiple-cursors' to remove cursor, press RET to confirm
;; step 4, press s or S to start replace
;; step 5, press C-g to quit multiple-cursors
(define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
(define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this-dwim)
(define-key evil-visual-state-map (kbd "md") 'mc/mark-all-like-this-in-defun)
(define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
(define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor)
;; }}

;; ffip-diff-mode evil setup
(defun ffip-diff-mode-hook-setup ()
    (evil-local-set-key 'normal "p" 'diff-hunk-prev)
    (evil-local-set-key 'normal "n" 'diff-hunk-next)
    (evil-local-set-key 'normal "P" 'diff-file-prev)
    (evil-local-set-key 'normal "N" 'diff-file-next)
    (evil-local-set-key 'normal "q" 'ffip-diff-quit)
    (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
    (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)


;; {{ define my own text objects, works on evil v1.0.9 using older method
;; @see http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; between dollar signs:
(define-and-bind-text-object "$" "\\$" "\\$")
;; between equal signs
(define-and-bind-text-object "=" "=" "=")
;; between pipe characters:
(define-and-bind-text-object "|" "|" "|")
;; regular expression
(define-and-bind-text-object "/" "/" "/")
;; trimmed line
(define-and-bind-text-object "l" "^ *" " *$")
;; angular template
(define-and-bind-text-object "r" "\{\{" "\}\}")
;; }}


;; {{ nearby file path as text object,
;;      - "vif" to select only basename
;;      - "vaf" to select the full path
;;
;;  example: "/hello/world" "/test/back.exe"
;;               "C:hello\\hello\\world\\test.exe" "D:blah\\hello\\world\\base.exe"
;;
;; tweak evil-filepath-is-nonname to re-define a path
(defun evil-filepath-is-separator-char (ch)
  "Check ascii table that CH is slash characters.
If the character before and after CH is space or tab, CH is NOT slash"
  (let (rlt prefix-ch postfix-ch)
    (when (and (> (point) (point-min)) (< (point) (point-max)))
        (save-excursion
          (backward-char)
          (setq prefix-ch (following-char)))
        (save-excursion
          (forward-char)
          (setq postfix-ch (following-char))))
    (if (and (not (or (= prefix-ch 32) (= postfix-ch 32)))
             (or (= ch 47) (= ch 92)) )
        (setq rlt t))
    rlt))

(defun evil-filepath-not-path-char (ch)
  "Check ascii table for charctater "
  (let (rlt)
    (if (or (and (<= 0 ch) (<= ch 32))
            (= ch 34) ; double quotes
            (= ch 39) ; single quote
            (= ch 40) ; (
            (= ch 41) ; )
            (= ch 60) ; <
            (= ch 62) ; >
            (= ch 91) ; [
            (= ch 93) ; ]
            (= ch 96) ; `
            (= ch 123) ; {
            (= ch 125) ; }
            (= 127 ch))
        (setq rlt t))
    rlt))

(defun evil-filepath-char-not-placed-at-end-of-path (ch)
  (or (= 44 ch) ; ,
      (= 46 ch) ; .
      ))

(defun evil-filepath-calculate-path (b e)
  (let (rlt f)
    (when (and b e)
      (setq b (+ 1 b))
      (when (save-excursion
                (goto-char e)
                (setq f (evil-filepath-search-forward-char 'evil-filepath-is-separator-char t))
                (and f (>= f b)))
        (setq rlt (list b (+ 1 f) (- e 1)))))
    rlt))

(defun evil-filepath-get-path-already-inside ()
  (let (b e)
    (save-excursion
      (setq b (evil-filepath-search-forward-char 'evil-filepath-not-path-char t)))
    (save-excursion
      (setq e (evil-filepath-search-forward-char 'evil-filepath-not-path-char))
      (when e
        (goto-char (- e 1))
        ;; example: hello/world,
        (if (evil-filepath-char-not-placed-at-end-of-path (following-char))
            (setq e (- e 1)))
        ))
    (evil-filepath-calculate-path b e)))

(defun evil-filepath-search-forward-char (fn &optional backward)
  (let (found rlt (limit (if backward (point-min) (point-max))) out-of-loop)
    (save-excursion
      (while (not out-of-loop)
        ;; for the char, exit
        (if (setq found (apply fn (list (following-char))))
            (setq out-of-loop t)
          ;; reach the limit, exit
          (if (= (point) limit)
              (setq out-of-loop t)
            ;; keep moving
            (if backward (backward-char) (forward-char)))))
      (if found (setq rlt (point))))
    rlt))

(defun evil-filepath-extract-region ()
  "Find the closest file path"
  (let (rlt
        b
        f1
        f2)

    (if (and (not (evil-filepath-not-path-char (following-char)))
             (setq rlt (evil-filepath-get-path-already-inside)))
        ;; maybe (point) is in the middle of the path
        t
      ;; need search forward AND backward to find the right path
      (save-excursion
        ;; path in backward direction
        (when (setq b (evil-filepath-search-forward-char 'evil-filepath-is-separator-char t))
          (goto-char b)
          (setq f1 (evil-filepath-get-path-already-inside))))
      (save-excursion
        ;; path in forward direction
        (when (setq b (evil-filepath-search-forward-char 'evil-filepath-is-separator-char))
          (goto-char b)
          (setq f2 (evil-filepath-get-path-already-inside))))
      ;; pick one path as the final result
      (cond
       ((and f1 f2)
        (if (> (- (point) (nth 2 f1)) (- (nth 0 f2) (point)))
            (setq rlt f2)
          (setq rlt f1)))
       (f1
        (setq rlt f1))
       (f2
        (setq rlt f2))))

    rlt))

(evil-define-text-object evil-filepath-inner-text-object (&optional count begin end type)
  "File name of nearby path"
  (let ((selected-region (evil-filepath-extract-region)))
    (if selected-region
        (evil-range (nth 1 selected-region) (nth 2 selected-region) :expanded t))))

(evil-define-text-object evil-filepath-outer-text-object (&optional NUM begin end type)
  "Nearby path"
  (let ((selected-region (evil-filepath-extract-region)))
    (if selected-region
        (evil-range (car selected-region) (+ 1 (nth 2 selected-region)) type :expanded t))))

(define-key evil-inner-text-objects-map "f" 'evil-filepath-inner-text-object)
(define-key evil-outer-text-objects-map "f" 'evil-filepath-outer-text-object)
;; }}

(defun toggle-org-or-message-mode ()
  (interactive)
  (if (eq major-mode 'message-mode)
      (org-mode)
    (if (eq major-mode 'org-mode) (message-mode))
    ))

;; (evil-set-initial-state 'org-mode 'emacs)

;; As a general RULE, mode specific evil leader keys started
;; with uppercased character or 'g' or special character except "=" and "-"
(evil-declare-key 'normal org-mode-map
  "gh" 'outline-up-heading
  "gl" 'outline-next-visible-heading
  "$" 'org-end-of-line ; smarter behaviour on headlines etc.
  "^" 'org-beginning-of-line ; ditto
  "<" (lambda () (interactive) (org-demote-or-promote 1)) ; out-dent
  ">" 'org-demote-or-promote ; indent
  (kbd "TAB") 'org-cycle)


;; I prefer Emacs way after pressing ":" in evil-mode
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

;;; restore original command `xref-find-definitions'
(define-key evil-normal-state-map (kbd "M-.") nil)

(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map "go" 'goto-char)
(define-key evil-normal-state-map (kbd "M-y") 'counsel-browse-kill-ring)
(define-key evil-normal-state-map (kbd "C-]") 'etags-select-find-tag-at-point)
(define-key evil-visual-state-map (kbd "C-]") 'etags-select-find-tag-at-point)


;; press ",xx" to expand region
;; then press "z" to contract, "x" to expand
(eval-after-load "evil"
  '(progn
     (setq expand-region-contract-fast-key "z")
     ))

;; I learn this trick from ReneFroger, need latest expand-region
;; @see https://github.com/redguardtoo/evil-matchit/issues/38
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
;; (define-key evil-insert-state-map (kbd "M-j") 'yas-expand)
;; (define-key evil-emacs-state-map (kbd "M-j") 'yas-expand)

;; {{ remember what we searched
;; http://emacs.stackexchange.com/questions/24099/how-to-yank-text-to-search-command-after-in-evil-mode/
(defvar my-search-text-history nil "List of text I searched.")
(defun my-select-from-search-text-history ()
  (interactive)
  (ivy-read "Search text history:" my-search-text-history
            :action (lambda (item)
                      (copy-yank-str item)
                      (message "%s => clipboard & yank ring" item))))
(defun my-cc-isearch-string ()
  (interactive)
  (if (and isearch-string (> (length isearch-string) 0))
      ;; NOT pollute clipboard who has things to paste into Emacs
      (add-to-list 'my-search-text-history isearch-string)))

(defadvice evil-search-incrementally (after evil-search-incrementally-after-hack activate)
  (my-cc-isearch-string))

(defadvice evil-search-word (after evil-search-word-after-hack activate)
  (my-cc-isearch-string))

(defadvice evil-visualstar/begin-search (after evil-visualstar/begin-search-after-hack activate)
  (my-cc-isearch-string))
;; }}

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))


;; }}

(provide 'init-evil)
;;; init-evil.el ends here
