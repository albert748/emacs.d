;;; init-misc.el --- Misc pacakges setup

;;; Commentary:

;;; Code:

(use-package apropos
  :ensure nil
  :defer t
  :config
  ;; apropos result sort match with scores.
  (setq apropos-sort-by-scores 'verbose)
  (setq apropos-documentation-sort-by-scores 'verbose))

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package hideshow
  :diminish hs-minor-mode
  :ensure nil
  :defer t)

(use-package checkdoc
  :diminish checkdoc-minor-mode
  :ensure nil
  :defer t)

(use-package eldoc
  :diminish eldoc-mode
  :ensure nil
  :defer t)

(use-package subword
  :diminish subword-mode
  :ensure nil
  :defer t)

(use-package sudo-edit
  :commands (sudo-edit))

(use-package powerline
  :init
  (setq powerline-default-separator 'zigzag)
  (powerline-default-theme)
  )

(use-package vimrc-mode
  :defer t)

(use-package csv-mode
  :defer t
  :config (setq csv-separators '("," ";" "|" " ")))

(use-package pdf-tools
  ;; awesome PDF mode better then DocView, @see https://github.com/politza/pdf-tools
  ;; you need install poppler-glib to compile the server.
  :init (pdf-tools-install))

(use-package sh-script
  :ensure nil                           ; built-in package
  ;; FIXME: merge all together
  :mode (("\\.bash_profile\\'" . sh-mode)
         ("\\.bash_history\\'" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.bashrc.local\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.bashrc\\'" . sh-mode)))

(use-package restart-emacs
  :commands (restart-emacs))

(use-package yaml-mode
  :defer t)

(use-package csharp-mode
  :defer t)

(use-package systemd
  :defer t
  :config
  (add-hook 'systemd-mode-hook #'company-mode))

(use-package pkgbuild-mode
  :defer t)

(use-package groovy-mode
  :defer t)

;; {{ shell and conf
;; (add-to-list 'auto-mode-alist '("\\.[^b][^a][a-zA-Z]*rc$" . conf-mode))
;; (add-to-list 'auto-mode-alist '("\\.aspell\\.en\\.pws\\'" . conf-mode))
;; (add-to-list 'auto-mode-alist '("\\.meta\\'" . conf-mode))
;; (add-to-list 'auto-mode-alist '("\\.?muttrc\\'" . conf-mode))
;; (add-to-list 'auto-mode-alist '("\\.ctags\\'" . conf-mode))
;; (add-to-list 'auto-mode-alist '("\\.mailcap\\'" . conf-mode))
(use-package conf-mode
  :ensure nil                           ; built-in package
  :mode ("\\.[^b][^a][a-zA-Z]*rc$" "\\.aspell\\.en\\.pws\\'" "\\.meta\\'" "\\.?muttrc\\'" "\\.ctags\\'" "\\.mailcap\\'"))
;; }}

;; java
(add-to-list 'auto-mode-alist '("\\.aj\\'" . java-mode))
;; makefile
(add-to-list 'auto-mode-alist '("\\.ninja$" . makefile-gmake-mode))



;; {{ auto-yasnippet
;; Use C-q instead tab to complete snippet
;; - `aya-create' at first, input ~ to mark the thing next
;; - `aya-expand' to expand snippet
;; - `aya-open-line' to finish
(global-set-key (kbd "C-q") #'aya-open-line)
;; }}

;; {{ support MY packages which are not included in melpa
(setq org2nikola-use-verbose-metadata t) ; for nikola 7.7+
;; }}

(define-key global-map (kbd "RET") 'newline-and-indent)

;; M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; {{ isearch
;; Use regex to search by default
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)
;; (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
;; }}

(setq-default buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              save-interprogram-paste-before-kill t
              indent-tabs-mode nil      ; disable TAB
              ;; line-spacing 0.2
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              ;; void problems with crontabs, etc.
              ;; require-final-newline t ; bad idea, could accidentally edit others' code
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)

;; @see http://www.emacswiki.org/emacs/SavePlace
;; (require 'saveplace)
;; (setq-default save-place t)             ; obsolete sinice 25.1
(use-package saveplace
  :ensure nil                           ; built-in package
  :config
  (setq save-place-file (expand-file-name "places" my-emacs-cache-directory))
  (save-place-mode))


;; {{ find-file-in-project (ffip)
(defun my-git-show-selected-commit ()
  "Run 'git show selected-commit' in shell."
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
         (git-cmd-rlts (split-string (shell-command-to-string git-cmd) "\n" t))
         (line (ivy-read "git log:" git-cmd-rlts)))
    (shell-command-to-string (format "git show %s"
                                     (car (split-string line "|" t))))))

(defun my-git-diff-current-file ()
  "Run 'git diff version:current-file current-file'."
  (let* ((git-cmd (concat "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an' "
                          buffer-file-name))
         (git-root (locate-dominating-file default-directory ".git"))
         (git-cmd-rlts (nconc (split-string (shell-command-to-string "git branch --no-color --all") "\n" t)
                              (split-string (shell-command-to-string git-cmd) "\n" t)))
         (line (ivy-read "git diff same file with version" git-cmd-rlts)))
    (shell-command-to-string (format "git --no-pager diff %s:%s %s"
                                     (replace-regexp-in-string "^ *\\*? *" "" (car (split-string line "|" t)))
                                     (file-relative-name buffer-file-name git-root)
                                     buffer-file-name))))

(setq ffip-match-path-instead-of-filename t)
;; I only use git
(setq ffip-diff-backends '(my-git-show-selected-commit
                           my-git-diff-current-file
                           my-git-log-patch-current-file
                           "cd $(git rev-parse --show-toplevel) && git diff"
                           "cd $(git rev-parse --show-toplevel) && git diff --cached"
                           (shell-command-to-string (format "cd $(git rev-parse --show-toplevel) && git --no-pager log --date=short -S'%s' -p"
                                                            (read-string "Git search string:")))
                           (car kill-ring)))
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-get-project-root-directory))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))
;; }}


;; {{ https://github.com/browse-kill-ring/browse-kill-ring
(require 'browse-kill-ring)
;; no duplicates
(setq browse-kill-ring-display-style 'one-line
      browse-kill-ring-display-duplicates nil
      ;; preview is annoying
      browse-kill-ring-show-preview nil)
(browse-kill-ring-default-keybindings)
;; hotkeys:
;; n/p => next/previous
;; s/r => search
;; l => filter with regex
;; g => update/refresh
;; }}

;; {{ gradle
(defun my-run-gradle-in-shell (cmd)
  (interactive "sEnter a string:")
  (let ((root-dir (locate-dominating-file default-directory
                                          "build.gradle")))
    (if root-dir
      (let ((default-directory root-dir))
        (shell-command (concat "gradle " cmd "&"))))
    ))
;; }}

;; {{ crontab
;; in shell "EDITOR='emacs -nw' crontab -e" to edit cron job
;; (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
;; (add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))
(use-package crontab-mode
  :ensure nil
  :mode ("\\.cron\\(tab\\)?\\'" "cron\\(tab\\)?\\."))
;; }}

;; cmake
;; (setq auto-mode-alist (append '(("CMakeLists\\.txt\\'" . cmake-mode))
;;                               '(("\\.cmake\\'" . cmake-mode))
;;                               auto-mode-alist))
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(defun back-to-previous-buffer ()
  (interactive)
  (switch-to-buffer nil))

;; {{ dictionary setup
(defun my-lookup-dict-org ()
  (interactive)
  (dictionary-new-search (cons (my-use-selected-string-or-ask "Input word for dict.org:")
                               dictionary-default-dictionary)))
;; }}

;; {{ bookmark
;; use my own bmk if it exists
;; (if (file-exists-p (file-truename "~/.emacs.bmk"))
;;     (setq bookmark-file (file-truename "~/.emacs.bmk")))

(setq bookmark-default-file (expand-file-name "bookmarks" my-emacs-private-directory))
;; }}

(defun insert-lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque sem mauris, aliquam vel interdum in, faucibus non libero. Asunt in anim uis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in anim id est laborum. Allamco laboris nisi ut aliquip ex ea commodo consequat."))

(defun my-gud-gdb ()
  (interactive)
  (gud-gdb (concat "gdb --fullname \"" (cppcm-get-exe-path-current-buffer) "\"")))

(defun my-overview-of-current-buffer ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun lookup-doc-in-man ()
  (interactive)
  (man (concat "-k " (my-use-selected-string-or-ask ""))))

;; @see http://blog.binchen.org/posts/effective-code-navigation-for-web-development.html
;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; Don't echo passwords when communicating with interactive programs:
;; Github prompt is like "Password for 'https://user@github.com/':"
(setq comint-password-prompt-regexp (format "%s\\|^ *Password for .*: *$" comint-password-prompt-regexp))
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; {{ which-key-mode
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode t)
  ;; :config
  ;; (setq which-key-allow-imprecise-window-fit t) ; performance
  ;; (setq which-key-separator ":")
  )
;; }}

;; smex or counsel-M-x?
;; (defvar my-use-smex nil
;;   "Use `smex' instead of `counsel-M-x' when press M-x.")
;; (defun my-M-x ()
;;   (interactive)
;;   (if my-use-smex (smex)
;;     ;; `counsel-M-x' will use `smex' to remember history
;;     (counsel-M-x)))
;;; to make fcitx.el recognizable
;; (global-set-key (kbd "M-x") 'counsel-M-x)
(use-package counsel
  :bind ("M-x" . counsel-M-x))

(use-package smex
  :config
  (setq smex-save-file (expand-file-name "smex-items" my-emacs-cache-directory)))

(defun compilation-finish-hide-buffer-on-success (buf str)
  "Could be reused by other major-mode after compilation."
  (if (string-match "exited abnormally" str)
      ;;there were errors
      (message "compilation errors, press C-x ` to visit")
    ;;no errors, make the compilation window go away in 0.5 seconds
    (when (string-match "*compilation*" (or (buffer-name buf) ""))
      ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
      (bury-buffer "*compilation*")
      (winner-undo)
      (message "NO COMPILATION ERRORS!"))))

(defun generic-prog-mode-hook-setup ()
  ;; turn off `linum-mode' when there are more than 5000 lines
  (if (buffer-too-big-p) (linum-mode -1))

  (unless (is-buffer-file-temp)

    ;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
    (setq compilation-window-height 8)
    (setq compilation-finish-functions
          '(compilation-finish-hide-buffer-on-success))

    ;; fic-mode has performance issue on 5000 line C++, we can always use swiper instead
    ;; don't spell check double words
    (setq flyspell-check-doublon nil)
    ;; enable for all programming modes
    ;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
    (subword-mode)

    ;; setting this may conflict with smartparens, which insert another double quotes.
    ;; (setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
    ;; (electric-pair-mode 1)

    ;; eldoc, show API doc in minibuffer echo area
    ;; (turn-on-eldoc-mode)
    ;; show trailing spaces in a programming mod
    (setq show-trailing-whitespace t)))

(add-hook 'prog-mode-hook 'generic-prog-mode-hook-setup)
;; some major-modes NOT inherited from prog-mode
(add-hook 'css-mode-hook 'generic-prog-mode-hook-setup)

;; turns on auto-fill-mode, don't use text-mode-hook because for some
;; mode (org-mode for example), this will make the exported document
;; ugly!
;; (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)
(add-hook 'cc-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; some project prefer tab, so be it
;; @see http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
(setq-default tab-width 4)

(setq history-delete-duplicates t)

;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;; @see http://stackoverflow.com/questions/4222183/emacs-how-to-jump-to-function-definition-in-el-file
(global-set-key (kbd "C-h C-f") 'find-function)

;; from RobinH, Time management
(use-package time
  :ensure nil                           ; built-in package
  :config
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  (display-time))

;;a no-op function to bind to if you want to set a keystroke to null
(defun void () "this is a no-op" (interactive))

;effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defun my-download-subtitles ()
  (interactive)
  (shell-command "periscope.py -l en *.mkv *.mp4 *.avi &"))

;; edit confluence wiki
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . confluence-edit-mode))

(defun erase-specific-buffer (num buf-name)
  (let ((message-buffer (get-buffer buf-name))
        (old-buffer (current-buffer)))
    (save-excursion
      (if (buffer-live-p message-buffer)
          (progn
            (switch-to-buffer message-buffer)
            (if (not (null num))
                (progn
                  (end-of-buffer)
                  (dotimes (i num)
                    (previous-line))
                  (set-register t (buffer-substring (point) (point-max)))
                  (erase-buffer)
                  (insert (get-register t))
                  (switch-to-buffer old-buffer))
              (progn
                (erase-buffer)
                (switch-to-buffer old-buffer))))
        (error "Message buffer doesn't exists!")
        ))))

;; {{ message buffer things
(defun erase-message-buffer (&optional num)
  "Erase the content of the *Messages* buffer in emacs.
    Keep the last num lines if argument num if given."
  (interactive "p")
  (let ((buf (cond
              ((eq 'ruby-mode major-mode) "*server*")
              (t "*Messages*"))))
    (erase-specific-buffer num buf)))

;; turn off read-only-mode in *Message* buffer, a "feature" in v24.4
(when (fboundp 'messages-buffer-mode)
  (defun messages-buffer-mode-hook-setup ()
    (message "messages-buffer-mode-hook-setup called")
    (read-only-mode -1))
  (add-hook 'messages-buffer-mode-hook 'messages-buffer-mode-hook-setup))
;; }}

;; increase and decrease font size in GUI emacs
;; (when (display-graphic-p)
;;   (global-set-key (kbd "C-=") 'text-scale-increase)
;;   (global-set-key (kbd "C--") 'text-scale-decrease))

;; {{ show email sent by `git send-email' in gnus
(eval-after-load 'gnus
  '(progn
     (require 'gnus-article-treat-patch)
     (setq gnus-article-patch-conditions
           '( "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" ))
     ))
;; }}

(defun toggle-full-window()
  "Toggle the full view of selected window"
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)
    ))

(defun add-pwd-into-load-path ()
  "add current directory into load-path, useful for elisp developers"
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (if (not (memq dir load-path))
        (add-to-list 'load-path dir)
      )
    (message "Directory added into load-path:%s" dir)
    )
  )

(setq system-time-locale "C")

(setq imenu-max-item-length 256)

;; {{ recentf-mode
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 2048
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        ;; ctags
                        "/TAGS$"
                        ;; global
                        "/GTAGS$"
                        "/GRAGS$"
                        "/GPATH$"
                        ;; binary
                        "\\.mkv$"
                        "\\.mp[34]$"
                        "\\.avi$"
                        "\\.pdf$"
                        ;; sub-titles
                        "\\.sub$"
                        "\\.srt$"
                        "\\.ass$"
                        ;; ~/.emacs.d/**/*.el included
                        ;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
                        ))
;; }}

;; {{ popup functions
(defun my-which-file ()
  "Return current file name for Yasnippets."
  (if (buffer-file-name) (format "%s:" (file-name-nondirectory (buffer-file-name)))
    ""))

(defun my-which-function ()
  "Return current function name."
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function))

(defun popup-which-function ()
  (interactive)
  (let ((msg (my-which-function)))
    (popup-tip msg)
    (copy-yank-str msg)))
;; }}

;; {{ music
(defun mpc-which-song ()
  (interactive)
  (let ((msg (car (split-string (shell-command-to-string "mpc") "\n+"))))
    (message msg)
    (copy-yank-str msg)))

(defun mpc-next-prev-song (&optional prev)
  (interactive)
  (message (car (split-string (shell-command-to-string
                               (concat "mpc " (if prev "prev" "next"))) "\n+"))))
(defun lyrics()
  "Prints the lyrics for the current song"
  (interactive)
  (let ((song (shell-command-to-string "lyrics")))
    (if (equal song "")
        (message "No lyrics - Opening browser.")
      (switch-to-buffer (create-file-buffer "Lyrics"))
      (insert song)
      (goto-line 0))))
;; }}

;; @see http://www.emacswiki.org/emacs/EasyPG#toc4
;; besides, use gnupg 1.4.9 instead of 2.0
(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

;; {{ move focus between sub-windows
(require 'window-numbering)
(custom-set-faces '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
(window-numbering-mode 1)
;; }}

;; ANSI-escape coloring in compilation-mode
;; {{ http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
;; }}

;; @see http://emacs.stackexchange.com/questions/14129/which-keyboard-shortcut-to-use-for-navigating-out-of-a-string
(defun font-face-is-similar (f1 f2)
  (let (rlt)
    ;; (message "f1=%s f2=%s" f1 f2)
    ;; in emacs-lisp-mode, the '^' from "^abde" has list of faces:
    ;;   (font-lock-negation-char-face font-lock-string-face)
    (if (listp f1) (setq f1 (nth 1 f1)))
    (if (listp f2) (setq f2 (nth 1 f2)))

    (if (eq f1 f2) (setq rlt t)
      ;; C++ comment has different font face for limit and content
      ;; f1 or f2 could be a function object because of rainbow mode
      (if (and (string-match "-comment-" (format "%s" f1)) (string-match "-comment-" (format "%s" f2)))
          (setq rlt t)))
    rlt))


;; {{
(defun goto-edge-by-comparing-font-face (&optional step)
"Goto either the begin or end of string/comment/whatever.
If step is -1, go backward."
  (interactive "P")
  (let ((cf (get-text-property (point) 'face))
        (p (point))
        rlt
        found
        end)
    (unless step (setq step 1)) ;default value
    (setq end (if (> step 0) (point-max) (point-min)))
    (while (and (not found) (not (= end p)))
      (if (not (font-face-is-similar (get-text-property p 'face) cf))
          (setq found t)
        (setq p (+ p step))))
    (if found (setq rlt (- p step))
      (setq rlt p))
    ;; (message "rlt=%s found=%s" rlt found)
    (goto-char rlt)))
;; }}

(defun my-minibuffer-setup-hook ()
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard)
  ;; comment this which conflict with ido ido-kill-buffer key binding
  ;; (local-set-key (kbd "C-k") 'kill-line)
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  ;; evil-mode also use minibuf
  (setq gc-cons-threshold best-gc-cons-threshold))

;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; {{ string-edit-mode
(defun string-edit-at-point-hook-setup ()
  (let ((major-mode-list (remove major-mode '(web-mode js2-mode js-mode css-mode emacs-lisp-mode)))
        (str (my-buffer-str)))
    ;; (ivy-read "directories:" collection :action 'dired)
    ;; (message "original=%s" (se/find-original))
    ;; (message "major-mode-list=%s major-mode=%s" major-mode-list major-mode)
    (save-excursion
      (cond
       ((string-match-p "<[a-zA-Z]" str)
        (web-mode))
       ((string-match-p "function(\\| var \\|" str)
        (js-mode))))))
(add-hook 'string-edit-at-point-hook 'string-edit-at-point-hook-setup)
;; }}

;; Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
;; Press "q" in evil-mode or "C-c C-c" to exit the diff output buffer
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (let (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))

    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare"
  (interactive)
  (when (region-active-p)
    (let (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b`"))

(defun diff-region-compare-with-b ()
  "Compare current region with region selected by `diff-region-tag-selected-as-a' "
  (interactive)
  (if (region-active-p)
      (let (rlt-buf
            diff-output
            (fa (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory))))
            (fb (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory)))))
        ;;  save current content as file B
        (when fb
          (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
          (write-region (car tmp) (cadr tmp) fb))

        (when (and fa (file-exists-p fa) fb (file-exists-p fb))
          ;; save region A as file A
          (save-current-buffer
            (set-buffer (get-buffer-create "*Diff-regionA*"))
            (write-region (point-min) (point-max) fa))
          ;; diff NOW!
          ;; show the diff output
          (if (string= (setq diff-output (shell-command-to-string (format "diff -Nabur %s %s" fa fb))) "")
              ;; two regions are same
              (message "Two regions are SAME!")
            ;; show the diff
            (diff-region-open-diff-output diff-output
                                          "*Diff-region-output*")))

        ;; clean the temporary files
        (if (and fa (file-exists-p fa))
            (delete-file fa))
        (if (and fb (file-exists-p fb))
            (delete-file fb)))
    (message "Please select region at first!")))

;; cliphist.el
(setq cliphist-use-ivy t)

(defun pabs()
  "Relative path to full path."
  (interactive)
  (let* ((str (my-use-selected-string-or-ask "Input relative path:"))
         (path (file-truename str)))
    (copy-yank-str path)
    (message "%s => clipboard & yank ring" path)))

(defun prel()
  "Full path to relative path."
  (interactive)
  (let* ((str (my-use-selected-string-or-ask "Input absolute path:"))
         (path (file-relative-name str)))
    (copy-yank-str path)
    (message "%s => clipboard & yank ring" path)))

;; indention management
(defun my-toggle-indentation ()
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode=%s" indent-tabs-mode))

;; {{ auto-save.el
;; (require 'auto-save)
;; (auto-save-enable)
;; (setq auto-save-slient t)

;; (defun cancel-auto-save-buffers ()
;;   "cancel auto-save-buffers idle timer if found."
;;   (message "Try to cancel auto-save-buffers idle timer...")
;;   (let (timer-obj)
;;     (dolist (elt timer-idle-list timer-obj)
;;       (let* ((index-of-func 5)
;;              (find-func-name 'auto-save-buffers)
;;              (elt-func-name (aref elt index-of-func)))
;;         (if (eq find-func-name elt-func-name)
;;             (setq timer-obj elt))))
;;     (if (eq timer-obj nil)
;;         (message "Can not find auto-save-buffers idle timer, please checkout.")
;;       (cancel-timer timer-obj))))
;; }}


;; {{ regular expression tools
(defun my-create-regex-from-kill-ring (&optional n)
  "Create extended regex from first N items of `kill-ring'."
  (interactive "p")
  (when (and kill-ring (> (length kill-ring) 0))
    (if (> n (length kill-ring))
        (setq n (length kill-ring)))
    (let* ((rlt (mapconcat 'identity (subseq kill-ring 0 n) "|")))
      (setq rlt (replace-regexp-in-string "(" "\\\\(" rlt))
      (copy-yank-str rlt)
      (message (format "%s => kill-ring&clipboard" rlt)))))
;; }}

;; Highlight FIXME TODO BUG keywords
(use-package fic-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package ag
  :if (if (executable-find "ag")
          t
        (message "[Missing] try to install grep replacement \"ag\": pacman -S the_silver_searcher")
        nil))

(use-package auto-save-buffers-enhanced
  :init
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  (auto-save-buffers-enhanced t)

  (setq auto-save-buffers-enhanced-exclude-regexps '(".gpg"))

  :config
  (defun my-auto-save-buffers-enhanced-quiet-save-buffer ()
    (let ((save-silently t))
      (save-buffer)))

  (advice-add 'auto-save-buffers-enhanced-quiet-save-buffer
              :override #'my-auto-save-buffers-enhanced-quiet-save-buffer)

  ;; first version, the default method, add git directories to auto-save-buffers-enhanced-include-regexps, also include untracked files.
  ;; (auto-save-buffers-enhanced-include-only-checkout-path t)
  (defun my-auto-save-buffers-enhanced-add-checkout-path-into-include-regexps ()
    "fix duplicates issue of origin function"
    (let ((current-dir default-directory)
          (checkout-path nil))
      (catch 'root
        (while t
          (if (or (file-exists-p ".svn")
                  (file-exists-p ".cvs")
                  (file-exists-p ".git"))
              (setq checkout-path (expand-file-name default-directory)))
          (cd "..")
          (when (equal "/" default-directory)
            (throw 'root t))))

      (when checkout-path
        (let ((path (concat "^" (regexp-quote checkout-path))))
          ;; use member instead of memq here
          (if (not (member path auto-save-buffers-enhanced-include-regexps))
              (setq auto-save-buffers-enhanced-include-regexps
                    (cons path auto-save-buffers-enhanced-include-regexps)))))
      (cd current-dir)))

  (advice-add 'auto-save-buffers-enhanced-add-checkout-path-into-include-regexps
              :override #'my-auto-save-buffers-enhanced-add-checkout-path-into-include-regexps)

  ;; second version, auto save only git tracked files.
  (defun my-auto-save-buffers-enhanced-add-checkout-path-exclude-untracked ()
    (setq auto-save-buffers-enhanced-include-regexps nil)
    (dolist (buf (buffer-list))
      (let ((file (buffer-file-name buf))
            code)
        (when file
          (let ((default-directory (file-name-directory file)))
            (setq code (call-process "git" nil nil nil "ls-files" "--error-unmatch" file))
            (if (and (eq code 0)
                     (not (member file auto-save-buffers-enhanced-include-regexps)))
                (setq auto-save-buffers-enhanced-include-regexps
                      (cons file auto-save-buffers-enhanced-include-regexps))))))))
  (add-hook 'find-file-hook #'my-auto-save-buffers-enhanced-add-checkout-path-exclude-untracked))


(defun diff-no-select-gpg (func old new &optional switches no-async buf)
  "View the differences between BUFFER and its associated file.

See `diff-buffer-with-file', the function is very useful for viewing the difference before save to disk, and this advice used to supplement the gpg differentiate when the file name is .gpg suffixed."
  (unless (bufferp old)
    (if (string-suffix-p ".gpg" old)
        (let ((tempfile (make-temp-file "buffer-content-")))
          (epa-decrypt-file old tempfile)
          (funcall func tempfile new switches no-async buf))
      (funcall func old new switches no-async buf))))
(advice-add 'diff-no-select :around #'diff-no-select-gpg)

(provide 'init-misc)
;;; init-misc.el ends here
