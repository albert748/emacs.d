;;; init-general.el --- general.el initialization

;;; Commentary:

;;; Code:

;; My frequently used commands are listed here
;; For example, for line like `"ef" 'end-of-defun`
;;   You can either press `,ef` or `M-x end-of-defun` to execute it

(use-package general
  :init (general-evil-setup)

  :config
  ;; {{ use `,` as leader key
  (general-nvmap :prefix ","
    "=" 'cnfonts-increase-fontsize
    "-" 'cnfonts-decrease-fontsize
    "bf" 'beginning-of-defun
    "bu" 'backward-up-list
    "bb" 'back-to-previous-buffer
    "ef" 'end-of-defun
    "mf" 'mark-defun
    ;; "em" 'erase-message-buffer
    "eb" 'eval-buffer
    "sd" 'sudo-edit
    "sc" 'shell-command
    "ee" 'eval-expression
    "aa" 'copy-to-x-clipboard ; used frequently
    "aw" 'ace-swap-window
    "af" 'ace-maximize-window
    "ac" 'aya-create
    "ae" 'aya-expand
    "zz" 'paste-from-x-clipboard ; used frequently
    "cy" 'strip-convert-lines-into-one-big-string
    "bs" '(lambda () (interactive) (goto-edge-by-comparing-font-face -1))
    "es" 'goto-edge-by-comparing-font-face
    "vj" 'my-validate-json-or-js-expression
    "mcr" 'my-create-regex-from-kill-ring
    "ntt" 'neotree-toggle
    "ntf" 'neotree-find ; open file in current buffer in neotree
    "ntd" 'neotree-project-dir
    "nth" 'neotree-hide
    "nts" 'neotree-show
    "fl" 'cp-filename-line-number-of-current-buffer
    "fn" 'cp-filename-of-current-buffer
    "fp" 'cp-fullpath-of-current-buffer
    "dj" 'dired-jump ;; open the dired from current file
    "ff" 'toggle-full-window ;; I use WIN+F in i3
    "ip" 'find-file-in-project
    "kk" 'find-file-in-project-by-selected
    "fd" 'find-directory-in-project-by-selected
    "trm" 'get-term
    "tff" 'toggle-frame-fullscreen
    "tfm" 'toggle-frame-maximized
    "ti" 'fastdef-insert
    "th" 'fastdef-insert-from-history
    ;; "ci" 'evilnc-comment-or-uncomment-lines
    ;; "cl" 'evilnc-comment-or-uncomment-to-the-line
    ;; "cc" 'evilnc-copy-and-comment-lines
    ;; "cp" 'evilnc-comment-or-uncomment-paragraphs
    ;; "epy" 'emmet-expand-yas
    ;; "epl" 'emmet-expand-line
    "rd" 'evilmr-replace-in-defun
    "rb" 'evilmr-replace-in-buffer
    "ts" 'evilmr-tag-selected-region ;; recommended
    "rt" 'evilmr-replace-in-tagged-region ;; recommended
    "tua" 'artbollocks-mode
    "cby" 'cb-switch-between-controller-and-view
    "cbu" 'cb-get-url-from-controller
    "ht" 'etags-select-find-tag-at-point ; better than find-tag C-]
    "hp" 'etags-select-find-tag
    "mm" 'counsel-bookmark-goto
    "mk" 'bookmark-set
    "yy" 'counsel-browse-kill-ring
    "gf" 'counsel-git-find-file
    "gc" 'counsel-git-find-file-committed-with-line-at-point
    "gl" 'counsel-git-grep-yank-line
    "gg" 'counsel-git-grep-in-project ; quickest grep should be easy to press
    "ga" 'counsel-git-grep-by-author
    "gm" 'counsel-git-find-my-file
    "gs" 'ffip-show-diff ; find-file-in-project 5.0+
    "gt" 'ggtags-find-tag-dwim
    "gr" 'ggtags-find-reference
    "gu" 'omnisharp-find-usages
    "gi" 'omnisharp-find-implementations
    "gd" 'omnisharp-go-to-definition
    "gR" 'omnisharp-rename
    ",i" 'omnisharp-current-type-information
    ",I" 'omnisharp-current-type-documentation
    ",." 'omnisharp-show-overloads-at-point
    ",m" 'omnisharp-navigate-to-current-file-member
    ",s" 'omnisharp-navigate-to-solution-member
    ",o" 'projectile-compile-project
    ",u" 'projectile-run-project
    ",r" 'recompile
    "sf" 'counsel-git-show-file
    "sh" 'my-select-from-search-text-history
    "df" 'counsel-git-diff-file
    "rjs" 'run-js
    "jsr" 'js-send-region
    "rmz" 'run-mozilla
    "rpy" 'run-python
    "rlu" 'run-lua
    "tci" 'toggle-company-ispell
    "kb" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
    "it" 'issue-tracker-increment-issue-id-under-cursor
    "ls" 'highlight-symbol
    "lq" 'highlight-symbol-query-replace
    "ln" 'highlight-symbol-nav-mode ; use M-n/M-p to navigation between symbols
    "bm" 'pomodoro-start ;; beat myself
    "ii" 'counsel-imenu-goto
    "im" 'ido-imenu
    "ij" 'rimenu-jump
    "." 'evil-ex
    ;; @see https://github.com/pidu/git-timemachine
    ;; p: previous; n: next; w:hash; W:complete hash; g:nth version; q:quit
    "tt" 'my-git-timemachine
    "tdb" 'tidy-buffer
    "tdl" 'tidy-current-line
    ;; toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
    "ov" 'my-overview-of-current-buffer
    "or" 'open-readme-in-git-root-directory
    "od" 'org-decrypt-entries
    "c$" 'org-archive-subtree ; `C-c $'
    ;; org-do-demote/org-do-premote support selected region
    "c<" 'org-do-promote ; `C-c C-<'
    "c>" 'org-do-demote ; `C-c C->'
    "cam" 'org-tags-view ; `C-c a m': search items in org-file-apps by tag
    "cxi" 'org-clock-in ; `C-c C-x C-i'
    "cxo" 'org-clock-out ; `C-c C-x C-o'
    "cxr" 'org-clock-report ; `C-c C-x C-r'
    "qq" 'my-grep
    "xc" 'save-buffers-kill-terminal
    "rr" 'counsel-recentf-goto
    "rh" 'counsel-yank-bash-history ; bash history command => yank-ring
    "rf" 'counsel-goto-recent-directory
    "da" 'diff-region-tag-selected-as-a
    "db" 'diff-region-compare-with-b
    "di" 'evilmi-delete-items
    "si" 'evilmi-select-items
    "jb" 'js-beautify
    "jp" 'my-print-json-path
    "sep" 'string-edit-at-point
    "sec" 'string-edit-conclude
    "sea" 'string-edit-abort
    "xe" 'eval-last-sexp
    "xp" 'eval-print-last-sexp
    "x0" 'delete-window
    "x1" 'delete-other-windows
    "x2" 'split-window-vertically
    "x3" 'split-window-horizontally
    "rw" 'rotate-windows
    "ru" 'undo-tree-save-state-to-register ; C-x r u
    "rU" 'undo-tree-restore-state-from-register ; C-x r U
    "xt" 'toggle-window-split
    "uu" 'winner-undo
    "UU" 'winner-redo
    "to" 'toggle-web-js-offset
    "sl" 'sort-lines
    "ulr" 'uniquify-all-lines-region
    "ulb" 'uniquify-all-lines-buffer
    "lj" 'moz-load-js-file-and-send-it
    "mr" 'moz-console-clear
    "rnr" 'rinari-web-server-restart
    "rnc" 'rinari-find-controller
    "rnv" 'rinari-find-view
    "rna" 'rinari-find-application
    "rnk" 'rinari-rake
    "rnm" 'rinari-find-model
    "rnl" 'rinari-find-log
    "rno" 'rinari-console
    "rnt" 'rinari-find-test
    ;; "fs" 'ffip-save-ivy-last
    ;; "fr" 'ffip-ivy-resume
    ;; "fc" 'cp-ffip-ivy-last
    "ss" 'swiper-the-thing ; http://oremacs.com/2015/03/25/swiper-0.2.0/ for guide
    ;; "hst" 'hs-toggle-fold
    ;; "hsa" 'hs-toggle-fold-all
    ;; "hsh" 'hs-hide-block
    ;; "hss" 'hs-show-block
    "ogt" 'origami-toggle-node
    "oga" 'origami-toggle-all-nodes
    "ogc" 'origami-close-node
    "ogs" 'origami-show-node
    "fo" 'folding-open-buffer
    "fb" 'folding-whole-buffer
    "fs" 'folding-toggle-show-hide
    "fe" 'folding-toggle-enter-exit
    "fj" 'folding-next-visible-heading
    "fk" 'folding-previous-visible-heading
    "hd" 'describe-function
    "hf" 'find-function
    "hk" 'describe-key
    "hv" 'describe-variable
    "fb" 'flyspell-buffer
    "fn" 'flyspell-goto-next-error
    "fp" 'flyspell-goto-previous-error
    "fa" 'flyspell-auto-correct-word
    "ep" '(lambda () (interactive) (if (fboundp 'flycheck-previous-error) (flycheck-previous-error) (flymake-goto-prev-error)))
    "en" '(lambda () (interactive) (if (fboundp 'flycheck-next-error) (flycheck-next-error) (flymake-goto-next-error)))
    "el" 'flycheck-list-errors
    "pb" 'previous-buffer
    "nb" 'next-buffer
    "emm" 'emms
    "ems" 'emms-stop
    "emp" 'emms-pause
    "eml" 'emms-volume-lower
    "emr" 'emms-volume-raise
    "emn" 'emms-next
    "fw" 'ispell-word
    "bc" '(lambda () (interactive) (wxhelp-browse-class-or-api (thing-at-point 'symbol)))
    "oag" 'org-agenda
    "otl" 'org-toggle-link-display
    "om" 'toggle-org-or-message-mode
    "ut" 'undo-tree-visualize
    "ar" 'align-regexp
    "wrn" 'httpd-restart-now
    "wrd" 'httpd-restart-at-default-directory
    "bk" 'buf-move-up
    "bj" 'buf-move-down
    "bh" 'buf-move-left
    "bl" 'buf-move-right
    "0" 'delete-window
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3
    "4" 'select-window-4
    "5" 'select-window-5
    "6" 'select-window-6
    "7" 'select-window-7
    "8" 'select-window-8
    "9" 'select-window-9
    "xm" 'my-M-x
    "xx" 'er/expand-region
    "xf" 'ido-find-file
    "xb" 'ido-switch-buffer
    "xh" 'mark-whole-buffer
    "xk" 'ido-kill-buffer
    "xs" 'save-buffer
    "xz" 'suspend-frame
    "vm" 'vc-rename-file-and-buffer
    "vc" 'vc-copy-file-and-rename-buffer
    "xvv" 'vc-next-action ; 'C-x v v' in original
    "va" 'git-add-current-file
    "vk" 'git-checkout-current-file
    "vg" 'vc-annotate ; 'C-x v g' in original
    "vn" 'git-gutter+-next-hunk
    "vp" 'git-gutter+-previous-hunk
    "v=" 'git-gutter+-show-hunk
    "vr" 'git-gutter+-revert-hunks
    "vt" 'git-gutter+-stage-hunks
    "vT" 'git-gutter+-unstage-whole-buffer
    "vl" 'vc-print-log
    "vv" 'git-messenger:popup-message
    "hh" 'cliphist-paste-item
    "yu" 'cliphist-select-item
    "ih" 'my-goto-git-gutter ; use ivy-mode
    "ir" 'ivy-resume
    "nn" 'my-goto-next-hunk
    "pp" 'my-goto-previous-hunk
    "ww" 'narrow-or-widen-dwim
    "xnw" 'widen
    "xnd" 'narrow-to-defun
    "xnr" 'narrow-to-region
    "ycr" 'my-yas-reload-all
    "wf" 'popup-which-function
    "mg" 'magit-status
    "dc" 'desktop-clear
    "rst" 'restart-emacs)
  ;; }}

  ;; {{ Use `SPC` as leader key
  ;; all keywords arguments are still supported
  ;; (general-nvmap :prefix "SPC"
  ;;   "ss" 'wg-create-workgroup ; save windows layout
  ;;   "ll" 'my-wg-switch-workgroup ; load windows layout
  ;;   "kk" 'scroll-other-window
  ;;   "jj" 'scroll-other-window-up
  ;;   "yy" 'hydra-launcher/body
  ;;   "hh" 'multiple-cursors-hydra/body
  ;;   "tt" 'my-toggle-indentation
  ;;   "gs" 'git-gutter:set-start-revision
  ;;   "gh" 'git-gutter-reset-to-head-parent
  ;;   "gr" 'git-gutter-reset-to-default
  ;;   "ps" 'profiler-start
  ;;   "pr" 'profiler-report
  ;;   "ud" 'my-gud-gdb
  ;;   "uk" 'gud-kill-yes
  ;;   "ur" 'gud-remove
  ;;   "ub" 'gud-break
  ;;   "uu" 'gud-run
  ;;   "up" 'gud-print
  ;;   "ue" 'gud-cls
  ;;   "un" 'gud-next
  ;;   "us" 'gud-step
  ;;   "ui" 'gud-stepi
  ;;   "uc" 'gud-cont
  ;;   "uf" 'gud-finish
  ;;   "ma" 'mc/mark-all-like-this-dwim
  ;;   "md" 'mc/mark-all-like-this-in-defun
  ;;   "mm" 'ace-mc-add-multiple-cursors
  ;;   "mn" 'mc/mark-next-like-this
  ;;   "ms" 'mc/skip-to-next-like-this
  ;;   "me" 'mc/edit-lines)

  ;; per-major-mode leader setup
  (general-define-key :states '(normal motion insert emacs)
                      :keymaps 'js2-mode-map
                      :prefix "SPC"
                      :non-normal-prefix "M-SPC"
                      "de" 'js2-display-error-list
                      "nn" 'js2-next-error
                      "te" 'js2-mode-toggle-element
                      "tf" 'js2-mode-toggle-hide-functions
                      "jeo" 'js2r-expand-object
                      "jco" 'js2r-contract-object
                      "jeu" 'js2r-expand-function
                      "jcu" 'js2r-contract-function
                      "jea" 'js2r-expand-array
                      "jca" 'js2r-contract-array
                      "jwi" 'js2r-wrap-buffer-in-iife
                      "jig" 'js2r-inject-global-in-iife
                      "jev" 'js2r-extract-var
                      "jiv" 'js2r-inline-var
                      "jrv" 'js2r-rename-var
                      "jvt" 'js2r-var-to-this
                      "jag" 'js2r-add-to-globals-annotation
                      "jsv" 'js2r-split-var-declaration
                      "jss" 'js2r-split-string
                      "jef" 'js2r-extract-function
                      "jem" 'js2r-extract-method
                      "jip" 'js2r-introduce-parameter
                      "jlp" 'js2r-localize-parameter
                      "jtf" 'js2r-toggle-function-expression-and-declaration
                      "jao" 'js2r-arguments-to-object
                      "juw" 'js2r-unwrap
                      "jwl" 'js2r-wrap-in-for-loop
                      "j3i" 'js2r-ternary-to-if
                      "jlt" 'js2r-log-this
                      "jsl" 'js2r-forward-slurp
                      "jba" 'js2r-forward-barf
                      "jk" 'js2r-kill)
  ;; }}

  ;; {{ Use `;` as leader key, for searching something
  (general-nvmap :prefix ";"
    ";" 'avy-goto-char-2
    "di" 'youdao-dictionary-search-from-input
    "do" 'youdao-dictionary-search-at-point+
    "db" 'sdcv-search-pointer ; in buffer
    "dt" 'sdcv-search-input+ ;; in tip
    "dd" 'my-lookup-dict-org
    "dw" 'define-word
    "dp" 'define-word-at-point
    "mm" 'lookup-doc-in-man
    "gg" 'w3m-google-search
    "gf" 'w3m-google-by-filetype
    "gd" 'w3m-search-financial-dictionary
    "gj" 'w3m-search-js-api-mdn
    "ga" 'w3m-java-search
    "gh" 'w3mext-hacker-search ; code search in all engines with firefox
    "gq" 'w3m-stackoverflow-search
    "mm" 'mpc-which-song
    "mn" 'mpc-next-prev-song
    "mp" '(lambda () (interactive) (mpc-next-prev-song t)))
  ;; }}
  )

(provide 'init-general)
;;; init-general.el ends here
