(require 'package)

;; You can set it to `t' to use safer HTTPS to download packages
(defvar melpa-use-https-repo nil
  "By default, HTTP is used to download packages.
But you may use safer HTTPS instead.")

;; for tsinghua mirror @see https://mirror.tuna.tsinghua.edu.cn/help/elpa/
;; and also @see https://elpa.emacs-china.org/index-en.html
;; FIXME: enable mirror site use toggle variable
(if melpa-use-https-repo
    (setq package-archives
          '(;; uncomment below line if you need use GNU ELPA
            ;; ("gnu" . "https://elpa.gnu.org/packages/")
            ;; ("org" . "http://orgmode.org/elpa/")  ; only http supported
            ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
            ;; ("melpa" . "https://melpa.org/packages/")
            ("ts-org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
            ("ts-melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	    ))
  (setq package-archives
        '(;; uncomment below line if you need use GNU ELPA
          ;; ("gnu" . "http://elpa.gnu.org/packages/")
          ;; ("org" . "http://orgmode.org/elpa/")
          ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
          ;; ("melpa" . "http://melpa.org/packages/")
          ("ts-org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
          ("ts-melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	  )))

(setq package-menu-hide-low-priority t)

(setq package-selected-packages
      '(
        ;; essential pacakges
        use-package
        company
        yasnippet
        restart-emacs
        dired+
        which-key
        smartparens
        origami

        ;; themes
        zenburn-theme
        molokai-theme

        ;; variety of editing modes
        vimrc-mode
        markdown-mode
        yaml-mode
        cmake-mode
        js2-mode
        crontab-mode
        pkgbuild-mode
        lua-mode
        systemd
        go-mode
        ;; groovy-mode
        ;; csharp-mode
        ;; powershell

        ;; functional helper modes
        fic-mode
        artbollocks-mode
        textile-mode

        ;; python
        ein
        elpy
        ob-ipython

        ;; evils
        evil
        evil-smartparens
        evil-matchit
        evil-visualstar
        evil-surround
        evil-exchange
        evil-mark-replace
        evil-escape
        evil-numbers
        evil-nerd-commenter

        counsel
        flx-ido

        avy
        ace-link

        ace-mc
        ace-window

        ;; chinese
        ace-pinyin
        chinese-pyim
        chinese-pyim-basedict
        chinese-pyim-greatdict
        chinese-fonts-setup
        youdao-dictionary
        fcitx                           ; need fcitx-remote support

        projectile

        ggtags
        pdf-tools
        powerline

        bbdb
        wgrep
        robe
        inf-ruby
        simple-httpd
        move-text
        string-edit ; looks magnars don't update stable tag frequently
        findr
        mwe-log-commands
        noflet
        db
        creole
        web
        idomenu
        buffer-move
        regex-tool
        quack
        legalese
        htmlize
        scratch
        session
        bookmark+
        flymake-lua
        multi-term
        inflections
        tidy
        auto-compile
        packed
        w3m
        ;; erlang
        workgroups2
        company-c-headers

        org
        org-plus-contrib
        org-download


        ;; version control
        magit
        git-link
        git-messenger
        git-gutter
        git-timemachine
        gitconfig-mode
        gitignore-mode
        yagist
        ibuffer-vc
        dsvn

        restclient
        smex
        auto-yasnippet
        expand-region
        fringe-helper
        rinari
        hydra
        define-word
        neotree
        ;; paredit
        dictionary
        rvm
        nvm
        flymake-css
        flymake-ruby
        flymake-jslint
        flymake-coffee
        ;; coffee-mode
        cliphist
        tagedit
        writeroom-mode
        haml-mode
        scss-mode
        rainbow-delimiters
        flyspell-lazy
        cpputils-cmake
        unfill
        emmet-mode
        page-break-lines
        less-css-mode
        hl-sexp
        find-file-in-project
        exec-path-from-shell
        js-doc
        js-comint
        js2-refactor
        window-numbering
        keyfreq
        names))

;;; force to install latest org
;;; FIXME: rewrite of site-lisp package looks impossible.
;;; So, please download org from http://orgmode.org/elpa/ manually for initial usage.
;;; extract the tarball to ~/.emacs.d/elpa/
(setq package-pinned-packages '((org . "org")
                               (org-plus-contrib . "org")))

(package-initialize)

(if (eq package-archive-contents nil)
    (package-refresh-contents))

(package-install-selected-packages)


;;------------------------------------------------------------------------------
;; Internal implementation, newbies should NOT touch code below this line!
;;------------------------------------------------------------------------------

;; Patch up annoying package.el quirks
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let* ((path (expand-file-name (concat
                                  ;; name is string when emacs <= 24.3.1,
                                  (if (symbolp name) (symbol-name name) name)
                                  "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))

;; Add support to package.el for pre-filtering available packages
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
      (funcall package-filter-function
         (car package)
         (funcall (if (fboundp 'package-desc-version)
          'package--ac-desc-version
        'package-desc-vers)
            (cdr package))
         archive))
    ad-do-it))

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (or (not (string-equal archive "melpa"))
            ;; (memq package melpa-include-packages)
            ;; use all color themes
            (string-match (format "%s" package) "-theme"))))

;; un-comment below code if you prefer use all the package on melpa (unstable) without limitation
(setq package-filter-function nil)

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------


(provide 'init-elpa)
