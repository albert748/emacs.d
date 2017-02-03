(require 'package)

;; You can set it to `t' to use safer HTTPS to download packages
(defvar melpa-use-https-repo nil
  "By default, HTTP is used to download packages.
But you may use safer HTTPS instead.")

(if melpa-use-https-repo
    (setq package-archives
          '(;; uncomment below line if you need use GNU ELPA
            ;; ("gnu" . "https://elpa.gnu.org/packages/")
            ("org" . "http://orgmode.org/elpa/")  ; only http supported
            ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
            ("melpa" . "https://melpa.org/packages/")))
  (setq package-archives
        '(;; uncomment below line if you need use GNU ELPA
          ;; ("gnu" . "http://elpa.gnu.org/packages/")
          ("org" . "http://orgmode.org/elpa/")
          ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
          ("melpa" . "http://melpa.org/packages/"))))

(setq package-menu-hide-low-priority t)

(setq package-selected-packages
      '(ace-mc
        bbdb
        wgrep
        robe
        inf-ruby
        company ; I won't wait another 2 years for stable
        simple-httpd
        dsvn
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
        lua-mode
        tidy
        auto-compile
        packed
        gitconfig-mode
        w3m
        erlang
        workgroups2
        company-c-headers
        go-mode
        org
        org-plus-contrib
        fcitx                           ; need fcitx-remote support
        restclient
        magit
        powershell
        use-package
        smex
        auto-yasnippet
        expand-region
        fringe-helper
        gitignore-mode
        request
        rinari
        hydra
        define-word
        neotree
        git-link
        git-messenger
        git-gutter
        git-timemachine
        ;; paredit
        dictionary
        rvm
        nvm
        flymake-css
        flymake-ruby
        flymake-jslint
        flymake-coffee
        ;; coffee-mode
        yasnippet
        cliphist
        tagedit
        yagist
        writeroom-mode
        haml-mode
        scss-mode
        rainbow-delimiters
        js2-mode
        flyspell-lazy
        cpputils-cmake
        unfill
        emmet-mode
        page-break-lines
        less-css-mode
        ibuffer-vc
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
