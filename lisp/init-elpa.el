;;; init-elpa.el --- initialize packages

;;; Commentary:

;;; Code:

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("elpa" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(setq package-menu-hide-low-priority t)

(setq package-selected-packages
      '(
        ;; essential packages
        use-package
        yasnippet
        restart-emacs
        sudo-edit
        which-key

        org
        org-plus-contrib
        org2blog
        org-download
        org-mind-map
        org-pomodoro
        ob-async

        company
        company-lua
        company-anaconda
        company-c-headers

        ;; paredit
        smartparens
        origami
        projectile
        flycheck
        flymake-css
        flymake-ruby
        flymake-jslint
        flymake-coffee
        flymake-lua

        elisp-def
        ggtags

        ;; languages
        slime
        lua-mode
        go-mode
        groovy-mode
        js2-mode
        omnisharp
        ;; python
        ein
        elpy
        ob-ipython
        anaconda-mode

        ;; themes
        zenburn-theme
        molokai-theme

        ;; variety of editing modes
        nginx-mode
        vimrc-mode
        markdown-mode
        yaml-mode
        cmake-mode
        pkgbuild-mode
        systemd
        csv-mode
        ssh-config-mode

        ;; functional helper modes
        fic-mode
        artbollocks-mode
        textile-mode
        pomodoro

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
        ;; evil-nerd-commenter

        counsel
        flx-ido

        avy
        ace-link

        ace-mc
        ace-window

        ;; chinese
        ace-pinyin
        pyim
        posframe
        pyim-basedict
        cnfonts
        youdao-dictionary
        fcitx                           ; need fcitx-remote support

        pdf-tools
        powerline

        emms
        diminish

        bbdb
        wgrep
        robe
        inf-ruby
        simple-httpd
        move-text
        string-edit ; looks magnars don't update stable tag frequently
        findr
        noflet
        db
        creole
        web
        idomenu
        buffer-move
        regex-tool
        legalese
        htmlize
        scratch
        session
        multi-term
        inflections
        auto-compile
        packed
        w3m
        ;; erlang
        workgroups2

        ;; version control
        magit
        git-link
        git-messenger
        git-gutter+
        git-gutter-fringe+
        git-timemachine
        gitconfig-mode
        gitignore-mode
        yagist
        ibuffer-vc
        dsvn

        dockerfile-mode
        restclient
        smex
        auto-yasnippet
        expand-region
        fringe-helper
        rinari
        hydra
        define-word
        neotree
        dictionary
        rvm
        nvm

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
        find-file-in-project
        exec-path-from-shell
        js-doc
        js-comint
        js2-refactor
        window-numbering
        keyfreq
        names))

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
;;; init-elpa.el ends here
