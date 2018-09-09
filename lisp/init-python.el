;;; init-python.el --- initialize elpy for python-mode

;;; Commentary:

;; To make company and eldoc work, virtualenv MUST be used for python.
;; It's recommended to use anaconda instead of origin python to manage
;; different python versions and different environments and use pip
;; for basic package management.

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)

  :init
  (if (and (not (executable-find "flake8"))
           (not (executable-find "pylint")))
      (message "[Missing] flycheck for python is disabled, try to install: pip install --user flake8 pylint"))

  ;; Jupyter console is recommended instead of ipython or default python shell
  ;; refer: https://elpy.readthedocs.io/en/latest/ide.html?highlight=rope#interactive-python
  (if (not (executable-find "jupyter"))
      (message "[Missing] Jupyter console is recommended, Try to install: pip install --user jupyter"))

  :config
  (defun my-elpy-company-setup ()
    (setq-local company-backends
                (cons '(elpy-company-backend company-yasnippet)
                      (delq 'elpy-company-backend company-backends))))

  (add-hook 'elpy-mode-hook #'my-elpy-company-setup)

  ;; anaconda-mode is not very useful. all features are covered by elpy.
  ;; also, there's bug which can not work with jupyter console
  (use-package anaconda-mode
    :disabled
    :init
    (add-hook 'python-mode-hook #'anaconda-mode)
    (add-hook 'python-mode-hook #'anaconda-eldoc-mode)

    :config
    (use-package company-anaconda
      :config
      (setq company-anaconda-case-insensitive nil)))

  (use-package elpy
    :init
    ;; run command `pip install --user jedi autopep8 yapf black flake8` in shell,
    ;; or check https://github.com/jorgenschaefer/elpy
    (elpy-enable)

    ;; there exist completion issue on rope, use jedi instead.
    ;; refer: https://github.com/jorgenschaefer/elpy/issues/631
    ;; rope have been removed from backend, elpy 1.17 only use jedi as backend
    ;; (setq elpy-rpc-backend "jedi")

    :config
    ;; yapf always timeout when format large size source, set timeout much
    ;; more bigger is safe.
    (setq elpy-rpc-timeout 3)

    (if (not (executable-find "jupyter"))
        nil
      (setq python-shell-interpreter "jupyter"
            python-shell-interpreter-args "console --simple-prompt"
            python-shell-prompt-detect-failure-warning nil)
      (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))

    ;; MacOS use python2 as default, set python version explicitly.
    (if (eq system-type 'darwin)
        (setq elpy-rpc-python-command "python3"))

    ;; (defun elpy-try-use-ipython ()
    ;;   (if (not (executable-find "ipython"))
    ;;       (message "Your need install ipython or Anaconda for elpy")

    ;;     ;; Fix ansi color issue (CSI codes) for ipython.
    ;;     ;; @see https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-09/msg00043.html
    ;;     (setq python-shell-interpreter-args "--simple-prompt -i")
    ;;     ))

    ;; (elpy-try-use-ipython)

    ;; looks like the issue already fixed with emacs 25.2.1
    ;; Fix issue completion issue of interactive shell for python 3.5.2
    ;; https://bugs.python.org/issue25660
    ;; https://github.com/jorgenschaefer/elpy/issues/887
    ;; https://github.com/emacs-mirror/emacs/commit/dbb341022870ecad4c9177485a6770a355633cc0
    ;; (defun ad-python-shell-completion-native-try ()
    ;;   "Return non-nil if can trigger native completion."
    ;;   (let ((python-shell-completion-native-enable t)
    ;;         (python-shell-completion-native-output-timeout
    ;;          python-shell-completion-native-try-output-timeout))
    ;;     (python-shell-completion-native-get-completions
    ;;      (get-buffer-process (current-buffer))
    ;;      nil "_")))
    ;; (advice-add 'python-shell-completion-native-try :after-until #'ad-python-shell-completion-native-try)

    )

  ;; org-babel functions for IPython evaluation
  (use-package ob-ipython)

  ;; ipython notebook support
  (use-package ein
    :config
    ;; Do not have to ask commands every time. also, don't forget to set `ein:jupyter-default-notebook-directory' as below:
    ;; (with-eval-after-load 'ein-jupyter
    ;;   (setq ein:jupyter-default-notebook-directory (expand-file-name "~/Sync/default/ein")))
    (setq ein:jupyter-default-server-command (executable-find "jupyter"))

    ;; you must have jupyter-console > 5.1 to make simple-prompt work.
    ;; @see https://github.com/jupyter/jupyter_console/issues/93
    (setq ein:console-args '("--simple-prompt"))))


(provide 'init-python)
;;; init-python.el ends here
