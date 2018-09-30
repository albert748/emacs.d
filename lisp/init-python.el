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
  (if (not (file-exists-p "~/anaconda3/envs/emacs"))
      (message "[Missing] It's recommanded to install anaconda and create \"emacs\" env for python mode")
    (setenv "WORKON_HOME" "~/anaconda3/envs"))

  :config
  (setq python-indent-guess-indent-offset nil)

  (use-package elpy
    :init
    (defun elpy-mode-hook-company-setup ()
      (setq-local company-backends
                  (cons '(elpy-company-backend
                          company-dabbrev-code
                          company-files
                          company-yasnippet)
                        (delq 'elpy-company-backend company-backends))))

    (add-hook 'elpy-mode-hook #'elpy-mode-hook-company-setup)

    (pyvenv-workon "emacs")

    ;; run command `pip install jedi autopep8 yapf black flake8` in virtualenv,
    ;; or check https://github.com/jorgenschaefer/elpy
    (elpy-enable)

    :config
    ;; yapf always timeout when format large size source, set timeout much
    ;; more bigger is safe.
    (setq elpy-rpc-timeout 3)

    ;; Jupyter console is recommended instead of ipython or default python shell
    ;; refer: https://elpy.readthedocs.io/en/latest/ide.html?highlight=rope#interactive-python
    (if (equal (executable-find "jupyter") (format "%s/bin/jupyter" pyvenv-virtual-env))
        (progn (setq python-shell-interpreter "jupyter"
                     python-shell-interpreter-args "console --simple-prompt"
                     python-shell-prompt-detect-failure-warning nil)
               (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))
      (message "[Missing] Jupyter console is not installed on env, Try to install: pip install jupyter"))

    ;; MacOS use python2 as default, set python version explicitly.
    (if (eq system-type 'darwin)
        (setq elpy-rpc-python-command "python3")))

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
