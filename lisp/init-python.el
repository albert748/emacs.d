;;; init-python.el --- initialize elpy for python-mode

;;; Commentary:

;;; Code:

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
  (setq ein:console-args '("--simple-prompt"))
  )

(use-package elpy
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)

  :init
  ;; run command `pip install jedi flake8 importmagic autopep8 yapf` in shell,
  ;; or check https://github.com/jorgenschaefer/elpy
  (elpy-enable)

  :config
  (if (or (not (executable-find "flake8"))
          (not (executable-find "pylint")))
      (message "[Missing] You need install flake8 and python-pylint to enable flycheck for python"))

  ;; Jupyter console is recommended instead of ipython or default python shell
  ;; refer: https://elpy.readthedocs.io/en/latest/ide.html?highlight=rope#interactive-python
  (if (not (executable-find "jupyter"))
      (message "[Missing] You need install jupyter to enable better interactive support on python")
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"))

  ;; there exist completion issue on rope, use jedi instead.
  ;; refer: https://github.com/jorgenschaefer/elpy/issues/631
  ;; rope have been removed from backend, elpy 1.17 only use jedi as backend
  ;; (setq elpy-rpc-backend "jedi")

  ;; yapf always timeout when format large source, set timeout much
  ;; more bigger is safe.
  (setq elpy-rpc-timeout 3)

  ;; MacOS use python2 as default, set python version explicitly.
  (if (eq system-type 'darwin)
      (setq elpy-rpc-python-command "python3"))

  ;; enable yasnippet completion with company
  (advice-add 'elpy-modules-buffer-init :after
              (lambda ()
                (if (memq 'elpy-company-backend company-backends)
                    (setq company-backends (cons '(elpy-company-backend company-yasnippet) (delq 'elpy-company-backend company-backends))))))

  ;; (defun elpy-try-use-ipython ()
  ;;   (if (not (executable-find "ipython"))
  ;;       (message "Your need install ipython or Anaconda for elpy")

  ;;     ;; Fix ansi color issue (CSI codes) for ipython.
  ;;     ;; @see https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-09/msg00043.html
  ;;     (setq python-shell-interpreter-args "--simple-prompt -i")
  ;;     ))

  ;; (elpy-try-use-ipython)

  ;; looks like the issue already fixed with emacs 25.2.1
  ;;; Fix issue completion issue of interactive shell for python 3.5.2
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

  (defun python-mode-hook-setup ()
    (unless (is-buffer-file-temp)
      ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
      ;; emacs 24.4 only
      (setq electric-indent-chars (delq ?: electric-indent-chars))))

  (add-hook 'python-mode-hook 'python-mode-hook-setup))

(provide 'init-python)
;;; init-python.el ends here
