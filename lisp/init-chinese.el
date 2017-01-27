;; @see https://github.com/tumashu/chinese-pyim

(use-package chinese-pyim
  :config
  ;; 激活 basedict 拼音词库
  (use-package chinese-pyim-basedict
    :config (chinese-pyim-basedict-enable))

  (use-package chinese-pyim-greatdict
    :config (chinese-pyim-greatdict-enable))

  (setq default-input-method "chinese-pyim")
  (setq pyim-default-scheme 'quanpin)
  (setq pyim-page-tooltip 'popup) ; use popup.el for drawing
  (setq pyim-page-length 5)

  (setq pyim-isearch-enable-pinyin-search t)

  ;; FIXME: these varables are not used by newest chiense-pyim anymore.
  ;; (setq pyim-cache-directory (concat my-emacs-cache-directory "/pyim/cache"))
  ;; (setq pyim-personal-file (concat my-emacs-private-directory "/pyim/pyim-personal.txt"))
  ;; (setq pyim-property-file (concat my-emacs-private-directory "/pyim/pyim-words-property.txt"))

  (setq pyim-directory (concat my-emacs-private-directory "/pyim"))
  (setq pyim-dcache-directory (concat my-emacs-private-directory "/pyim/dcache"))

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))

  :bind
  (("M-j" . pyim-convert-code-at-point)
   ("C-;" . pyim-delete-word-from-personal-buffer)))


;; Init chinese pinyin
;; {{ make IME compatible with evil-mode
(defun evil-toggle-input-method ()
  "when toggle on input method, goto evil-insert-state. "
  (interactive)

  ;; load IME when needed, less memory footprint
  (unless (featurep 'chinese-pyim)
    (require 'chinese-pyim))

  ;; some guy don't use evil-mode at all
  (cond
   ((and (boundp 'evil-mode) evil-mode)
    ;; evil-mode
    (cond
     ((eq evil-state 'insert)
      (toggle-input-method))
     (t
      (evil-insert-state)
      (unless current-input-method
        (toggle-input-method))
      ))
    (if current-input-method (message "IME on!")))
   (t
    ;; NOT evil-mode
    (toggle-input-method)))
  )

(defadvice evil-insert-state (around evil-insert-state-hack activate)
  ad-do-it
  (if current-input-method (message "IME on!")))

(global-set-key (kbd "C-\\") 'evil-toggle-input-method)
;; }}

;; (setq pyim-punctuation-translate-p nil) ;; use western punctuation (ban jiao fu hao)

;; (eval-after-load 'chinese-pyim
;;   '(progn

     ;; Disable immediate association to avoid sluggish.
     ;; (setq pyim-enable-words-predict nil)

     ;; Disable chinese completion to avoid sluggish.
     ;; (setq pyim-company-complete-chinese-enable nil)

     ;; (require 'chinese-pyim-company)
     ;; (setq pyim-company-max-length 6)

     ;; {{ fuzzy pinyin setup
  ;;    (defun pyim-fuzzy-pinyin-adjust-shanghai ()
  ;;      "As Shanghai guy, I can't tell difference between:
  ;; - 'en' and 'eng'
  ;; - 'in' and 'ing'"
  ;;      (interactive)
  ;;      (cond
  ;;       ((string-match-p "[a-z][ei]ng?-.*[a-z][ei]ng?" pyim-current-key)
  ;;        ;; for two fuzzy pinyin characters, just use its SHENMU as key
  ;;        (setq pyim-current-key (replace-regexp-in-string "\\([a-z]\\)[ie]ng" "\\1" pyim-current-key)))
  ;;       (t
  ;;        ;; single fuzzy pinyin character
  ;;        (cond
  ;;         ((string-match-p "[ei]ng" pyim-current-key)
  ;;          (setq pyim-current-key (replace-regexp-in-string "\\([ei]\\)ng" "\\1n" pyim-current-key)))
  ;;         ((string-match-p "[ie]n[^g]*" pyim-current-key)
  ;;          (setq pyim-current-key (replace-regexp-in-string "\\([ie]\\)n" "\\1ng" pyim-current-key))))))
  ;;      (pyim-handle-string))

     ;; Comment out below line for default fuzzy algorithm,
     ;; or just `(setq pyim-fuzzy-pinyin-adjust-function nil)`
     ;; (setq pyim-fuzzy-pinyin-adjust-function 'pyim-fuzzy-pinyin-adjust-shanghai)
     ;; }}

     ;; ))


;; Init chinese fonts
(use-package chinese-fonts-setup
  :config
  (setq cfs-profiles-directory (concat my-emacs-private-directory "/chinese-fonts-setup/"))
  (setq cfs-use-system-type t)
  (chinese-fonts-setup-enable))


(provide 'init-chinese)
