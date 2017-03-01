;; @see https://github.com/tumashu/chinese-pyim

(use-package chinese-pyim
  :bind
  (("M-j" . pyim-convert-code-at-point))

  :init
  (setq default-input-method "chinese-pyim")

  :config
  ;; 激活 basedict 拼音词库
  (use-package chinese-pyim-basedict
    :config (chinese-pyim-basedict-enable))

  (use-package chinese-pyim-greatdict
    :config (chinese-pyim-greatdict-enable))

  (setq pyim-default-scheme 'quanpin)

  ;; popup.el have performance issue, use pos-tip instead
  ;; (setq pyim-page-tooltip 'popup) ; use popup.el for drawing
  (setq pyim-page-tooltip 'pos-tip)
  (setq x-gtk-use-system-tooltips t) ; builtin emacs implementation have performance issue.

  ;; temprarily disable company-mode if input chinese
  (defun pyim-input-method-company-only-ascii (func key-or-string)
    (if (pyim-input-chinese-p)
        (company-cancel))
    (funcall func key-or-string))
  (advice-add 'pyim-input-method :around #'pyim-input-method-company-only-ascii)

  (setq pyim-page-length 5)

  ;; enable chinese pinyin search for isearch
  (setq pyim-isearch-enable-pinyin-search t)

  ;; FIXME: these varables are not used by newest chiense-pyim anymore.
  ;; (setq pyim-cache-directory (concat my-emacs-cache-directory "/pyim/cache"))
  ;; (setq pyim-personal-file (concat my-emacs-private-directory "/pyim/pyim-personal.txt"))
  ;; (setq pyim-property-file (concat my-emacs-private-directory "/pyim/pyim-words-property.txt"))

  (setq pyim-directory (concat my-emacs-private-directory "/pyim"))
  (setq pyim-dcache-directory (concat my-emacs-private-directory "/pyim/dcache"))

  (defun my-pyim-probe-dynamic-english ()
    "the same like pyim-probe-dynamic-english, but used to for half width punctuation.

the only difference is: not only check char before the point, but
also look back another 1 char position.  this make sense for half
width punctuation."
    (let ((str-before-1 (pyim-char-before-to-string 0))
          (str-before-2 (pyim-char-before-to-string 1)))
      (unless (string= (buffer-name) " *temp*") ; Make sure this probe can work with exim of exwm.
        (if (<= (point) (save-excursion (back-to-indentation)
                                        (point)))
            (not (or (string-match-p "\\cc" (save-excursion
                                                   ;; 查找前一个非空格字符。
                                                   (if (re-search-backward "[^[:space:]\n]" nil t)
                                                       (char-to-string (char-after (point))))))
                     (string-match-p "\\cc" (save-excursion
                                                   ;; 查找前一个非空格字符之前的字符。
                                                   (if (re-search-backward "[^[:space:]\n]" nil t)
                                                       (char-to-string (char-before (point))))))
                     (> (length pyim-entered-code) 0)))
          ;; at the middle of line
          (cond ((string-match-p "[[:space:]]" str-before-1))
                ((not (or (string-match-p "\\cc" str-before-1)
                          (string-match-p "\\cc" str-before-2)
                          (> (length pyim-entered-code) 0)))))))))


  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(my-pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template
                  pyim-probe-evil-normal-mode))

  ;; use half width style directly
  (setq-default pyim-punctuation-translate-p '(no yes auto))

  ;; or use auto detect probe functions
  ;; (setq-default pyim-punctuation-half-width-functions
  ;;               '(pyim-probe-punctuation-line-beginning
  ;;                 pyim-probe-punctuation-after-punctuation))

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t))))


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

;; (defadvice evil-insert-state (around evil-insert-state-hack activate)
;;   ad-do-it
;;   (if current-input-method (message "IME on!")))

;; (global-set-key (kbd "C-\\") 'evil-toggle-input-method)
;; }}

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
