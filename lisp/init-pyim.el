(use-package pyim
  :bind
  (("M-j" . pyim-convert-code-at-point)
   ("C-;" . pyim-delete-word-from-personal-buffer))

  :init
  (setq pyim-directory (concat my-emacs-private-directory "pyim"))
  (setq pyim-dcache-directory (concat my-emacs-cache-directory "pyim/dcache"))

  :config
  (use-package pyim-basedict
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'quanpin)

  ;; only use half width punctuation even input chinese
  (setq pyim-punctuation-translate-p '(no yes auto))

  ;; (setq pyim-page-tooltip 'popup)
  (setq pyim-page-tooltip 'child-frame)

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

  ;; (pyim-isearch-mode 1)

  (setq pyim-page-length 5))


(provide 'init-pyim)