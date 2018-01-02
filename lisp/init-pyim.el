(use-package pyim
  :bind
  (("M-j" . pyim-convert-code-at-point)
   ;; FIXME: this function not work for me
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
  ;; (setq-default pyim-punctuation-translate-p '(no yes auto))

  ;; or use the default behavior the author used:
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  (if (version< emacs-version "26.0")
      (setq pyim-page-tooltip 'popup)
    (setq pyim-page-tooltip 'child-frame))

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  ;; my-pyim-probe-dynamic-english-half-punctuation
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (defun my-pyim-probe-dynamic-english-half-punctuation ()
    "like pyim-probe-dynamic-english, but used to for half width punctuation.

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

  ;; (pyim-isearch-mode 1)

  (setq pyim-page-length 5))


(provide 'init-pyim)
;;; init-pyim ends here