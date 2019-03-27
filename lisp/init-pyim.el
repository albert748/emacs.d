;;; init-pyim.el --- see https://github.com/tumashu/pyim

;;; Commentary:

;;; Code:

(eval-when-compile
  (defvar my-emacs-cache-directory)
  (defvar my-emacs-private-directory))

(use-package pyim
  :defer 3                              ; load when Emacs start
  :bind
  (("M-j". pyim-convert-code-at-point)
   ("M-f" . pyim-forward-word)
   ("M-b" . pyim-backward-word))

  :init
;;   (defvar my-pyim-english-input-switch-functions-old nil
;;     "The temporary postion to store `pyim-english-input-switch-functions'.")

;;   (defun my-pyim-convert-code-at-point (orig-func)
;;     "Activate pyim when not.
;; Default `pyim-convert-code-at-point' call `toggle-input-method' which makes circular invoke of the device function `my-toggle-input-method'."
;;     (interactive)
;;     (unless (equal input-method-function 'pyim-input-method)
;;         (activate-input-method 'pyim))
;;     (funcall orig-func))
;;   (advice-add 'pyim-convert-code-at-point :around #'my-pyim-convert-code-at-point)

;;   (defun my-pyim-toggle-input-method (&optional arg interactive)
;;     "Toggle pyim prober if `toggle-input-method' toggled.

;; This used as compatible advice function for traditional toggle
;; method. useful when do isearch use pyim, under which probe input
;; not work. 需要注意的是，evil 必须处于 insert 模式才能在 isearch
;; minibuffer 中输入中文, 这应该是一个 bug."
;;     ;; Do not try to determine probe mode or compatible mode use INTERACTIVE
;;     (when interactive
;;       (if (equal current-input-method 'pyim)
;;           (progn
;;             ;; store
;;             (setq my-pyim-english-input-switch-functions-old
;;                   pyim-english-input-switch-functions)
;;             (setq pyim-english-input-switch-functions nil)
;;             (setq-default pyim-english-input-switch-functions nil))

;;         ;; restore
;;         (setq pyim-english-input-switch-functions
;;               my-pyim-english-input-switch-functions-old)
;;         (setq-default pyim-english-input-switch-functions
;;                       my-pyim-english-input-switch-functions-old)
;;         (setq my-pyim-english-input-switch-functions-old nil))))

;;   (advice-add 'toggle-input-method :after #'my-pyim-toggle-input-method)


  :config
  (use-package pyim-basedict
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim"
        pyim-default-scheme 'quanpin
        pyim-page-length 5)

  (if (version< emacs-version "26.0")
      (setq pyim-page-tooltip 'popup)
    ;; package posframe should installed manually
    (setq pyim-page-tooltip 'posframe))

  ;; personal cache and dict
  (setq pyim-dcache-directory (expand-file-name "pyim/dcache" my-emacs-cache-directory))
  (setq pyim-dicts
        `((:name "personal" :file ,(expand-file-name "pyim/pyim-dict.txt" my-emacs-private-directory))))

  ;; personal fuzzy alist
  (setq pyim-fuzzy-pinyin-alist '(("z" "zh") ("ch" "c") ("sh" "s") ("l" "n")))

  ;; only use half width punctuation even input Chinese
  ;; (setq-default pyim-punctuation-translate-p '(no yes auto))

  ;; or use the default behavior the author used:
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

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

  ;; 注意：这个功能有一些限制，搜索字符串中只能出现 “a-z” 和 “’”，如果有其他字符（比如 regexp 操作符），则自动关闭拼音搜索功能。
  ;; 开启这个功能后，一些 isearch 扩展有可能失效，如果遇到这种问题，只能禁用这个 Minor-mode，然后联系 pyim 的维护者，看有没有法子实现兼容。
  ;; (pyim-isearch-mode 1)

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

  (defun pyim-dcache-export-personal-dcache-2 (file &optional confirm)
    "将 ‘pyim-dcache-icode2word’ 导出为 pyim 词库文件.

如果 FILE 为 nil, 提示用户指定导出文件位置, 如果 CONFIRM 为
non-nil，文件存在时将会提示用户是否覆盖，默认为覆盖模式"
    (interactive "F将个人缓存中的词条导出到文件：")
    (with-temp-buffer
      (insert ";;; -*- coding: utf-8-unix -*-\n")
      (maphash
       #'(lambda (key value)
           (insert (concat key " " (mapconcat #'identity value " ") "\n")))
       pyim-dcache-icode2word)
      (write-file file confirm)))
  (advice-add 'pyim-dcache-export-personal-dcache :override #'pyim-dcache-export-personal-dcache-2)

  ;; Not very useful at the time - 14.02.2019.
  ;; (use-package liberime
  ;;   :load-path "~/Sync/default/config/emacs/pyim/liberime/build/"
  ;;   :config
  ;;   (liberime-start "/usr/share/rime-data" (expand-file-name "~/.emacs.d/rime/"))
  ;;   (liberime-select-schema "luna_pinyin_simp")
  ;;   (setq pyim-default-scheme 'rime-quanpin))

  )


(provide 'init-pyim)
;;; init-pyim ends here
