;;; init-dictionary.el --- dictionary search

;;; Commentary:

;;; Code:

;; @see https://github.com/xuchunyang/youdao-dictionary.el

(eval-when-compile (defvar my-emacs-private-directory))

(use-package youdao-dictionary
  :config
  ;; Enable Cache
  (setq url-automatic-caching t)

  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  ;; (push "*Youdao Dictionary*" popwin:special-display-config)

  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file (concat my-emacs-private-directory "/youdao.history"))

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  ;; Use emacs mode for query result buffer if evil in use
  (with-eval-after-load 'evil
    (push '("^\\*Youdao Dictionary\\*" . emacs) evil-buffer-regexps)))


(provide 'init-dictionary)
;;; init-dictionary.el ends here
