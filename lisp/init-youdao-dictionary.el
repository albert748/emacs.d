;; @see https://github.com/xuchunyang/youdao-dictionary.el

(use-package youdao-dictionary
  :bind ("C-c u" . youdao-dictionary-search-at-point)
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


(provide 'init-youdao-dictionary)