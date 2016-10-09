;; @see https://github.com/xuchunyang/youdao-dictionary.el

(with-eval-after-load 'youdao-dictionary
  ;; Enable Cache
  (setq url-automatic-caching t)

  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  ;; (push "*Youdao Dictionary*" popwin:special-display-config)

  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file (concat my-emacs-private-directory "emacs.youdao.history"))

  ;; Enable Chinese word segmentation support (支持中文分词)
  ;; (setq youdao-dictionary-use-chinese-word-segmentation t)

  )


(provide 'init-youdao-dictionary)