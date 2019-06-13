;;; init-folding.el -- folding related stuff

;;; Commentary:

;;; Code:

(use-package origami
  :disabled t
  :init
  (add-hook 'prog-mode-hook #'origami-mode)

  :config
  (setq origami-show-fold-header nil)

  (and (fboundp 'origami-markers-parser)
       (add-to-list 'origami-parser-alist `(lua-mode . ,(origami-markers-parser "{{{" "}}}")))))

(use-package folding
  :init
  (add-hook 'prog-mode-hook #'turn-on-folding-mode)
  :config
  (and (fboundp 'folding-add-to-marks-list)
       (folding-add-to-marks-list 'lua-mode "-- {{{" "-- }}}" nil t)))


(provide 'init-folding)
;;; init-folding ends here
