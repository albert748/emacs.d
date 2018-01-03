;; FIXME: disable display-line-numbers-mode on paticular mode (eg:
;; term-mode) is not possible setting from here right now.
(use-package display-line-numbers
  :ensure nil
  :init
  (global-display-line-numbers-mode))

(provide 'init-display-line-numbers)