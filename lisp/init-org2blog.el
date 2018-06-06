(use-package org2blog
  :config

  (require 'auth-source)
  (let (credentials)
    ;; only required if your auth file is not already in the list of auth-sources
    (add-to-list 'auth-sources "~/.authinfo")
    (setq credentials (auth-source-user-and-password "wordpress"))
    (setq org2blog/wp-blog-alist
          `(("absz"
             :url "https://absz.me/xmlrpc.php"
             :username ,(car credentials)
             :password ,(cadr credentials)))))

  ;; 代码显示优化， wordpress 需要安装 SyntaxHighter 插件
  (setq org2blog/wp-use-sourcecode-shortcode t)

  (defcustom org-wp-shortcode-tag "cc"
    "default shortcode tag 'sourcecode' is used for SyntaxHighter
plugin which not support lisp, you need install CodeColorer
plugin for this tweak."
    :group 'org-export-wp
    :type 'string)

  (defun my-org-wp-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to WP HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((lang (org-element-property :language src-block))
        (caption (org-export-get-caption src-block))
        (label (let ((lbl (org-element-property :name src-block)))
                 (if (not lbl) ""
                   (format " id=\"%s\""
                           (org-export-get-reference lbl info)))))
        (sc (plist-get info :wp-shortcode))
        (langs-map (plist-get info :wp-shortcode-langs-map))
        (syntaxhl (org-export-read-attribute :attr_wp src-block :syntaxhl)))

    (if (not sc)
        (org-html-src-block src-block contents info)
      (format "[%s lang=\"%s\" title=\"%s\" %s]\n%s[/%s]"
              (or org-wp-shortcode-tag "sourcecode")
              (or (cdr (assoc lang langs-map)) (when lang (downcase lang)) "text")
              (or caption "")
              (or syntaxhl "")
              (org-export-format-code-default src-block info)
              (or org-wp-shortcode-tag "sourcecode")))))

  (advice-add 'org-wp-src-block :override #'my-org-wp-src-block))

(provide 'init-org2blog)
