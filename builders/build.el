(require 'ox-html)
(require 'ox-publish)

(defun slurp (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq org-export-with-smart-quotes t)
(setq org-publish-project-alist
      (list
       (list "my-portfolio"
             :recursive t
             :base-directory "./contents"
             :publishing-directory "./dist"
             :base-extension "org"
             :publishing-function 'org-html-publish-to-html
             :html-head-include-default-style nil
             :html-head-include-scripts nil
             :html-head "<link rel=\"stylesheet\" href=\"/templates/style.css\"/>"
             :html-head-extra "<script src=\"/templates/typewriter-name.js\" defer></script>"
             :with-title nil
             :html-preamble (slurp "/home/albin/portfolio/templates/layout-fragment.html"))))

(org-publish-all t)
(message "Build Complete!")
