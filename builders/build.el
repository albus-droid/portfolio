(require 'ox-html)
(require 'ox-publish)

(defun slurp (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq org-publish-project-alist
      (list
       (list "my-portfolio"
             :recursive t
             :base-directory "."
             :publishing-directory "."
             :base-extension "org"
             :publishing-function 'org-html-publish-to-html
             :html-head-include-default-style nil
             :html-head-include-scripts nil
             :html-head "<link rel=\"stylesheet\" href=\"style.css\"/>"
             :html-head-extra "<script src=\"typewriter-name.js\" defer></script>"
             :with-title nil
             :html-container "main"
             :html-preamble (slurp "layout-fragment.html"))))

(org-publish-all t)
(message "Build Complete!")
