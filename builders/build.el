(require 'ox-html)
(require 'ox-publish)

(defun fetch-url-content (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    (buffer-string)))

(setq github-base-url "https://raw.githubusercontent.com/albus-droid/portfolio/refs/heads/master/templates/")
(setq css-url (concat github-base-url "style.css"))
(setq js-url (concat github-base-url "typewriter-name.js"))
(setq layout-fragment-url (concat github-base-url "layout-fragment.html"))

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
             :html-head (concat "<link rel=\"stylesheet\" href=\"" css-url "\"/>")
             :html-head-extra (concat "<script src=\"" js-url "\" defer></script>")
             :with-title nil
             :html-preamble (fetch-url-content layout-fragment-url))))


(org-publish-all t)
(message "Build Complete!")
