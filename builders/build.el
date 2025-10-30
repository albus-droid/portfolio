(require 'ox-html)
(require 'ox-publish)

(defun fetch-url-content (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    (buffer-string)))

(setq github-base-url "https://albus-droid.github.io/portfolio/")
(setq css-url (concat github-base-url "templates/style.css"))
(setq js-url (concat github-base-url "templates/typewriter-name.js"))
(setq highlight-js-url (concat github-base-url "templates/highlight-code.js"))
(setq layout-fragment-url (concat github-base-url "templates/layout-fragment.html"))

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
             :html-head-extra (concat "<script src=\"" js-url "\" defer></script>"
                                      "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/atom-one-dark.min.css\">"
                                      "<script src=\"" highlight-js-url "\" defer></script>")             
             :with-title t
             :html-preamble (fetch-url-content layout-fragment-url)
             :html-toplevel-hlevel 3)))


(org-publish-all t)
(message "Build Complete!")
