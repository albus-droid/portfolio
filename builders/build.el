;; This script builds html files from org files
;; Load the publishing system
(require 'ox-publish)
(setq org-publish-project-alist
      ( list
	(list "my-portfolio"
	      :recursive t
	      :base-directory "."
	      :publishing-directory "."
	      :base-extension "org"
	      :publishing-function 'org-html-publish-to-html
              :html-head-include-default-style nil
              :html-head "<link rel=\"stylesheet\" href=\"style.css\"/>"
	      :html-head-extra "<script src=\"typewriter-name.js\" defer></script>")))

;; Generate the site output
(org-publish-all t)
(message "Build Complete!")
