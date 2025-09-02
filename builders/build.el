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
	      :publishing-function 'org-html-publish-to-html)))

;; Generate the site output
(org-publish-all t)
(message "Build Complete!")
