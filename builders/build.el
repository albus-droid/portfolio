;; This script builds html files from org files
;; Load the publishing system
(require 'ox-html)
(require 'ox-publish)

(defun )

(setq org-publish-project-alist
      ( list
	(list "my-portfolio"
	      :recursive t
	      :base-directory "contents"
	      :publishing-directory "dist"
	      :base-extension "org"
	      :publishing-function 'org-html-publish-to-html)))

;; Generate the site output
(org-publish-all t)
(message "Build Complete!")
