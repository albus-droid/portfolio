;; build.el — run with: emacs -Q --script build.el
(require 'ox-html) (require 'ox-publish)

(defun slurp (p) (with-temp-buffer (insert-file-contents p) (buffer-string)))

(defun add-header-fragment (_backend)
  ;; Insert raw HTML block at the top of each Org file before export.
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (insert "#+OPTIONS: title:nil\n") ;; avoid Org's <h1>
      (insert "#+BEGIN_EXPORT html\n")
      (insert (slurp "layout-fragment.html"))  ;; your <h1 id=...> + social links
      (insert "\n#+END_EXPORT\n\n"))))

(add-hook 'org-export-before-processing-hook #'add-header-fragment)

(setq org-publish-project-alist
      (list
       (list "site"
             :recursive t
             :base-directory "contents"
             :publishing-directory "dist"
             :base-extension "org"
             :publishing-function 'org-html-publish-to-html
             :html-head-include-default-style nil
             :html-head-include-scripts nil
             :html-head "<link rel=\"stylesheet\" href=\"style.css\"/>"
             :html-head-extra "<script src=\"typewriter-name.js\" defer></script>"
             :html-container "main"       ;; single <main> wrapper
             :with-title nil)))

(org-publish "site" t)
(message "Build Complete!")
