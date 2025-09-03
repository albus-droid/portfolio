;; build.el — Org→HTML using dist/layout.html + class/heading tweaks
(require 'ox-html) (require 'ox-publish)

(defun my/post (html)
  ;; Add your classes + visible hashes
  (setq html (replace-regexp-in-string "<div class=\"outline-2\\b" "<div class=\"section" html t t))
  (setq html (replace-regexp-in-string "<div class=\"outline-3\\b" "<div class=\"subsection" html t t))
  (setq html (replace-regexp-in-string "<h2>\\([^<]+\\)</h2>" "<h2># \\1</h2>" html t t))
  (setq html (replace-regexp-in-string "<h3>\\([^<]+\\)</h3>" "<h3>## \\1</h3>" html t t))
  html)

(defun my/publish (plist filename pub-dir)
  (let* ((layout (expand-file-name "layout.html" pub-dir))
         (base   (file-name-as-directory (plist-get plist :base-directory)))
         (rel    (file-relative-name filename base))
         (out    (expand-file-name (concat (file-name-sans-extension rel) ".html") pub-dir))
         ;; exporter opts
         (org-html-toplevel-hlevel 2)
         (org-export-with-toc nil)
         (org-export-with-section-numbers nil)
         (org-html-head-include-default-style nil)
         (org-html-head-include-scripts nil))
    (with-current-buffer (find-file-noselect filename)
      (let* ((env   (org-export-get-environment 'html))
             (title (or (plist-get env :title) (file-name-base filename)))
             (body  (my/post (org-export-as 'html nil nil t))) ; body-only
             (shell (if (file-exists-p layout)
                        (with-temp-buffer (insert-file-contents layout) (buffer-string))
                      "<!doctype html><meta charset='utf-8'><title>{{title}}</title><main>{{content}}</main>"))
             (html  (replace-regexp-in-string "{{content}}" body
                     (replace-regexp-in-string "{{title}}" title shell t t) t t)))
        (make-directory (file-name-directory out) t)
        (with-temp-file out (insert html))
        (message "Built %s" (file-relative-name out pub-dir))
        out))))

(setq org-publish-project-alist
      (list (list "my-portfolio"
                  :recursive t
                  :base-directory "contents"
                  :publishing-directory "dist"
                  :base-extension "org"
                  :publishing-function 'my/publish)))

(org-publish-all t)
(message "Build Complete!")
