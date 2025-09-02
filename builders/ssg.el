;;; ssg.el --- Org based SSG -*- lexical-binding:t; -*-

;; Usage:
;;   emacs -Q --batch -l ssg.el
;;
;; Layout:
;;   content/            <- site pages (index.org, projects.org, ...)
;;   content/posts/      <- blog posts (*.org) with #+TITLE, #+DATE, (# +TAGS optional)
;;   assets/             <- static files (images, js, css)
;;   dist/               <- generated site
;;
;; Output:
;;   dist/<page>.html
;;   dist/blog/<slug>.html
;;   dist/blog/index.html
;;   dist/feed.xml

(require 'org)
(require 'ox-html)
(require 'subr-x)
(require 'time-date)

(defconst ssg-srcdir   (expand-file-name "content"))
(defconst ssg-postdir  (expand-file-name "posts" ssg-srcdir))
(defconst ssg-outdir   (expand-file-name "dist"))
(defconst ssg-blogdir  (expand-file-name "blog" ssg-outdir))
(defconst ssg-assets   (expand-file-name "assets"))
(make-directory ssg-outdir t)
(make-directory ssg-blogdir t)

;; ---------------- Theme ----------------
(defconst ssg-css
  "<style>
:root{--bg:#0b0c10;--card:#111217;--muted:#9aa0aa;--fg:#e6e8eb;--accent:#7c5cff;--link:#86c5ff;--pill:#1a1c23}
*{box-sizing:border-box}
html,body{margin:0;padding:0;background:var(--bg);color:var(--fg);font:15px/1.6 Inter,system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif}
a{color:var(--link);text-decoration:none} a:hover{text-decoration:underline}
.wrap{max-width:980px;margin:0 auto;padding:28px 20px}
.nav{display:flex;gap:18px;align-items:center;justify-content:center;padding:12px 0;margin-bottom:12px;position:sticky;top:0;background:rgba(11,12,16,.7);backdrop-filter:saturate(150%) blur(8px);border-bottom:1px solid #1f212a}
.nav a{font-weight:600}
.hero{display:flex;flex-direction:column;gap:10px;align-items:center;text-align:center;padding:22px 0 4px}
.name{font-size:34px;font-weight:800;letter-spacing:.3px}
.sub{color:var(--muted)}
.contact{display:flex;flex-wrap:wrap;gap:12px;justify-content:center;margin-top:6px}
.badge{padding:6px 10px;border-radius:999px;background:var(--pill);border:1px solid #21232d;font-size:14px}
h2{font-size:22px;margin:24px 0 10px;border-bottom:1px solid #1f212a;padding-bottom:6px}
.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:14px}
.card{background:var(--card);border:1px solid #1f212a;border-radius:16px;padding:14px}
.card h3{margin:6px 0 6px;font-size:18px}
.chip{display:inline-block;padding:4px 8px;border-radius:999px;background:#161821;border:1px solid #242736;margin:2px 4px 0 0;font-size:12px;color:#c9ced6}
.list{list-style:none;padding-left:0;margin:0} .list li{margin:8px 0}
.kvs{display:grid;grid-template-columns:140px 1fr;gap:6px}
.post-meta{color:var(--muted);font-size:14px;margin:6px 0 14px}
.post-list{display:grid;gap:12px}
.post-item{padding:14px;border:1px solid #1f212a;border-radius:12px;background:var(--card)}
.post-item h3{margin:0 0 6px}
footer{color:var(--muted);text-align:center;margin:28px 0}
@media print{.wrap{padding:0 16px} a{color:inherit}}
</style>")

(defconst ssg-head-fonts
  "<link rel='preconnect' href='https://fonts.googleapis.com'>
<link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>
<link href='https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap' rel='stylesheet'>")

;; -------------- Helpers --------------
(defun ssg--slug (s)
  (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" (string-trim (or s "")))))

(defun ssg--collect-top-headings (info)
  "Return ( (id . title) ... ) for level-1 headings."
  (let* ((tree (plist-get info :parse-tree)))
    (org-element-map tree 'headline
      (lambda (h)
        (when (= 1 (org-element-property :level h))
          (let* ((raw (org-element-property :raw-value h))
                 (cid (org-element-property :CUSTOM_ID h))
                 (id  (or cid (ssg--slug raw))))
            (cons id raw)))))
      nil nil 'headline))

(defun ssg--nav-html (pairs)
  (format "<nav class='nav'>%s <a href='/blog/'>Blog</a></nav>"
          (mapconcat (lambda (pr)
                       (format "<a href='#%s'>%s</a>" (car pr) (cdr pr)))
                     pairs " ")))

(defun ssg--inject-ids-into-headings (contents pairs)
  "Ensure each H2 has the id from pairs so nav anchors work."
  (let ((out contents))
    (dolist (pr pairs out)
      (let* ((id (car pr)) (title (regexp-quote (cdr pr))))
        (setq out
              (replace-regexp-in-string
               (format "<h2[^>]*>%s</h2>" title)
               (format "<h2 id='%s'>%s</h2>" id (cdr pr))
               out t t))))
    out))

(defun ssg--read-prop (info key)
  (org-export-data (or (plist-get info key) "") info))

(defun ssg--estimate-reading-mins (text)
  "Approx words/200."
  (let* ((w (length (split-string (or text "") "\\W+" t))))
    (max 1 (ceiling (/ w 200.0)))))

(defun ssg--parse-date (datestr)
  "Return (time . yyyy-mm-dd) from Org #+DATE:; tolerate many formats."
  (when (and datestr (not (string-empty-p datestr)))
    (condition-case nil
        (let* ((tobj (apply #'encode-time (parse-time-string datestr)))
               (fmt  (format-time-string "%Y-%m-%d" tobj)))
          (cons tobj fmt))
      (error nil))))

;; -------------- Templates --------------
(defun ssg--page-template (contents info)
  "Template for normal pages."
  (let* ((title (or (plist-get info :title) "Untitled"))
         (author (plist-get info :author))
         (desc (or (plist-get info :description) ""))
         (pairs (ssg--collect-top-headings info))
         (nav (ssg--nav-html pairs))
         (body (ssg--inject-ids-into-headings contents pairs)))
    (concat
     "<!doctype html><html><head><meta charset='utf-8'>"
     "<meta name='viewport' content='width=device-width, initial-scale=1'>"
     ssg-head-fonts ssg-css
     "<title>" (org-export-data title info) "</title>"
     (when (and desc (not (string-empty-p desc)))
       (concat "<meta name='description' content='" (org-export-data desc info) "'>"))
     "</head><body>"
     nav
     "<main class='wrap'>"
     (when (or author (not (string-empty-p desc)))
       (format "<section class='hero'><div class='name'>%s</div>%s%s</section>"
               (org-export-data title info)
               (if author (format "<div class='sub'>%s</div>" (org-export-data author info)) "")
               (if (not (string-empty-p desc))
                   (format "<div class='sub'>%s</div>" (org-export-data desc info)) "")))
     body
     "<footer>© <span id='y'></span> "
     (org-export-data (or author "Your Name") info)
     " · Built with Org + ssg.el"
     "<script>document.getElementById('y').textContent=new Date().getFullYear()</script>"
     "</footer></main></body></html>")))

(defun ssg--post-template (contents info)
  "Template for individual blog posts."
  (let* ((title (or (plist-get info :title) "Untitled"))
         (author (or (plist-get info :author) ""))
         (desc (or (plist-get info :description) ""))
         (raw (buffer-substring-no-properties (point-min) (point-max)))
         (mins (ssg--estimate-reading-mins raw))
         (datestr (or (plist-get info :date) ""))
         (datepair (ssg--parse-date datestr))
         (datehtml (if datepair (cdr datepair) "")))
    (concat
     "<!doctype html><html><head><meta charset='utf-8'>"
     "<meta name='viewport' content='width=device-width, initial-scale=1'>"
     ssg-head-fonts ssg-css
     "<title>" (org-export-data title info) " — Blog</title>"
     (when (and desc (not (string-empty-p desc)))
       (concat "<meta name='description' content='" (org-export-data desc info) "'>"))
     "</head><body>"
     "<nav class='nav'><a href='/'>Home</a> <a href='/blog/'>Blog</a></nav>"
     "<main class='wrap'>"
     (format "<h1 class='name' style='font-size:28px'>%s</h1>" (org-export-data title info))
     (format "<div class='post-meta'>%s%s%s</div>"
             (if (and datehtml (not (string-empty-p datehtml)))
                 (format "Published %s" datehtml) "")
             (if (> mins 0) (format " · %d min read" mins) "")
             (if (and author (not (string-empty-p author)))
                 (format " · %s" (org-export-data author info)) ""))
     contents
     "<footer>© <span id='y'></span> "
     (org-export-data (or author "Your Name") info)
     " · Built with Org + ssg.el"
     "<script>document.getElementById('y').textContent=new Date().getFullYear()</script>"
     "</footer></main></body></html>")))

;; Tune HTML exporter (shared)
(setq org-html-html5-fancy t
      org-html-postamble nil
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-validation-link nil
      org-export-with-toc nil
      org-export-with-section-numbers nil
      org-html-checkbox-type 'unicode)

;; We'll override template dynamically: normal pages use ssg--page-template,
;; posts use ssg--post-template.
(defun ssg--with-template (template fn)
  (let ((sym 'org-html-template))
    (unwind-protect
        (progn (advice-add sym :override template) (funcall fn))
      (advice-remove sym template))))

;; -------------- Build normal pages --------------
(unless (file-directory-p ssg-srcdir)
  (user-error "Create a 'content/' folder with your .org files"))

(let* ((files (seq-filter (lambda (f) (not (string-match-p "/posts/" f)))
                          (directory-files-recursively ssg-srcdir "\\.org\\'"))))
  (dolist (f files)
    (with-current-buffer (find-file-noselect f)
      (let* ((base (file-name-base f))
             (out  (expand-file-name (concat base ".html") ssg-outdir)))
        (message "Exporting page %s -> %s" f out)
        (ssg--with-template #'ssg--page-template
          (lambda () (org-export-to-file 'html out nil nil nil nil)))))))

;; -------------- Build posts --------------
(defun ssg--collect-posts ()
  "Return list of alists: (:file :title :date-time :date-str :desc :slug :out)."
  (let ((acc nil))
    (when (file-directory-p ssg-postdir)
      (dolist (f (directory-files ssg-postdir t "\\.org\\'"))
        (with-current-buffer (find-file-noselect f)
          (let* ((info (org-export-get-environment 'html))
                 (title (plist-get info :title))
                 (desc  (plist-get info :description))
                 (datestr (plist-get info :date))
                 (dp (ssg--parse-date datestr))
                 (tobj (car dp))
                 (dstr (cdr dp))
                 (slug (ssg--slug (or title (file-name-base f))))
                 (out (expand-file-name (concat slug ".html") ssg-blogdir)))
            (push (list :file f :title title :desc desc
                        :date-time tobj :date-str dstr :slug slug :out out)
                  acc))))))
    ;; newest first
    (seq-sort
     (lambda (a b)
       (let ((ta (plist-get a :date-time))
             (tb (plist-get b :date-time)))
         (cond
          ((and ta tb) (time-less-p tb ta))      ; newer first
          (ta t)
          (tb nil)
          (t (string-lessp (plist-get b :title) (plist-get a :title))))))
     acc)))

(let ((posts (ssg--collect-posts)))
  ;; Export each post
  (dolist (p posts)
    (with-current-buffer (find-file-noselect (plist-get p :file))
      (message "Exporting post %s -> %s" (plist-get p :file) (plist-get p :out))
      (ssg--with-template #'ssg--post-template
        (lambda () (org-export-to-file 'html (plist-get p :out) nil nil nil nil)))))

  ;; Blog index
  (let ((idx (expand-file-name "index.html" ssg-blogdir)))
    (with-temp-buffer
      (insert "#+TITLE: Blog\n#+AUTHOR: Albin Babu Varghese\n\n")
      (insert "* Posts\n")
      (insert "#+begin_export html\n<div class='post-list'>\n")
      (dolist (p posts)
        (let* ((url (format "/blog/%s.html" (plist-get p :slug)))
               (title (or (plist-get p :title) (plist-get p :slug)))
               (dstr (or (plist-get p :date-str) ""))
               (desc (or (plist-get p :desc) "")))
          (insert (format "<div class='post-item'>\n<h3><a href='%s'>%s</a></h3>\n" url (org-export-string-as title 'html t)))
          (when (not (string-empty-p dstr))
            (insert (format "<div class='post-meta'>%s</div>\n" dstr)))
          (when (not (string-empty-p desc))
            (insert (format "<p>%s</p>\n" (org-export-string-as desc 'html t))))
          (insert "</div>\n")))
      (insert "</div>\n#+end_export\n")
      (write-file (expand-file-name "_blog_index.org" ssg-srcdir)))
    ;; Export the generated index org with page template
    (with-current-buffer (find-file-noselect (expand-file-name "_blog_index.org" ssg-srcdir))
      (let ((out (expand-file-name "index.html" ssg-blogdir)))
        (ssg--with-template #'ssg--page-template
          (lambda () (org-export-to-file 'html out nil nil nil nil))))
      (delete-file (expand-file-name "_blog_index.org" ssg-srcdir))))

  ;; RSS feed
  (let ((rss (expand-file-name "feed.xml" ssg-outdir)))
    (with-temp-buffer
      (let ((now (format-time-string "%a, %d %b %Y %T %z")))
        (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
        (insert "<rss version=\"2.0\"><channel>\n")
        (insert "<title>Albin’s Blog</title>\n<link>https://YOUR_DOMAIN/</link>\n")
        (insert "<description>Posts by Albin Babu Varghese</description>\n")
        (insert (format "<lastBuildDate>%s</lastBuildDate>\n" now))
        (dolist (p posts)
          (let* ((title (or (plist-get p :title) (plist-get p :slug)))
                 (dstr (or (plist-get p :date-str) ""))
                 (url  (format "https://YOUR_DOMAIN/blog/%s.html" (plist-get p :slug))))
            (insert "<item>\n")
            (insert (format "<title>%s</title>\n" (org-export-string-as title 'html t)))
            (insert (format "<link>%s</link>\n" url))
            (when (not (string-empty-p dstr))
              (let ((tobj (car (ssg--parse-date dstr))))
                (when tobj (insert (format "<pubDate>%s</pubDate>\n" (format-time-string "%a, %d %b %Y %T %z" tobj))))))
            (insert (format "<guid>%s</guid>\n" url))
            (insert "</item>\n")))
        (insert "</channel></rss>\n"))
      (write-file rss))))

;; Copy assets
(when (file-directory-p ssg-assets)
  (let ((cmd (format "rsync -a '%s/' '%s/assets/'" ssg-assets ssg-outdir)))
    (shell-command cmd)))

(message "Site ready → %s" ssg-outdir)
