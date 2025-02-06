;;; publish.el --- generate and publish my blog -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Thomas Ingram

;;; Commentary:

;; This file is cobbled togther from following many different blog posts and
;; examples for building an Org mode blog.  This setup works for me, and I
;; expect it may be helpful for others attempting the same.

;; https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

;;; Code:

(require 'org)
(require 'ox-publish)
(require 'htmlize)

(setq org-export-with-section-numbers nil
      org-export-with-toc nil

      ;; ISO8601 Date Format
      org-export-date-timestamp-format "%Y-%m-%d"
      org-html-metadata-timestamp-format "%Y-%m-%d"

      ;; Enable HTML5
      org-html-html5-fancy t
      org-html-doctype     "html5"

      ;; Disable default stylesheet and Javascript
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil

      ;; TODO: tweak headline id format. I dislike the randomly generated ones.
      ;; Look into customizing: :html-format-headline-function and
      ;; org-html-format-headline-function
      ;; org-html-self-link-headlines 'nil

      org-html-htmlize-output-type 'css)

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))

;; Render ~code~ as kbd tag in HTML
(add-to-list 'org-html-text-markup-alist '(code . "<kbd>%s</kbd>"))

(defvar taingram--head
  "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" media=\"(prefers-color-scheme: light)\" href=\"/modus-operandi.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" media=\"(prefers-color-scheme: dark)\" href=\"/modus-vivendi.css\" type=\"text/css\"/>
<script data-goatcounter=\"https://taingram.goatcounter.com/count\"
        async src=\"//gc.zgo.at/count.js\"></script>")

(defvar taingram--preamble
  "<div id=\"updated\">Updated: %C</div>
<header>
<h1 class=\"title\">%t</h1>
<div class=\"publish-date timestamp\">%d</div>
</header>")


(defun taingram--gen-footer (&optional comment)
  "Automatically generate footer text and optionally show COMMENT section."
  (concat
   (when comment
     "<div id=\"comments\">
<h2>Comments</h2>
<div id=\"text-comments\">
<p>Email comments and corrections to <a href=\"mailto:comment@taingram.org\">comment@taingram.org</a>.</p>
<p>Submissions may be posted publicly unless requested otherwise in your email.</p>
</div>
</div>")
   "<hr/>
<footer>
<div class=\"copyright-container\">
<div class=\"copyright\">
Copyright &copy; 2017-2025 Thomas Ingram. All rights reserved unless noted otherwise.</div></div>
<div class=\"banner\">
<a href=\"https://www.controlmywebsite.com/aff.php?aff=313\" rel=\"nofollow\" alt=\"Solar Powered Hosting By Viridio\">
<img src=\"https://cdn.viridio.net/affiliate/imgs/logo-54.png\" height=\"50%%\" width=\"50%%\" border=\"0\">
</a>
</div>
<div class=\"generated\">
Created with %c on <a href=\"https://www.debian.org/\">Debian</a> <a href=\"https://www.gnu.org\">GNU</a>/<a href=\"https://www.kernel.org/\">Linux</a>
</div>
</footer>"))


(defvar taingram--base-directory
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "org/")
  "The `:base-directory' for taingram site.")

(defvar taingram--preview-directory
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "html/")
  "The `:publishing-directory' for taingram-preview.")

(defvar taingram--publish-directory
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "html/")
  ;; OLD: "/ssh:thomas@taingram.org:/var/www/taingram.org/"
  "The `:publishing-directory' for taingram.org project.")

(defun taingram--sitemap-dated-entry-format (entry style project)
  "Sitemap PROJECT ENTRY STYLE format that includes date."
  (cond ((not (directory-name-p entry))
	 (let* ((file (org-publish--expand-file-name entry project))
		(title (org-publish-find-title entry project))
		(date (format-time-string "%Y-%m-%d"
					  (org-publish-find-date entry project)))
		(link (concat (file-name-sans-extension entry) ".html")))
	   (with-temp-buffer
	     (insert (format "{{{timestamp(%s)}}} [[file:%s][%s]]\n" date file title))
	     (buffer-string))))
	((eq style 'tree)
	 (file-name-nondirectory (directory-file-name entry)))
	(t entry)))


  ;; (concat "<rss version=\"2.0\">
;; <channel>
;;   <title>taingram.org Blog Posts</title>
;;   <link>https://taingram.org/blog/</link>
;;   <description>Liftoff to Space Exploration.</description>
;;   <language>en-us</language>
;; ")
  )




(defun taingram--sitemap-and-rss (title list)
  "Generate sitemap and RSS feed.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
‘org-list-to-lisp’.  PROJECT is the current project."
  (progn
    (taingram--sitemap-rss title list)
    (org-publish-sitemap-default title list)))

(setq org-publish-project-alist
      `(("index"
	 :base-directory ,taingram--base-directory
	 :base-extension "org"
	 :exclude ".*"
	 :include ("index.org")
	 :publishing-directory ,taingram--publish-directory
	 :publishing-function org-html-publish-to-html

	 :with-title nil
	 :html-head     ,taingram--head
	 :html-preamble ,taingram--preamble
	 :html-postamble ,(taingram--gen-footer))
	("pages"
	 :base-directory ,taingram--base-directory
	 :base-extension "org"
	 :exclude ,(regexp-opt '("index.org"  ".*-draft\.org"  "drafts/" "blog/"))

	 :html-link-home "https://taingram.org/"
	 :html-link-up "https://taingram.org/"
	 :html-home/up-format "<div id=\"org-div-home-and-up\"><a href=\"%s\">HOME</a></div>"

	 :recursive t
	 :publishing-directory ,taingram--publish-directory
	 :publishing-function org-html-publish-to-html

	 :with-title nil
	 :html-head     ,taingram--head
	 :html-preamble ,taingram--preamble
	 :html-postamble ,(taingram--gen-footer t))
	("blog"
	 :base-directory ,(concat taingram--base-directory "blog/")
	 :base-extension "org"
	 :exclude ".*-draft\.org"
	 :publishing-directory ,(concat taingram--publish-directory "blog/")
	 :publishing-function org-html-publish-to-html

	 :html-link-home "https://taingram.org/"
	 :html-link-up "https://taingram.org/blog"
	 :html-home/up-format "<div id=\"org-div-home-and-up\"><a href=\"%s\">Blog</a> <a href=\"%s\">Home</a></div>"

	 :auto-sitemap t
	 :sitemap-title "Blog Posts"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically
         :sitemap-format-entry taingram--sitemap-dated-entry-format
	 :sitemap-function taingram--sitemap-and-rss


	 :with-title nil
	 :html-head ,taingram--head
	 :html-preamble ,taingram--preamble
	 :html-postamble ,(taingram--gen-footer t))
	("blog-files"
	 :base-directory ,(concat taingram--base-directory "blog/files/")
	 :base-extension "png\\|jpg\\|jpeg\\|gif"
	 :recursive nil
	 :publishing-directory ,(concat taingram--publish-directory "blog/files/")
	 :publishing-function org-publish-attachment)
	("static"
	 :base-directory ,taingram--base-directory
	 :base-extension "css"
	 :include ("robots.txt" "profile.gif")
	 :recursive nil
	 :publishing-directory ,taingram--publish-directory
	 :publishing-function org-publish-attachment)
	("taingram.org" :components ("index" "static" "pages" "blog" "blog-files"))))

;; Uncomment to force full site regeneration
;; (org-publish "taingram.org" t)


;;; publish.el ends here
