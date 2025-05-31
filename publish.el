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


;; (setq org-export-date-timestamp-format "%Y-%m-%d")

(setq org-html-htmlize-output-type 'css)

;; ISO8601 Date Format
(setq org-html-metadata-timestamp-format "%Y-%m-%d")


(add-to-list 'org-export-global-macros
	     '("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@"))

(add-to-list 'org-export-global-macros
	     '("rss" . "@@html:<span class=\"rss-badge\"><svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 448 512\"><path d=\"M128.081 415.959c0 35.369-28.672 64.041-64.041 64.041S0 451.328 0 415.959s28.672-64.041 64.041-64.041 64.04 28.673 64.04 64.041zm175.66 47.25c-8.354-154.6-132.185-278.587-286.95-286.95C7.656 175.765 0 183.105 0 192.253v48.069c0 8.415 6.49 15.472 14.887 16.018 111.832 7.284 201.473 96.702 208.772 208.772.547 8.397 7.604 14.887 16.018 14.887h48.069c9.149.001 16.489-7.655 15.995-16.79zm144.249.288C439.596 229.677 251.465 40.445 16.503 32.01 7.473 31.686 0 38.981 0 48.016v48.068c0 8.625 6.835 15.645 15.453 15.999 191.179 7.839 344.627 161.316 352.465 352.465.353 8.618 7.373 15.453 15.999 15.453h48.068c9.034-.001 16.329-7.474 16.005-16.504z\"></path></svg></span>@@ $1"))



(defvar taingram--head
  "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" media=\"(prefers-color-scheme: light)\"
      href=\"/static/modus-operandi.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" media=\"(prefers-color-scheme: dark)\"
      href=\"/static/modus-vivendi.css\" type=\"text/css\"/>
<script data-goatcounter=\"https://taingram.goatcounter.com/count\"
        async src=\"//gc.zgo.at/count.js\"></script>")

(defvar taingram--preamble
  "<div id=\"updated\">Updated: %C</div>
<header>
<h1 class=\"title\">%t</h1>
</header>")

(defvar taingram--blog-preamble
  "<div id=\"updated\">Updated: %C</div>
<header>
<h1 class=\"title\">%t</h1>
<div class=\"publish-date\">Published: <span class=\"timestamp\">%d</span></div>
</header>")

(defun taingram--gen-footer (&optional comment)
  "Automatically generate footer text and optionally show COMMENT section."
  (concat
   (when comment
     "<div id=\"comments\">
<h2>Comments</h2>
<div id=\"text-comments\">
<p>Email <a href=\"mailto:comment@taingram.org\">comment@taingram.org</a>.</p>
</div>
</div>")
   "<hr/>
<footer>
<div class=\"copyright-container\">
<div class=\"copyright\">
Copyright &copy; 2017-2025 Thomas Ingram. All rights reserved unless otherwise noted.</div></div>
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
  "/ftp:taingram.org:/html/"
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


(setq org-publish-project-alist
      `(("index"
	 :base-directory ,taingram--base-directory
	 :include  ("index.org")
	 :exclude ".*"
	 :publishing-directory ,taingram--publish-directory
	 :publishing-function org-html-publish-to-html

	 :with-title nil
	 :with-toc nil
	 :section-numbers nil
	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-default-style nil
	 :html-head-include-scripts  nil
	 :html-head     ,taingram--head
	 :html-preamble ,taingram--preamble
	 :html-postamble ,(taingram--gen-footer))
	("pages"
	 :base-directory ,taingram--base-directory
	 :base-extension "org"
	 :exclude  "\\(index\\.org\\)"
	 :publishing-directory ,taingram--publish-directory
	 :publishing-function org-html-publish-to-html

	 :with-title nil
	 :with-toc nil
	 :section-numbers nil

	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-default-style nil
	 :html-head-include-scripts  nil
	 :html-link-home "https://taingram.org/"
	 :html-link-up "https://taingram.org/"
	 :html-home/up-format ,(concat "<div id=\"org-div-home-and-up\">"
				       "<a href=\"%s\">HOME</a></div>")
	 :html-head     ,taingram--head
	 :html-preamble ,taingram--preamble
	 :html-postamble ,(taingram--gen-footer t))
	("init"
	 :base-directory ,user-emacs-directory
	 :include ("init.org")
	 :exclude ".*"
	 :publishing-directory
	 :publishing-function org-html-publish-to-html

	 :with-title nil
	 :with-toc nil
	 :section-numbers nil
	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-default-style nil
	 :html-head-include-scripts  nil
	 :html-link-home "https://taingram.org/"
	 :html-link-up "https://taingram.org/"
	 :html-home/up-format (concat "<div id=\"org-div-home-and-up\">"
				      "<a href=\"%s\">HOME</a></div>")
	 :html-head     ,taingram--head
	 :html-preamble ,taingram--preamble
	 :html-postamble ,(taingram--gen-footer t))
	("blog"
	 :base-directory ,(concat taingram--base-directory "blog/")
	 :base-extension "org"

	 :publishing-directory ,(concat taingram--publish-directory "blog/")
	 :publishing-function  org-html-publish-to-html

	 :with-title nil
	 :with-toc nil
	 :section-numbers nil
	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-default-style nil
	 :html-head-include-scripts  nil
	 :html-link-home "https://taingram.org/"
	 :html-link-up "https://taingram.org/blog"
	 :html-home/up-format ,(concat "<div id=\"org-div-home-and-up\">"
				      "<a href=\"%s\">Blog</a>"
				      "<a href=\"%s\">Home</a></div>")
	 :html-head ,taingram--head
	 :html-preamble ,taingram--blog-preamble
	 :html-postamble ,(taingram--gen-footer t)

	 :auto-sitemap t
	 :sitemap-title "Blog Posts"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically
         :sitemap-format-entry taingram--sitemap-dated-entry-format

	 :auto-rss t
	 :rss-file "blog-rss.xml"
	 :rss-title "Thomas Ingram's Blog"
	 :rss-description "Blog posts on Emacs, GNU+Linux, etc."
	 :rss-with-content all
	 :completion-function org-publish-auto-rss)
	("blog-files"
	 :base-directory ,(concat taingram--base-directory "blog/")
	 :base-extension "png\\|jpg\\|jpeg\\|gif"
	 :include ("blog-rss.xml")
	 :recursive t
	 :publishing-directory ,(concat taingram--publish-directory "blog/")
	 :publishing-function org-publish-attachment)
	("static"
	 :base-directory ,taingram--base-directory
	 :base-extension "txt\\|css\\|woff\\|woff2"
	 :include ("profile.gif")
	 :exclude "blog/"
	 :recursive t
	 :publishing-directory ,taingram--publish-directory
	 :publishing-function org-publish-attachment)
	("taingram.org" :components ("index" "static" "pages" "blog" "blog-files"))))

;;; publish.el ends here
