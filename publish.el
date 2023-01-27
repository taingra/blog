;;; publish.el --- generate and publish my blog -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Thomas Ingram

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
  "<div id=\"updated\">Updated: %C</div>")

;;; My idea for this
(defun taingram--gen-footer (&optional license comment)
  (concat
   (when comment
     "<div id=\"comments\">
<h2>Comments</h2>
<div id=\"text-comments\">
<p>Email questions, comments, and corrections to <a href=\"mailto:comment@taingram.org\">comment@taingram.org</a>.</p>
<p>Submissions may appear publicly on this website, unless requested otherwise in your email.</p>
</div>
</div>")
   "<hr/>
<footer>
<div class=\"copyright-container\">
<div class=\"copyright\">
Copyright &copy; 2017-2022 Thomas Ingram"
   (when license
       "<br/>
Content on this page is licensed
<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
CC-BY-SA 4.0</a> unless otherwise noted.
</div>
<div class=\"cc-badge\">
<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
<img alt=\"Creative Commons License\"
     src=\"https://i.creativecommons.org/l/by-sa/4.0/88x31.png\" />
</a>
</div>
</div>\n"
     ". All rights reserved unless noted otherwise.</div></div>\n")

"<div class=\"banner\">
<a href=\"https://www.controlmywebsite.com/aff.php?aff=313\">
<img src=\"https://cdn.aiso.net/affiliate/banners/aiso-banner2c.jpg\"
     alt=\"AISO.net solar powered web hosting provider\">
</a>
</div>
<div class=\"generated\">
Created with %c on <a href=\"https://www.gnu.org\">GNU</a>/<a href=\"https://www.kernel.org/\">Linux</a>
</div>
</footer>"))

;; WIP need to get this setup
(defvar taingram--support "<div id=\"supports\">
<h2>Support:</h2>
<div id=\"text-support\">
<p>If you found the info on my website helpful please consider sending a tip to my librapay</p>
</div>
</div>")

(defvar taingram--base-directory
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "org/")
  "The `:base-directory' for taingram site.")

(defvar taingram--preview-directory
  (concat
   (if (null load-file-name)
       (expand-file-name default-directory)
     (file-name-directory load-file-name))
   "html/")
  "The `:publishing-directory' for taingram-preview.")

(defvar taingram--publish-directory
  "/ssh:thomas@taingram.org:/var/www/taingram.org/"
  "The `:publishing-directory' for taingram.org project.")

(defun taingram--sitemap-dated-entry-format (entry style project)
  "Sitemap PROJECT ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
              (format-time-string "%Y-%m-%d"
				  (org-publish-find-date entry project))
              entry
              filename))))



(defun taingram--sitemap-rss (title list)
  "Generate an RSS feed for a org project using a custom sitemap function.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
‘org-list-to-lisp’.  PROJECT is the current project."


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

	 :html-head     ,taingram--head
	 :html-preamble ,taingram--preamble
	 :html-postamble ,(taingram--gen-footer nil t)
	("blog"
	 :base-directory ,(concat taingram--base-directory "blog/")
	 :base-extension "org"
	 :exclude ".*-draft\.org"
	 :publishing-directory ,(concat taingram--publish-directory "blog/")
	 :publishing-function org-html-publish-to-html

	 :html-link-home "https://taingram.org/"
	 :html-link-up "https://taingram.org/blog"
	 :html-home/up-format "<div id=\"org-div-home-and-up\"><a href=\"%s\">Blog</a> <a href=\"%s\">Home</a> </div>"

	 :auto-sitemap t
	 :sitemap-title "Blog Posts"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically
         :sitemap-format-entry taingram--sitemap-dated-entry-format
	 :sitemap-function taingram--sitemap-and-rss

	 :html-head ,taingram--head
	 :html-preamble ,taingram--preamble
	 :html-postamble ,(taingram--gen-footer t t)
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
