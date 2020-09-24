;;; publish.el --- generate and publish my blog -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Thomas Ingram

;;; Commentary:

;; This file is cobbled togther from following many different blog posts and
;; examples for building an Org mode blog.  This setup works for me, and I
;; expect it may be helpful for others attempting the same.

;; https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

;;; Code:

(require 'ox-publish)

(setq org-html-htmlize-output-type `nil)

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")
	("cc-by-nd" . "@@html:<div class=\"license-notice-container\">
  <div class=\"license-notice\">
    <div class\"notice\">
      <span><b>Note:</b></span>
      <span>
        <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nd/4.0/\">
          <img alt=\"Creative Commons License\"
               src=\"https://i.creativecommons.org/l/by-nd/4.0/88x31.png\" />
        </a>
      </span>
    </div>
    This post is licensed under the
    <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nd/4.0/\">
      CC-BY-ND 4.0
    </a>.
  </div>
</div>@@")
	("right-justify" . "@@html:<span class=\"right-justify\">$1</span>@@")))

(defun org-sitemap-custom-entry-format (entry style project)
  "Sitemap PROJECT ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
              (format-time-string "%Y-%m-%d"
				  (org-publish-find-date entry project))
              entry
              filename))))

(setf org-html-metadata-timestamp-format "%Y %b %d")
(setf org-export-date-timestamp-format "%Y-%m-%d")

(defvar taingram-css "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\"/>")
(defvar taingram-header "<div id=\"updated\">Updated: %C</div>
<nav>
  <a href=\"/\">&lt; Home</a>
</nav>")

(defvar taingram-footer "<hr/>
<footer>
  <div class=\"copyright-container\">
    <div class=\"copyright\">
      Copyright &copy; 2019 Thomas Ingram some rights reserved<br/>
      Content is available under
      <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
      CC-BY-SA 4.0
      </a> unless otherwise noted
    </div>
    <div class=\"cc-badge\">
      <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
        <img alt=\"Creative Commons License\"
             src=\"https://i.creativecommons.org/l/by-sa/4.0/88x31.png\" />
       </a>
    </div>
  </div>

  <div class=\"generated\">
    Created with %c; powered by  <a href=\"https://httpd.apache.org/\">Apache</a> and <a href=\"https://getfedora.org/\">Fedora</a> <a href=\"https://www.gnu.org\">GNU</a>/<a href=\"https://www.kernel.org/\">Linux</a>
  </div>
</footer>")

     (defun my/relative-path-to-full (path)
       "Expand relative PATH from current buffer or file to a full path."
       (concat
	(if load-file-name
	    (file-name-directory load-file-name)
	  default-directory)
	path))
(setq org-publish-project-alist
      `(("index"
	 :base-directory "./org/"
	 :included "index.org"
	 :publishing-directory "./html/"
	 :publishing-function org-html-publish-to-html
	 :recursive nil

         ; Disable some Org publish defaults
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :with-toc nil
	 :section-numbers nil

         ; Use HTML5 tags
	 :html-html5-fancy t
	 :html-doctype "html5"

	 :html-head     ,taingram-css
	 :html-preamble "<div id=\"updated\">Updated: %C</div>"
	 :html-postamble ,taingram-footer)
	("pages"
	 :base-directory "./org/"
	 :base-directory "./org/"
	 :base-extension "org"
	 :exclude "index.org"
	 :publishing-directory "./html/"
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-filename "sitemap.org"
	 :recursive nil

         ; Disable some Org publish defaults
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :with-toc nil
	 :section-numbers nil

         ; Use HTML5 tags
	 :html-html5-fancy t
	 :html-doctype "html5"

	 :html-head     ,taingram-css
	 :html-preamble ,taingram-header
	 :html-postamble ,taingram-footer)
	("blog"
	 :base-directory "./org/blog/"
	 :base-extension "org"
	 :exclude "blog.org"
	 :publishing-directory "./html/blog/"
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-filename "blog.org"
	 :sitemap-sort-files anti-chronologically
         :sitemap-format-entry org-sitemap-custom-entry-format

	 ; Disable some Org publish defaults
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :with-toc nil
	 :section-numbers nil

	 ; Use HTML5 tags
	 :html-html5-fancy t
	 :html-doctype "html5"
	 :html-head ,taingram-css
	 :html-preamble ,taingram-header
	 :html-postamble ,taingram-footer)
	("static"
	 :base-directory "./org/"
	 :base-extension "css\\|jpg\\|gif\\|png\\|txt\\|pdf"
	 :recursive t
	 :publishing-directory "./html/"
	 :publishing-function org-publish-attachment)
	("taingram.org" :components ("index" "pages" "blog" "static"))))

;; Uncomment to force full site regeneration
;; (org-publish "taingram.org" t)

;;; publish.el ends here
