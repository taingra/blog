;;; publish.el --- generate and publish my blog -*- lexical-binding: t -*-

;; Copyright (C) 2018 Thomas Ingram

;;; Commentary:

;; https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

;;; Code:

(require 'ox-publish)

(when (not (require 'htmlize))
  (setq org-html-htmlize-output-type `nil))

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")
	("cc-by-nd" . "@@html:<div class=\"license-notice-container\">
  <div class=\"license-notice\">
    <div class\"notice\">
      <span><b>Note:</b></span>
      <span><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nd/4.0/\">
        <img alt=\"Creative Commons License\" src=\"https://i.creativecommons.org/l/by-nd/4.0/88x31.png\" />
      </a></span>
    </div>
    This post is licensed under the <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nd/4.0/\">CC-BY-ND 4.0</a>.
  </div>
</div>@@")
	("right" . "@@html:<span class=\"right-justify\">$1</span>@@")))

(defun org-sitemap-custom-entry-format (entry style project)
  "Sitemap ENTRY format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}} [[file:blog/%s][%s]]"
              (format-time-string "%Y-%m-%d"
				  (org-publish-find-date entry project))
              entry
              filename))))

(setf org-html-metadata-timestamp-format "%Y %b %d")
(setf org-export-date-timestamp-format "%Y-%m-%d")

(defvar taingram-head "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\"/>")

(defvar taingram-urgent "<div class=\"urgent\">
<a href=\"https://www.icrc.org/en/nuclear-ban-treaty-no-to-nukes\">Stop the construction of Nuclear Weapons, encourage your country to sign the
Nuclear Weapon Ban Treaty!</a>
</div>")

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

(setq org-publish-project-alist
      `(("index"
	:base-directory "~/Documents/blog/org/"
	:base-extension "org"
	:publishing-directory "~/Documents/blog/html/"
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

	:html-head     ,taingram-head
	:html-preamble "<div id=\"updated\">Updated: %C</div>"
	:html-postamble ,taingram-footer)
	("blog"
	 :base-directory "~/Documents/blog/org/blog/"
	 :base-extension "org"
	 :publishing-directory "~/Documents/blog/html/blog/"
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
	 :html-head ,taingram-head
	 :html-preamble "<div id=\"updated\">Updated: %C</div>
<nav>
  <a href=\"/\">&lt; Home</a>
</nav>"
	 :html-postamble ,taingram-footer)
	("static"
	 :base-directory "~/Documents/blog/org/"
	 :base-extension "css\\|txt\\|pdf"
	 :recursive t
	 :publishing-directory "~/Documents/blog/html/"
	 :publishing-function org-publish-attachment)
	("images"
	 :base-directory "~/Documents/blog/org/img/"
	 :base-extension "jpg\\|gif\\|png"
	 :publishing-directory "~/Documents/blog/html/img/"
	 :publishing-function org-publish-attachment)
	("taingram.org" :components ("index" "blog" "static" "images"))))

;; Uncomment to force full site regeneration
;; (org-publish "taingram.org" t)

;;; publish.el ends here
