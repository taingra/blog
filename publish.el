;;; publish.el --- generate and publish my blog

;; Copyright (C) 2018 Thomas Ingram

;;; Commentary:

;; https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

;;; Code:

(require 'ox-publish)

(when (not (require 'htmlize))
  (setq org-html-htmlize-output-type `nil))

(defun org-sitemap-custom-entry-format (entry style project)
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "%s [[file:blog/%s][%s]]"
              (format-time-string "%Y-%m-%d"
				  (org-publish-find-date entry project))
              entry
              filename))))

;; (format-time-string "%Y")

(defvar taingram-footer (concat ))

(setq org-publish-project-alist
      '(("home"
	:base-directory "~/Documents/blog/org/"
	:base-extension "org"
	:publishing-directory "~/Documents/blog/html/"
	:publishing-function org-html-publish-to-html
	:recursive nil
	:section-numbers nil
        :with-toc nil
	:html-head "<link rel=\"stylesheet\"
                  href=\"style.css\" type=\"text/css\"/>"
	:html-postamble "
<hr/>
<footer>
  Last Updated: %C<br/>
  Generated with %c<br/>
  Copyright &copy; 2019 Thomas Ingram<br/>
  The content of this blog is licensed under
  <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
    Creative Commons Attribution-ShareAlike 4.0 International License
  </a> unless otherwise stated.
  <br/>
  <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
    <img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/4.0/88x31.png\" />
  </a>
</footer>")
	("blog"
	 :base-directory "~/Documents/blog/org/blog/"
	 :base-extension "org"
	 :publishing-directory "~/Documents/blog/html/blog/"
	 :publishing-function org-html-publish-to-html
	 :section-numbers nil
	 :with-toc nil
	 :htmlized-source nil
	 :auto-sitemap t
	 :sitemap-filename "blog.org"
	 :sitemap-sort-files anti-chronologically
         :sitemap-format-entry org-sitemap-custom-entry-format
	 :html-preamble "
<nav class=\"navigation\">
 <ol>
  <li><a href=\"/index.html\"><img src=\"../images/home.png\"></a></li>
  <li><a href=\"/blog/\"><img src=\"../images/folder.png\">Blog</a></li>
  <li><img src=\"../images/file.png\">%f</li>
 </ol>
</nav>"
	 :html-head "<link rel=\"stylesheet\"
                  href=\"../style.css\" type=\"text/css\"/>")
	("static"
	 :base-directory "~/Documents/blog/org/"
	 :base-extension "css\\|txt\\|pdf"
	 :recursive t
	 :publishing-directory "~/Documents/blog/html/"
	 :publishing-function org-publish-attachment)
	("images"
	 :base-directory "~/Documents/blog/org/images/"
	 :base-extension "jpg\\|gif\\|png"
	 :publishing-directory "~/Documents/blog/html/images/"
	 :publishing-function org-publish-attachment)
	("taingram.org" :components ("home" "blog" "static" "images"))))


;;; publish.el ends here
