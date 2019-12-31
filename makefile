
all:
	emacs -Q --batch -l publish.el -f org-publish-all

publish: all
	rsync -e ssh -vr html/ taingram@taingram.org:~/public_html/

clean:
	rm -r html/*
