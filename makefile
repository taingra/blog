
all:
	emacs -Q --batch -l publish.el -f org-publish-all

publish: all
	rsync -e ssh -vr html/ thomas@taingram.org:/var/www/taingram.org/html/

clean:
	rm -r html/*
