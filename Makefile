.PHONY: blog resume all

all: blog resume

blog:
	emacs --script builders/build.el

resume:
	cd contents/resume && \
	pandoc resume_coop.tex -s -t html5 \
		-o ../../dist/resume.html \
		-H ../../templates/header.html \
		-A ../../templates/footer.html \
		-V pagetitle="My Resume" -M title-block=false -M title=
