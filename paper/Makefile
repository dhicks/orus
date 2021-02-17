all: oru_paper.pdf
oru_paper.pdf: oru_paper.md oru_project.bib header.yaml
	pandoc header.yaml $< -o $@ --citeproc
