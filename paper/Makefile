all: oru_paper.pdf
oru_paper.pdf: oru_paper.md oru_project.yaml header.yaml
	pandoc header.yaml $< -o $@ --citeproc --pdf-engine=xelatex
