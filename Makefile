PAPER = paper
PLOTS = plots
SCRIPTS = scripts
R = R
DATA = data

analysis = $(SCRIPTS)/12_analysis.html

all: paper

.PHONY: paper
paper: $(PAPER)/oru_paper.pdf
$(PAPER)/oru_paper.pdf: $(PAPER)/oru_paper.md \
                        $(PAPER)/oru_project.yaml \
                        $(PAPER)/header.yaml \
                        $(analysis)
	cd $(PAPER); pandoc header.yaml oru_paper.md -o oru_paper.pdf --citeproc --pdf-engine=lualatex


$(analysis): $(SCRIPTS)/12_analysis.R \
                             $(R)/hellinger.R \
                             $(DATA)/05_author_meta.Rds \
                             $(DATA)/07_coauth_count.Rds \
                             $(DATA)/09_H.Rds \
                             $(DATA)/10_models.Rds \
                             $(DATA)/10_model_stats.Rds \
                             $(DATA)/11_dept_dummies.Rds \
                             $(DATA)/11_au_dept_xwalk.Rds \
                             $(DATA)/11_test_train.Rds \
                             $(DATA)/11_dept_gamma.Rds \
                             $(DATA)/11_oru_gamma.Rds \
                             $(DATA)/12_layout.Rds
	cd $(SCRIPTS); Rscript -e "rmarkdown::render('12_analysis.R')" 

$(DATA)/11_*.Rds &: $(SCRIPTS)/11_depts.R \
                    $(DATA)/05_author_meta.Rds \
                    $(DATA)/06_author_histories.Rds \
                    $(DATA)/08_phrases.Rds \
                    $(DATA)/09_H.Rds \
                    $(DATA)/10_models.Rds \
                    $(DATA)/11_departments_canonical.csv
	cd $(SCRIPTS); Rscript 11_depts.R

$(DATA)/10_*.Rds &: $(SCRIPTS)/10_topic_modeling.R \
                    $(DATA)/09_H.Rds
	$(error Need to re-run topic models. Do `make topicmodels`)

.PHONY: topicmodels
topicmodels: 
	cd $(SCRIPTS); Rscript 10_topic_modeling.R

	