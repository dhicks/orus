---
title:  'Supplement to "Productivity and interdisciplinary impacts of Organized Research Units"'
author:  |
 | Daniel J. Hicks 
 | Data Science Initiative, UC Davis
 | Cognitive and Information Sciences, UC Merced
 | <hicks.daniel.j@gmail.com>
numbersections: false
toc: false
bibliography: oru_project.yaml
header-includes: 
	- \usepackage{longtable}
	- \usepackage{booktabs}
    - \usepackage{graphics}
    - \usepackage{enumitem}
    - \usepackage{lineno}
    - \usepackage{multirow}
    - \usepackage{float}
--- 

\newcommand{\beginsupplement}{%
		\renewcommand{\thesection}{S\arabic{section}}%
        \setcounter{table}{0}
        \renewcommand{\thetable}{S\arabic{table}}%
        \setcounter{figure}{0}
        \renewcommand{\thefigure}{S\arabic{figure}}%
     }
     
\let\origfigure\figure
\let\endorigfigure\endfigure
\renewenvironment{figure}[1][2] {
    \expandafter\origfigure\expandafter[H]
} {
    \endorigfigure
}

\beginsupplement

\listoftables

\listoffigures

\newpage
\input{../data/09_vocab.tex}

\newpage
\input{../plots/12_beta.tex}

![Organizational relationships in the data.  Nodes are either organizations (ORUs or departments at UC Davis) or individuals (ORU faculty or comparison set authors).  Edges connect an individual to an organization.  Node size indicates betweenness centrality, which can be used as a measure of importance for information flow within the network \label{fig.individual_net}](img/network.png)

![Department-level topic distributions.  Panels correspond to different values of $k$.  These distributions are constructed by taking a sample of non-ORU-affiliated authors from the department, aggregating all of their papers that were not written by any ORU-affiliated author, then processing the aggregate through the fitted topic models.  The visualization indicates that, except for $k=5$, non-ORU-affiliated work in most departments occurs in just a few topics. \label{fig.dept_gamma}](img/dept_gamma.png)

![ORU-level topic distributions.  Panels correspond to different values of $k$.  These distributions are constructed by aggregating all papers by authors affiliated with the ORU, then processing the aggregate through the fitted topic models.  The visualization indicates that, except for $k=5$, work in most ORUs is clustered into 1 or 2 topics.  CNPRC and JMIE are notable exceptions, with work spread more evenly across 4-5 topics. \label{fig.oru_gamma}](img/oru_gamma.png)

![Similarity networks for departments and ORUs, based on Hellinger distance between department- and ORU-level topic distributions (figures \ref{fig.dept_gamma} and \ref{fig.oru_gamma}).  Nodes (labels) correspond to departments/ORUs, with ORUs indicated by slightly larger labels.  Edges widths and shading correspond to similarity values, and edges for all pairs are used in each network (no values are trimmed or rounded to 0).  Node placement uses the sparse stress majorization algorithm [@OrtmannSparseStressModel2016], with similarity scores used as edge weights.  Label color corresponds to results of Louvain community detection [@BlondelFastUnfoldingCommunities2008] with similarity scores used as edge weights.  Colors are not meaningful across networks/panels.  Networks/panels correspond to values of $k$. \label{fig.dept_hell_net}](img/dept_hell_net.png)

![Researcher entropies, by ORU affiliation status and across values of $k$ (number of topics).  Violin plots include 25th, 50th (median), and 75th percentiles.  Color fills are points for individual researchers, staggered horizontally to correspond to the violin plots.  Panels correspond to values of $k$.  Grey horizontal lines at the top of each plot indicate the maximum possible entropy (uniform distribution across all topics) for that value of $k$. \label{fig.entropies}](img/entropies.png)

![Coefficient estimates for association between distance to departmental mean distribution and ORU affiliation across values of $k$ (number of topics).  Whiskers are 95% confidence intervals.  Regression models include as controls attributed gender, first year of publication, number of publications, number of coauthors, and departmental dummies. "No ORU affiliation" is used as the contrast value. Dashed lines indicate the thresholds for trivial values at ±.05.  $k=50%$ is highlighted because it was judged "best" by some but not all goodness-of-fit statistics.    \label{fig.reg.dept_dist_fixed}](img/dept_dist_fixed_reg.png)

# References #

::: {#refs}
:::