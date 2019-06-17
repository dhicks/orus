---
title:  "Paper title goes here"
author:  "Daniel J. Hicks"
output: pdf_document
header-includes: 
    - \usepackage{graphics}
---

analysis TODO
- descriptive summaries

<!--
![A simplified directed acyclic graph (DAG) used to account for relationships between the productivity dependent variables. \label{fig.dag}](../plots/ORU_DAG.png)
-->


# (Lit review) #


## Organized research units at UC Davis ##

*[there are 8 of them]*

| Abbreviation | Full name |
|:-------------|:----------|
| AQRC         | Air Quality Research Center                    |
| BML/CMSI     | Bodega Marine Laboratory/Coastal Marine Science Institute |
| CNPRC        | California National Primate Research Center    |
| CCC          | Comprehensive Cancer Center                    |
| CHPR         | Center for Healthcare Policy & Research        |
| ITS          | Institute for Transportation Studies           |
| JMIE         | John Muir Institute of the Environment         |
| PICN         | Program in International & Community Nutrition |

Table:  UC Davis Organized Research Units [ORUs] examined in this study \\label{tab.orus}

## (Topic modeling as interdisciplinarity)



# Data and methods #

## Author identification ##

In November 2018, the UC Davis Office of Research provided me with current rosters of faculty (tenured/tenure-track faculty) and "other academics" (primarily staff scientists) affiliated with each ORU.  I extracted the names and ORU affiliation for all of the affiliated faculty.  *[how many]*  In the remainder of this paper, I refer to these ORU-affiliated faculty as "ORU faculty" and "ORU researchers," and use these terms interchangeably.  

In January 2019, I conducted searches using the Scopus web interface for all papers published with UC Davis affiliations in 2016, 2017, and 2018.  *[how many]*  The metadata retrieved for these papers included names, affiliation strings, and Scopus author IDs for each author.  Using a combination of automated and manual matching, I identified author IDs for ORU-affiliated faculty.  I next searched the affiliation strings for "Department of" to identify departmental affiliations for these faculty.  This process matched *[num]* ORU-affiliated faculty from the rosters, and resulted in *[num]* distinct departmental affiliation strings.  While many faculty have multiple distinct affiliation strings, these generally reflect minor variations in capitalization and punctuation *[example]*.  Given limited resources, and the high likelihood that these variations would wash out later in the data collection and analysis process (e.g., misspellings would only show up a single time), I elected not to clean or standardize these affiliation strings.  

To identify a comparison set of researchers, I first identified all authors in the 2016-2018 Scopus results with the same departmental affiliations; that is, an author was included in this stage if they shared at least one departmental affiliation string, and thus a departmental affiliation simpliciter, with an ORU researcher.  However, this process likely captured many graduate students and postdoctoral researchers.  Because ORU researchers are generally tenured faculty, including students and postdoctoral researchers would confound the analysis of differences between ORU and non-ORU researchers.  

I therefore used the Scopus API (application programming interface) to retrieve author-level metadata for both the ORU faculty and the potential comparison researchers.  Specifically, I examined the number of publications and the year of first publication.  After exploratory data analysis, I filtered both ORU faculty and comparison researchers, including them for further analysis only if they met all three of the following conditions:  

1. 15 or more total publications
2. First publication in or prior to 2009
3. First publication after 1970

The first two conditions appeared to effectively remove students and postdoctoral researchers, along with early-career faculty.  Note that these conditions may not exclude staff scientists or lecturers who are later in their careers.  The third condition was used to exclude a small number of researchers with very early first publication years (e.g., 1955) that were plausibly due to data errors.  

Note that, in the analysis below, departmental affiliations for all authors are based on the 2016-2018 Scopus results, not entire publication careers.  

After applying these filters, a total of *[num]* researchers had been selected for analysis.  *[any dropouts in the abstract retrieval stage?]*  Figure \ref{fig.sample} shows the number of researchers in the analysis dataset for each ORU and the comparison set.  

![Number of researchers in the analysis dataset, for each ORU and the comparison set.  Note the y-axis uses a square-root scale. \label{fig.sample}](../plots/12_sample.png)


*[background on gender effects]* To account for these effects, the online tool `genderize.io` was used to attribute gender to all authors based on their first or given name.  This tool resulted in a trinary gender attribution:  woman, man, and "missing" when the tool was not sufficiently confident in either primary gender attribution.  *[distribution of genders across ORU and non-ORU]*  In the analyses below, men are used as the reference level for estimated gender effects.  

*[network viz here]*

## Productivity impacts ##

To investigate the productivity impacts of ORUs, I used author-level metadata from the Scopus API.  Specifically, all-career publication counts and (incoming) citation counts are both reported in the Scopus author retrieval API, and so these data were retrieved prior to the filtering step above.  Coauthor counts (total number of unique coauthors) were calculated from the article-level metadata retrieved for the text analysis steps discussed below.  Because coauthor counts, publication counts, and citation counts all varied over multiple orders of magnitude, I used the log (base 10) value for these variables in all analyses.  

I fit regression models for each of these three dependent variables, using ORU affiliation as the primary independent variable of interest and incorporating controls for gender, first year of publication (centered at 1997, which is the rounded mean first year in the analysis dataset), and dummy variables for departmental affiliation.  

Because of the log transformation of the dependent variables, the regression model coefficients can be exponentiated and interpreted as multiplicative associations.  For example, a coefficient of .5 can be interpreted as an association with a $10^{.5} \approx 3.16$-fold or $3.16 \times 100\% - 100\% = 216\%$ increase in the dependent variable.  

To account for relationships between the three dependent variables, I use the simplified directed acyclic graph (DAG) shown in figure \ref{fig.dag}.  According to this model, the number of coauthors influences the number of publications influences the number of citations.  The number of publications thus mediates between coauthors and citations, and coauthors mediates between the independent variables and publications; I also allow that coauthors might directly influence citations.  

![A simplified directed acyclic graph (DAG) used to account for relationships between the productivity dependent variables. \label{fig.dag}](../plots/ORU_DAG.png)

## Discursive impacts ##

I used topic models and related text analysis methods to examine the discursive impacts of ORUs.  

### Topic modeling ###

Specifically, I first used the Scopus API to retrieve paper-level metadata for all authors in the analysis dataset.  These metadata included the text of paper abstracts.  Abstract texts were aggregated within individual authors, treating each individual author as a single "document."  For example, suppose researcher A was an author on documents 1 and 2, and researcher B was an author on documents 2 and 3.  Researcher A, as a single "document," would be represented for text analysis as the combination of abstracts 1 and 2; while researcher B would be represented as the combination of abstracts 2 and 3.  

Vocabulary selection began by using part-of-speech tagging to identify nouns in the aggregated abstract text.  *[software used for this]*  I then calculated an entropy-based statistic for each noun, keeping the *[top 5,000]* terms for further analysis.  Note that stopwords were not explicitly excluded at this stage; though typical lists of English stopwords do not include many nouns.  

Specifically, consider the task of identifying an author (or, more generally, a "document"), selected uniformly at random from the analysis dataset.  For $N$ authors, the  unconditional probability of drawing any given author is $p_N = \frac{1}{N}$; this distribution has entropy 
\begin{align*}
    H(P) &= \sum_{N} - p_N \log_2 p_N\\
         &= - N \frac{1}{N} \log_2 \frac{1}{N}\\
         &= \log_2 N
\end{align*}

Now suppose we are given a term $t$ drawn from the token distribution of the selected author.  Because the uniform distribution has maximal entropy, the conditional author distribution given the term, $p(author_j | term_t)$, has a lower entropy $H_t \leq \log_2 N$.  Let $\Delta H_t = \log_2 N - H_t$.   $\Delta H_t$ measures the information about the identity of the author gained when we are given the term $t$.  Insofar as we are given a high-information term, we can dramatically narrow down the range of possible authors.  Terms have higher information insofar as they are specific to a smaller group of authors.  

However, typically the most high-information terms will be unique to a single author, such as typos or idiosyncratic terms.  To account for this, I also calculate the order-of-magnitude of the overall occurrence of a term, $\log_{10} n_t$.  I then take the product $\log_{10} n_t \Delta H_t$, which I represent in the code as `ndH`, and select the top terms according to this $\log_{10} n_t \Delta H_t$ statistic.  Table \ref{tab:vocab} shows the top 50 terms selected for the analysis vocabulary.  As the term list suggests, this statistic is effective at identifying terms that are clearly distinctive (in this case, to different disciplines and research fields), meaningful, and occur frequently. 

\input{../data/09_vocab.tex}

Given this vocabulary list, I next construct topic models, again treating each author as a single "document."  *[this probably belongs in the lit review]*
Briefly, topic models are based on a highly simplified generative model of writing.  A given document "contains" an assortment of topics (represented as a probability distribution over a fixed number of topics $k$); each topic, in turn, "contains" an assortment of terms (represented as a probability distribution over the vocabulary).  When a token is added to the document, the writer first selects a topic using the document's topic distribution, then selects a term from that topic's term distribution.  

*[software used]*

A well-known limitation of topic models is that the number of topics, $k$, is a free parameter.  Exploratory analysis of the author-term distributions using principal components found that 80% of the variance could be covered by 63 topics, and 90% of the variance could be covered by 135 topics.  Given this range, I fit models with 5, 25, 45, \ldots, 145 topics.  I calculated 5 goodness-of-fit statistics for each of these models:  semantic coherence, exclusivity, the log likehood of a holdout subset of terms (i.e., not used to fit the model), the residuals, and the number of iterations required by the algorithm to fit the model.  *[cites]*  

While these goodness-of-fit statistics have been promoted for use in selecting the number of topics, they all have known limitations.  For example, semantic coherence favors a small number of "large" topics, while exclusivity favors a large number of "small" topics.  Inspecting these various statistics, I found that their values generally did not change much beyond $k=45$, and were very stable beyond $k=85$.  

Rather than selecting a single "best" topic model, in the analysis below I either (a) conduct and report analyses using all of the topic models, highlighting $k=85$, or (b) conduct analyses for $k = 25, 45, 85, 125$, reporting all four equally.  Approach (b) was generally used when the analysis involved a complex visualization component — to keep the number of plots manageable — or an intensive computation — such as all pairwise distances between authors.  

### Analyses ###

For a given value of $k$, I focused my analysis on the conditional topic distribution $\gamma_{t,i} = p(topic_t | author_i)$.  Recall the three hypotheses for discursive impacts, introduced in §*[ref]*.  For hypothesis 1, I calculated "discursive breadth" for author $i$ as the entropy of the topic distribution $H_i = H(\gamma_{\cdot, i})$.  

To operationalize the "discursive distance" between any pair of topic distributions $\gamma_1, \gamma_2$, I use Hellinger distance *[cites]*:  
\begin{align*}
    h(\gamma_1, \gamma_2) &= \frac{1}{\sqrt 2} \sqrt{\sum_{t} (\sqrt{\gamma_{t,1}} - \sqrt{\gamma_{t,2}})}\\
    &= \sqrt{1 - \sum_t \sqrt{\gamma_{t,1} \cdot \gamma_{t,2}}}
\end{align*}
where $t$ indexes topics.  Hellinger distances range from 0 to 1 inclusive, where 0 indicates that two distributions are the same and 1 indicates that the two distributions have completely different support.  Hellinger distance is a scaled version of the Euclidean distance between the square root vectors $\sqrt \gamma_1, \sqrt \gamma_2$.  Note that, because the square root vectors are all unit length, the Hellinger distance is directly related to the cosine similarity between the square root vectors.  Cosine similarity is widely used in bibliometrics.  

Hypothesis 2 requires constructing department-level topic distributions.  For a given set of authors $X$, a conditional topic probability $\gamma_{t,X}$ can be constructed simply as the (unweighted) mean $\bar \gamma_{t, x, x \in X}$.  Note that $\sum_t \gamma_{t,X} = 1$, so that $\gamma_{\cdot, X}$ gives a probability distribution over topics.  So the simplest way to construct a department-level topic distribution might be to take this mean over the set of all members of each department.  

However, for the purposes of investigating hypothesis 2, this simple construction biases the department-level topics towards ORU faculty.  If ORU faculty are distant from the other members of their department, their contributions to the mean will act as outliers, and the resulting distance measures will be biased downward.  A second approach would be to use all and only non-ORU faculty members of the department.  But then these non-ORU faculty would be counted twice:  first they would be used to construct the mean, and then second we would calculate their distances from this mean.  Because ORU faculty would be counted only once, this approach would bias the ORU distances upwards.  

I therefore borrowed an approach from machine learning *[cite]*, and randomly separated non-ORU department members into two discrete subsets.  The first subset — referred to as the "training" set in machine learning — was used to construct the department-level topic distributions.  The second subset — the "testing" subset — was used to make the distance comparisons, using Hellinger distance.  50% of non-ORU department members were allocated to each subset, selected uniformly at random.  

After selecting these subsets and constructing department-level topic distributions, I calculated "departmental distance" using Hellinger distance for all ORU-affiliated faculty and all faculty in testing subsets.  I used these departmental distance values as dependent variables in a series of regression models, one for each value of $k$, including first publication year, gender, log number of documents and coauthors, and department dummies as controls.  

For hypothesis 3, I calculated pairwise Hellinger distances between all ORU faculty and (a) other members of their same ORU and (b) non-ORU-affiliated members of their same department.  I then took the minimum distance in each of (a) and (b), and compared these minimums using scatterplots.  If the minimum for (a) is greater than the minimum for (b), this indicates that the ORU faculty member is closer to their home department than their ORU, conflicting with hypothesis 3.  

Because these distances are a computationally-intensive step, I did not calculate distances between pairs of non-ORU-affiliated authors, and only used selected values of $k = \{25, 45, 85, 125\}$.  I did calculate distances between all pairs of ORU-affiliated authors, for use in visualizing "discursive space."  

All data collection and analysis was conducted in R [@RCoreTeamLanguageEnvironmentStatistical2018].  The `RCurl` package was used for API access *[cite]*; the `stm` package was used to fit topic models *[cites]*; and numerous tools in the `tidyverse` suite were used to wrangle data [@WickhamtidyverseEasilyInstall2017].  All code used in data collection and analysis is available at *[repo]* *[can I release the roster?]*  Due to intellectual property restrictions, results returned from the Scopus web interface and API cannot be made publicly available.  However, some downstream analysis files may be made available upon request.  



# Results #

## Productivity impacts ##

Regression analyses indicate that ORU affiliation is associated with a substantial increase in the number of coauthors, 1.6-2.4-fold (2.0-fold).[^estimates]  See figure \ref{fig.reg.coauths}. ORU affiliation had a much weaker direct association with the number of publications, 1.0-1.3-fold (1.2-fold), while an order-of-magnitude increase in number of coauthors had a much stronger association, 2.2-3.4-fold (2.7-fold).  See figure \ref{fig.reg.pubs}. 

[estimates]: Statistical estimates are reported as 95% confidence intervals followed by the maximum likelihood point estimate in parentheses.  No hypothesis testing was done in this paper.  Confidence intervals should be interpreted as a range of values that are highly compatible with the observed data, given the modeling assumptions *[cite]*.  A confidence interval that contains zero can still be evidence of a substantial (non-zero, say, positive) relationship insofar as the bulk of the interval is greater than zero.  

![Regression estimates for number of coauthors \label{fig.reg.coauths}](../plots/12_coauths_regression.png)

![Regression estimates for number of publications \label{fig.reg.pubs}](../plots/12_pub_regression.png)

The estimates for citations are similar.  Order-of-magnitude increases in number of publications and number of coauthors are both associated with substantial increases in the number of citations a researcher has received to date:  2.7-6.9-fold (4.3-fold) for publications, 1.8-3.7-fold (2.6-fold) for coauthors.  (When number of coauthors is dropped, the point estimate for publications is approximately 10, that is, publishing 10 times as many publications is associated with receiving 10 times as many citations.)  While controlling for these midstream dependent variables, ORU affiliation is, if anything, negatively associated with citations received, .7-1.1-fold change (0.9-fold), as much as a 30% decrease.  See figure \ref{fig.reg.cites}. 

![Regression estimates for number of citations \label{fig.reg.cites}](../plots/12_cites_regression.png)

All together, in causal terms, these regression results suggest that ORU affiliation has a substantial direct effect only on the number of coauthors.  This increase in coauthors in turn leads to increased publications and increased citations; but ORU affiliation has a much smaller direct effect on these two downstream productivity measures.  On this interpretation of the findings, ORU affiliation makes faculty more productive primarily by connecting them with collaborators.  

However, the evidence for this causal interpretation is limited, because we do not have the data to compare a researchers's number of coauthors before and after they join the ORU.  The available data are consistent with a pattern where some unobserved variable is a common cause of both ORU affiliation and coauthor count.  For example, highly gregarious and extraverted faculty members might tend to have more coauthors and also be more likely to be invited to join an ORU.  



## Discursive space ##

Before discussing the discursive impacts of ORUs, I visualize "discursive space," based on the pairwise Hellinger distance between author-topic distributions for all ORU-affiliated faculty.  (Calculating distances for all pairs of researchers in the dataset was not feasible with available computational resources.)  In figure \ref{fig.mds}, these pairwise distances are represented in 2-dimensional space using classical multidimensional scaling (MDS).  If $h(\gamma_1, \gamma_2)$ is the Hellinger distance between authors 1 and 2 and $d(m_1, m_2)$ is the Euclidean distance between their corresponding points in the visualization, classical MDS attempts to find values of $m_1, m_2$ (simultaneously across all pairs) such that $h(\gamma_1, \gamma_2) \approx d(m_1, m_2)$ *[cite]*.  That is, distance in the visualization approximates Hellinger distance.  

Figure \ref{fig.mds} shows MDS visualizations across 4 values of $k$ (numbers of topics).  Because MDS solutions are not unique up to rotations and reflections, the overall structure of the visualizations are the same across the 4 values of $k$:  3 arms or axes, roughly corresponding to environmental ORUs (JMIE, ITS, BML), medical ORUs (CCC, CHPR), and CNPRC (which has a primatological-behavioral science focus).  PICN and AQRC are located near where these 3 arms meet, suggesting a mix of environmental and health work.    

![Visualization of ORU "discursive space."  Panels correspond to different values of $k$ (number of topics).  Point positions are calculated using classical multidimensional scaling (MDS) on the pairwise Hellinger distance between author-topic distributions.  MDS solutions are not unique up to translations,rotations, and reflections.  Ellipses indicate bounds on the researchers affiliated with each ORU (based on the convex hull). \label{fig.mds}](../plots/12_mds.png)

Besides the relative positions of ORU authors in "discursive space," figure \ref{fig.mds} also provides qualitative evidence that certain ORUs are spread "further" across this space.  For example, JMIE researchers can be found on the furthest end of the environmental arm, but also near the center of the visualization and partway down the medical and CNPRC arms.  By contrast, BML and ITS researchers are located entirely within the environmental arm.  



## Discursive impacts ##

*[H1]* states that ORU interdisciplinarity may lead to increased "discursive breadth."  Above I proposed to measure this as the entropy of an author's topic distribution.  Figure \ref{fig.reg.entropy} shows the coefficient estimates for the association between topic entropy and ORU affiliation across all topic models.  Across all models, confidence intervals generally cover from -0.6 to 0.1 bits, with point estimates in the range -0.2 to -0.3 bits.  That is, these models suggest generally indicate *decreased* discursive breadth; this suggests that ORU authors may be more specialized than their departmental peers.  

![Coefficient estimates for association between topic entropy and ORU affiliation across values of $k$ (number of topics).  Whiskers are 95% confidence intervals.  Regression models include as controls attributed gender, first year of publication, number of publications, number of coauthors, and departmental dummies.   $k=85% is highlighted as the single "best" model, though this should not be overinterpreted.  \label{fig.reg.entropy}](../plots/12_entropy_regression.png)  

However, using 0.5 bits ("half of a coin flip") as a threshold for substantive difference (that is, treating any value between -0.5 and 0.5 as too small to be interesting), overall the models indicate that the difference in discursive breadth between ORU authors and their peers is *trivial*.  In either case (whether discursive breadth is narrower or the difference is trivial), *[H1]* does not seem to be supported.  


*[H2]* states that ORU interdisciplinarity may lead to increased departmental distance, that is, increased Hellinger distance from the departmental mean topic distribution.  Figure \ref{fig.reg.dept_dist} shows coefficient estimates for the association between topic entropy and ORU affiliation across values of $k$ (number of topics), along with 95% confidence intervals.  Here the estimates may appear to support *[H2]*, as the estimates are generally positive.  However, most confidence intervals end well below .05, and point estimates almost all approximately .02.  Recall that Hellinger distance is on a 0-1 scale.  On this scale, distances less than .05 would seem to be trivial.  That is, there does not seem to be a meaningful difference between ORU faculty and their departmental peers, and so *[H2]* does not appear to be supported either.  

![Coefficient estimates for association between distance to departmental mean distribution and ORU affiliation across values of $k$ (number of topics).  Whiskers are 95% confidence intervals.  Regression models include as controls attributed gender, first year of publication, number of publications, number of coauthors, and departmental dummies.   $k=85% is highlighted as the single "best" model, though this should not be overinterpreted.  \label{fig.reg.dept_dist}](../plots/12_dept_dist_regression.png)  

Due to differences ORU and department size, as well as varying research foci, it might be suspected that departmental distance effects could vary across ORUs.  Figure \ref{fig.reg.dept_dist_fixed} reports coefficient estimates for ORU dummy variables, rather than the binary yes/no ORU affiliation used above; "no ORU affiliation" is used as the contrast value for the ORU dummies.  

![Coefficient estimates for association between distance to departmental mean distribution and ORU affiliation across values of $k$ (number of topics).  Whiskers are 95% confidence intervals.  Regression models include as controls attributed gender, first year of publication, number of publications, number of coauthors, and departmental dummies. "No ORU affiliation" is used as the contrast value. Dashed lines indicate the thresholds for trivial values at ±.05.  $k=85% is highlighted as the single "best" model, though this should not be overinterpreted.   \label{fig.reg.dept_dist_fixed}](../plots/12_dept_dist_fixed_reg.png)

Figure \ref{fig.reg.dept_dist_fixed} does indeed suggest that the potential effects of ORU affiliation on departmental distance do vary across ORUs, albeit still to a limited extent.  In no case do we see consistent evidence (across values of $k$) of an effect larger than ±.10.  However, there does seem to be some evidence of a *non-trivial effect for CNPRC*, with point estimates generally just above .05.  Surprisingly, there is also some evidence of a *negative effect for ITS and PICN*.  That is, there is some evidence that these researchers are *closer* to the departmental means than a randomly-selected subset of their departmental peers.  This last signal is far from clear, however; and generally figure \ref{fig.reg.dept_dist_fixed} indicates *no substantive difference* in departmental distance between ORU researchers and their peers.  

*[H3]* proposes that ORU interdisciplinarity leads researchers to be closer to their ORU peers than their (non-ORU-affiliated) departmental peers in discursive space.  Figure \ref{fig.silhouette} shows scatterplots for minimal distances to both kinds of peers, for each ORU and 4 values of $k$.  In these scatterplots, the dashed line indicates $y=x$.  Points above this line are closer to ORU peers than departmental peers, supporting *[H3]*.  

![Minimal distance to ORU peers vs. departmental peers.  Both x- and y-axes are on Hellinger distance scale (0-1).  The dashed line in each panel indicates $y=x$.  Points above this line are closer to ORU peers than departmental peers, supporting Hypothesis 3.  Note that comparisons to the dashed line should be made *vertically* or *horizontally*.    \label{fig.silhouette}](../plots/12_oru_dept_min_dist.png)

For most ORUs, across values of $k$, most researchers are located near or somewhat below the dashed line.  This means that researchers are typically *equidistant or closer to their closest departmental peers* than their closest ORU peers.  

Because distance comparisons in scatterplots can be misleading (comparing vertical distance to the dashed line, not Euclidean distance), figure \ref{fig.ridges} shows the distribution of these comparisons.  In this figure, positive x-axis values indicate that departmental distance is greater than ORU distance, supporting *[H3]*.  In this figure, modal values are all negative or near 0, and this also appears to be true for median values.  Notable exceptions to this trend are ITS and, less consistently, CNPRC.  In short, with a exceptions, ORU researchers are generally *equidistant or closer to their closest departmental peers.*  As before, these findings conflict with *[H3]*.  

![Comparison of minimal ORU and departmental distances, across ORUs and values of $k$.  Positive x-axis values indicate that departmental distance is greater than ORU distance, supporting Hypothesis 3.  Dashed vertical line indicates 0; solid lines within densities indicate median values; and small vertical dashes indicated individual values.\label{fig.ridges}](../plots/12_oru_dept_min_dist_ridges.png)

