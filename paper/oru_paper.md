---
title:  "Productivity and interdisciplinary impacts of ORUs"
author:  "Daniel J. Hicks"
affiliation: "Data Science Initiative, UC Davis"
email: <hicks.daniel.j@gmail.com>
output: pdf_document
numbersections: true
bibliography: oru_project.bib
header-includes: 
    - \usepackage{graphics}
    - \usepackage{enumitem}
abstract: "*[abstract]*"
---

<!-- https://www.mitpressjournals.org/journals/qss/sub -->

TODO
- intro/background/lit review
    - what and why are ORUs
    - discursive space diagram
    - topic models in bibliometrics
- descriptive summaries
- finish discussion/conclusion
- limitation:  computational resources
    - need to use small vocabularies to fit topic models in reasonable time
    - but this might mean our representation of discursive space is too coarse
- check present tense

<!--
![A simplified directed acyclic graph (DAG) used to account for relationships between the productivity dependent variables. \label{fig.dag}](../plots/ORU_DAG.png)
-->



# Introduction #


*[couple paragraphs setting up the general topic]*




## Organized research units at UC Davis ##

At the time this study was conducted, UC Davis had 8 Organized Research Units (ORUs), all dedicated to interdisciplinary research.  See table \ref{tab.orus}.  

| Abbreviation | Full name | Founded in |
|:-------------|:----------|:-----------|
| AQRC         | Air Quality Research Center                    | 2005 |
| BML/CMSI     | Bodega Marine Laboratory &                     | 1960 (BML)/2013 (CMSI) |
|              | Coastal Marine Science Institute               ||
| CNPRC        | California National Primate Research Center    | 1962 |
| CCC          | Comprehensive Cancer Center                    | 2002 |
| CHPR         | Center for Healthcare Policy & Research        | 1994 |
| ITS          | Institute for Transportation Studies           | 1991 |
| JMIE         | John Muir Institute of the Environment         | 1997 |
| PICN         | Program in International & Community Nutrition | 1987 |

Table:  UC Davis Organized Research Units [ORUs] examined in this study \label{tab.orus}

Four of these ORUs are dedicated to environmental topics, broadly constructed to include ecology, conservation biology, environmental science, and environmental policy:  the Air Quality Research Center (AQRC); the Bodega Marine Lab/Coastal and Marine Science Institute (BML/CMSI); the Institute of Transportation Studies (ITS); and the John Muir Institute of the Environment (JMIE).  Of these four ORUs, JMIE is the most heterogeneous, with distinct initiatives in climate change, data science in environmental science, energy systems, polar science, and water science and policy.  

Three other ORUs are dedicated to biomedical topics:  the Comprehensive Cancer Center (CCC); the Center for Healthcare Policy and Research (CHPR); and the Program in International and Community Nutrition (PICN).  CCC has both research and clinical aspects, and as the name indicates CHPR supports both academic research and policy analysis.  PICN has a strong global focus, with active research projects in Laos; Haiti; Cameroon and Ethiopia; Burkina Faso, Ghana, and Malawi; the Gambia; Niger; Bangladesh; India; and Kenya.  

The eighth ORU, CNPRC, is organized into four "units," devoted to infectious diseases, neuroscience and behavioral research, reproductive science and regenerative medicine, and respiratory diseases.  As these labels suggest, CNPRC supports a mix of behavioral and biomedical research.  

As table \ref{tab.orus} indicates, the age of these ORUs vary substantially.  BML is the oldest of the current ORUs, founded in 1960.  CMSI, the youngest of the current ORUs, was formed in 2013 to coordinate research activities between BML (a single laboratory on the Pacific Ocean north of San Francisco) and other water research (e.g., the Tahoe Environmental Research Center in Incline Village, Nevada, on the north short of Lake Tahoe).  BML/CMSI are treated as a single unit for the purposes of this study.  

This study only considered ORU affiliations as of Fall 2018; researchers who might have been affiliated with an ORU previously, but were no longer affiliated as of Fall 2018, and were still actively publishing with a UC Davis affiliation during the period 2015-2017, would be considered not affiliated with any ORU.  


## Productivity and discursive impacts ##

### Productivity impacts ###

Interdisciplinary research is often evaluated in terms of standard research outputs, such as publication counts, citation counts, and perhaps patents or other indicators of economic impact.  *[cites]*  In the context of evaluating the effects of a particular (set of) programmatic interventions — namely, recruiting faculty to an ORU — I refer to these familiar kinds of outputs as *productivity impacts*.  Research evaluation might also consider productivity inputs, such as grant application success rate or quantity of external research funds received.  

In this study, I examine three productivity impacts of the UC Davis ORUs.  Publication and citation counts are familiar measures of research productivity.  The third, coauthor count, is not usually used as a primary measure of productivity.  However, it is highly plausible that increased collaboration — and so an increased number of coauthors — leads at least to increased publication.  Treating this as a "problem," bibliometricians and sociologists of science have for at least 70 years deployed various "fractional" schemes for discounting individual authors' contributions to coauthored papers [@LindseyProductionCitationMeasures1980].  In this study, I do not view coauthorship as a "problem" to be solved with fractional counting, but instead as one potential mechanism by which ORUs might increase productivity.  That is, along with providing (or facilitating the provision of) research funds and other material resources, ORUs might serve an important network function, encouraging researchers to work together more than they would have otherwise.  I therefore include coauthor count as a potentially significant productivity impact.  


### Discursive impacts ###

Evaluating the productivity impacts of an interdisciplinary research program or organizational unit is, methodologically and conceptually, essentially the same as evaluating a disciplinary program or organizational unit:  the same kinds of data will be collected and analyzed in the same way.  In addition, productivity impacts abstract from the content of research.  Counting publications doesn't consider what those publications say.  

But interdisciplinary research is typically justified in terms of distinctive pragmatic goals.  For example, an unsigned editorial in *Nature* argues that "tackl[ing] society's challenges through research requires the engagement of multiple disciplines" [@MeetChallengeInterdisciplinary2016].  Following the distinctions made by Vanevar Bush and James Conant in the early Cold War period, Geiger contrasts disciplinary research with "programmatic research" [@GeigerOrganizedResearchUnits1990, 8].  Geiger argues that the norms of disciplinary research are enforced by academic departments, making them "inherently [epistemically] conservative institutions."  And so ORUs (broadly understood to include museums, observatories, and extension offices) "exist to do what departments cannot do: to operate in interdisciplinary, applied, or capital-intensive areas in response to social demands for new knowledge" [@GeigerOrganizedResearchUnits1990, 17].  

Interdisciplinary research may also have epistemic goals.  @HuutoniemiAnalyzingInterdisciplinarityTypology2010 distinguish epistemologically oriented and instrumentally oriented interdisciplinary research (and sometimes use a third category of "mixed orientation" interdisciplinary research) [see also @BruunPromotingInterdisciplinaryResearch2005 29-30, 90-1].  In a qualitative analysis of interdisciplinary research funded by the Academy of Finland, they find that the majority of interdisciplinary funding is directed towards (purely) epistemologically oriented projects [@BruunPromotingInterdisciplinaryResearch2005 104], and that, weighted by funding, epistemologically oriented research is more likely to be deeply integrative (rather than merely multidisciplinary research) than instrumentally oriented research [@BruunPromotingInterdisciplinaryResearch2005 106].  

Even when interdisciplinary research has purely epistemic goals, we would expect these goals to be distinctive from those of disciplinary research.  "Integration of various disciplinary perspectives is expected to lead to a more profound scientific understanding or more comprehensive explanations of the phenomena under study" [@HuutoniemiAnalyzingInterdisciplinarityTypology2010 85].  Different disciplines are assumed to offer complementary perspectives on the phenomenon or subject.  Then, bringing these complementary perspectives together is expected to produce qualitatively better knowledge than each could have produced on its own.  

However, different disciplinary perspectives are not necessarily complementary.  Different disciplines — or even lines of research within a given discipline — may depend on different metaphysical, epistemological, and methodological background assumptions [@KuhnStructureScientificRevolutions1996; @CartwrightDappledWorld1999; @EigenbrodeEmployingPhilosophicalDialogue2007; 
@HolbrookWhatInterdisciplinaryCommunication2012; @LonginoStudyingHumanBehavior2013; @PotochnikIdealizationAimsScience2017 ch. 7].  These background assumptions may be deeply incompatible, and attempts to integrate them might be frustrating and unproductive.  In other words, in this case, interdisciplinary research might be *less*, rather than more, than the sum of its disciplinary parts.  

Insofar as interdisciplinary integration has been successful in a particular case — that is, insofar as a body of research has been interdisciplinary rather than multidisciplinary — a variety of theoretical perspectives predict that researchers will have to have produced a collection of material and linguistic affordances spanning the divide.  In a material mode, @StarInstitutionalEcologyTranslations1989 examine "boundary objects" that circulate across disciplinary communities, serving as both shared objects of inquiry and shared sources of evidence.  Work on "trading zones" has drawn on concepts from linguistics, such as pidgins and creoles, to analyze linguistic innovation in successful cross-disciplinary interactions [@GalisonImageLogicMaterial1997; @CollinsTradingZonesInteractional2007].  For example, @AndersenConceptualDevelopmentInterdisciplinary2012 analyzes a successful collaboration between chemists and physicists, stressing the need for "interlocking mental models" such that "the same concepts may form part of multiple lexica concerned with, for example, different aspects of a phenomenon" [@AndersenConceptualDevelopmentInterdisciplinary2012 281ff].  

In this study, I emphasize the linguistic side of these exchanges and interlocking mental models.  I propose that interdisciplinary research, as contrasted with disciplinary and multidisciplinary research, will have distinctive linguistic traces that can be detected using text mining methods.  

Conceptually, I begin with the idea of *discursive space*, the space of research topics and conceptual schemes as they manifest in language.  Figure \ref{fig.conceptual} suggests how disciplinary and interdisciplinary researchers might be configured in this discursive space.  There are two groups of disciplinary researchers, "red" and "blue."  These researchers have simple primary colors and are clustered close together, indicating that they work on similar research topics, employ similar conceptual schemes, and more generally use similar language.  The circles representing these researchers are small, indicating that they work on a relatively small set of topics.  And the clusters are in distinct areas of discursive space, indicating that they differ substantially in their research topics and conceptual schemes.  (The clusters are internally homogeneous but externally heterogeneous.)

![Conceptual model of "discursive space."  Disciplinary researchers (red and blue dots) tend to be located close to members of the same discipline but far from members of other disciplines.  Interdisciplinary researchers (purple ellipses) are located in between these disciplinary clusters. \label{fig.conceptual}](img/conceptual_model.png){width="60%"}

Figure \ref{fig.conceptual} also includes two interdisciplinary researchers.  These researchers are shades of purpose and are located in the space between the red and blue clusters, indicating that they use a mix of research topics, conceptual schemes, and language more generally from the two disciplines.  The ellipses representing the interdisciplinary researchers are larger, indicating that they work on a relatively large set of topics.  The shading and position of the researchers suggests that they have home departments or disciplines:  one is a bluish purple, and is closer to the blues; the other is a redish purple, and is closer to the reds.  But these interdisciplinary researchers are closer to each other than they are to their home disciplinary clusters.   

More precisely, I suggest three hypotheses based on figure \ref{fig.conceptual} and the conceptual framework of "discursive space":  compared to their departmental peers, interdisciplinary researchers will

\begin{enumerate}[label = H\arabic*., ref = H\arabic*]
\item have greater *discursive breadth*, that is, be spread out further in discursive space; \label{h1}
\item be further from the *discursive means* of their home departments; and \label{h2}
\item be closer to their interdisciplinary peers than their departmental peers.  \label{h3}
\end{enumerate}

In a context where we expect an intervention to promote interdisciplinary research — for example, recruiting a faculty member to an ORU — I refer to these three hypotheses as the expected *discursive impacts* of the intervention.  This concept of discursive impacts provides a framework for evaluating ORUs and other interdisciplinary research initiatives.  Insofar as the UC Davis ORUs have effectively promoted interdisciplinary research, they should exhibit discursive impacts.  

I suggest that these discursive impacts can be operationalized using topic models.  Topic models begin with document-term frequency data; for example, the term "researcher" appears in a certain document 5 times.  The models then interpolate a probability distribution of topics "between" documents and terms.  For example, a given document may "contain" 50% topic 1, 25% topic 2, 10% topic 3, and so on.  Each topic, in turn, has a probability distribution over terms.  For example, topic 1 might have "researcher" with probability 1%, "starfish" with probability 0.5%, "cancer" with probability 0.0001%, and so on.  

More formally, topic models begin with observed conditional probability distributions over terms, $p(term_w | document_i)$, and fit two probability distributions $\beta_{w,t} = p(term_w | topic_t)$ and $\gamma_{t,i} = p(topic_t | document_i)$.  These conditional probability distributions allow us to quantitative compare and relate researchers to each other in a space characterized by the language that they use.  In this study, I use authors as the "documents" for the topic model; that is, the term-author distribution describes how frequently a given researcher has used the given term over their entire publication history.  

All together, insofar as the UC Davis ORUs have promoted interdisciplinary research, I expect a topic model analysis to confirm the three hypotheses for ORU researchers.  


# Data and methods #

In this study, my unit of analysis is individual researchers or authors, as individuated by the Scopus Author Identifier system[^auid], except for a few analytical moments in which I compare individual researchers to institutional entities (departments or ORUs).  My unit of observation is publications — paradigmatically, journal articles — retrieved from Scopus and aggregated as either author-level totals or concatenated blocks of text (specifically, the abstracts of an author's published work, treated as a single block of text).  

[^auid]: See <https://service.elsevier.com/app/answers/detail/a_id/11212/supporthub/scopus/> for current details on this system.  

Unless otherwise noted, all data used in this project was retrieved from Scopus, using either the web interface or application programming interface (API), between November 2018 and June 2019.  Due to intellectual property restrictions the data cannot be made publicly available.  Some downstream analysis files may be made available upon request.  

All data collection and analysis was conducted in R [@RCoreTeamLanguageEnvironmentStatistical2018].  The `RCurl` package was used for API access *[cite]*; the `spaCy` Python library was used for tokenizing, lemmatizing, and tagging abstract texts with parts of speech *[cite]*; the `cleanNLP` package was used as an interface between R and `spaCy`; the `stm` package was used to fit topic models *[cites]*; and numerous tools in the `tidyverse` suite were used to wrangle data [@WickhamTidyverseEasilyInstall2017].   

All code used in data collection and analysis is available at *[repo]* *[can I release the roster?]*  


## Author identification ##

In November 2018, the UC Davis Office of Research provided me with current rosters of faculty (tenured/tenure-track faculty) and "other academics" (primarily staff scientists) affiliated with each ORU, as well as "collaborators".  I extracted the names and ORU affiliation for all 134 affiliated faculty.  In the remainder of this paper, I refer to these ORU-affiliated faculty as "ORU faculty" and "ORU researchers," and use these terms interchangeably.  

In January 2019, I conducted searches using the Scopus web interface for all papers published with UC Davis affiliations in 2016, 2017, and 2018.  These searches returned 7,460, 7,771 and 8,066 results, respectively, for a total of 23,297 publications.  The metadata retrieved for these papers included names, affiliation strings, and Scopus author IDs for each author.  Using a combination of automated and manual matching, I identified author IDs for ORU-affiliated faculty, matching 125 out of 134 affiliated faculty.  I next searched the affiliation strings (from the publication metadata) for "Department of" to identify departmental affiliations for these faculty.  These departmental affiliation are not standardized, with 102 unique affiliation strings.  Manual inspection indicated that variations appeared in part because of spelling and abbreviation variations, e.g., "Department of Civil & Environmental Engineering" vs. "Department of Civil and Env. Engineering" vs. "Department of Civil and Environmental Engineering"; and because some affiliation strings included multiple departments (and other units, including ORUs), in varying order.  Given limited resources, and the high likelihood that these variations would wash out later in the data collection and analysis process, I elected not to clean or standardize these affiliation strings.  

To identify a comparison set of researchers, I first identified all authors in the 2016-2018 Scopus results with the same departmental affiliations; that is, an author was included in this stage if they shared at least one departmental affiliation string, and thus a departmental affiliation simpliciter, with an ORU researcher.  However, this process likely captured many graduate students and postdoctoral researchers.  Because ORU researchers are generally tenured faculty, including students and postdoctoral researchers would confound the analysis of differences between ORU and non-ORU researchers.  

I therefore used the Scopus API (application programming interface) to retrieve author-level metadata for both the ORU faculty and the potential comparison researchers.  Specifically, I examined the number of publications and the year of first publication.  After exploratory data analysis, I filtered both ORU faculty and comparison researchers, including them for further analysis only if they met all three of the following conditions:  

1. 15 or more total publications
2. First publication in or prior to 2009
3. First publication after 1970

The first two conditions appeared to effectively remove students and postdoctoral researchers, along with early-career faculty.  Note that these conditions may not exclude staff scientists or lecturers who are later in their careers.  The third condition was used to exclude a small number of researchers with very early first publication years (e.g., 1955) that were plausibly due to data errors.  

Note that, in the analysis below, departmental affiliations for all authors are based on the 2016-2018 Scopus results, not entire publication careers.  

After applying these filters, a total of 1,967 researchers had been selected for analysis, including 114 ORU-affiliated researchers and 1853 "codepartmental" comparison researchers.  Figure \ref{fig.sample} shows the number of researchers in the analysis dataset for each ORU and the comparison set, and figures \ref{fig.individual_net} and \ref{fig.org_net} show the structure of organizational relationships in the data.  (Note that, because some researchers are affiliated with multiple ORUs, the ORU counts are greater than 114.) 

![Number of researchers in the analysis dataset, for each ORU and the comparison set.  Note the y-axis uses a square-root scale. \label{fig.sample}](../plots/12_sample.png)

![Organizational relationships in the data.  Nodes are either organizations (ORUs or departments at UC Davis) or individuals (ORU faculty or comparison set authors).  Edges connect an individual to an organization.  Node size indicates betweenness centrality, which can be used as a measure of importance for information flow within the network \label{fig.individual_net}](../plots/12_network.png)

![Organizational relationships in the data.  Nodes are either ORUs or departments at UC Davis.  Edges connect an ORU to a department if the two organizations share a common faculty member.  Edge width and color indicates the number of such individuals. \label{fig.org_net}](../plots/12_oru_dept_network.png)

A substantial body of work in bibliometrics finds consistent evidence that women academics have lower publication rates and typically receive fewer citations per publication than men academics [@SymondsGenderDifferencesPublication2006; @vanArensbergenGenderDifferencesScientific2012; @LariviereBibliometricsGlobalGender2013; @GhiasiComplianceWomenEngineers2015; @BeaudryWhichGenderGap2016; @CameronSolvingProductivityImpact2016; @ChauvinGenderDifferencesResearch2019].  (I was unable to find any literature that reported findings for nonbinary, genderqueer, or trans gender identities.  @ChauvinGenderDifferencesResearch2019 note that they "planned to include faculty identifying as nonbinary or genderqueer in a separate group," but "were unable to identify any such faculty from publicly available data sources.")  To control for these gender effects, the online tool `genderize.io` was used to attribute gender to all authors based on their first or given name.  This tool resulted in a trinary gender attribution:  woman, man, and "missing" when the tool was not sufficiently confident in either primary gender attribution.  While extremely limited, this tool allows us to account for a known confounder given the limited time available for this project.  Figure \label{fig.gender} shows the distribution of attributed gender for ORU- and non-ORU-affiliated researchers and each ORU separately.  All together, ORUs appear to be slightly more men-dominated than the comparison group.  However, there is substantial variation across ORUs; all AQRC-affiliated faculty are men, more than half of PICN-affiliated faculty are women, and most ORUs have 20-40% women.  In the regression analyses below, men are used as the reference level for estimated gender effects.  

![Distribution of attributed gender for ORUs and comparison researchers.  A: All ORU-affiliated researchers grouped together.  B: Gender distribution within each ORU.  Gender attributions were made using an automated online tool based only on first or given name. \label{fig.gender}](../plots/12_gender.png)



## Productivity impacts ##

To investigate the productivity impacts of ORUs, I used author-level metadata from the Scopus API.  Specifically, all-career publication counts and (incoming) citation counts are both reported in the Scopus author retrieval API, and so these data were retrieved prior to the filtering step above.  Coauthor counts (total number of unique coauthors) were calculated from the article-level metadata retrieved for the text analysis steps discussed below.  Because coauthor counts, publication counts, and citation counts all varied over multiple orders of magnitude, I used the log (base 10) value for these variables in all analyses.  

I fit regression models for each of these three dependent variables, using ORU affiliation as the primary independent variable of interest and incorporating controls for gender, first year of publication (centered at 1997, which is the rounded mean first year in the analysis dataset), and dummy variables for departmental affiliation. 

Because of the log transformation of the dependent variables, the regression model coefficients can be exponentiated and interpreted as multiplicative associations.  For example, a coefficient of .5 can be interpreted as an association with a $10^{.5} \approx 3.16$-fold or $3.16 \times 100\% - 100\% = 216\%$ increase in the dependent variable.  

To account for relationships between the three dependent variables, I use the simplified directed acyclic graph (DAG) shown in figure \ref{fig.dag}.  According to this model, the number of coauthors influences the number of publications influences the number of citations.  The number of publications thus mediates between coauthors and citations, and coauthors mediates between the independent variables and publications; I also allow that coauthors might directly influence citations.  Both ORU affiliation and all of the included control variables (first year of publication, gender, department affiliation) might directly influence all three dependent variables.  

![A simplified directed acyclic graph (DAG) used to account for relationships between the productivity dependent variables. \label{fig.dag}](../plots/ORU_DAG.png)

## Discursive impacts ##

I use topic models and related text analysis methods to examine the discursive impacts of ORUs.  

### Topic modeling ###

Specifically, I first used the Scopus API to retrieve paper-level metadata for all authors in the analysis dataset.  I aimed to collect complete author histories — metadata for every publication each author had written in their entire career to date.  Metadata were retrieved for 121,827 distinct papers in May 2019, of which 107,964 had abstract text.  

Abstract texts were aggregated within individual authors, treating each individual author as a single "document."  For example, suppose researcher A was an author on documents 1 and 2, and researcher B was an author on documents 2 and 3.  Researcher A, as a single "document," would be represented for text analysis as the combination of abstracts 1 and 2; while researcher B would be represented as the combination of abstracts 2 and 3.  

Vocabulary selection began by using part-of-speech tagging to identify nouns in the aggregated abstract text.  I then calculated an entropy-based statistic for each noun, keeping the top 1,967 terms for further analysis to achieve a 1:1 ratio between authors ("documents") and terms.[^vocab]  Note that stopwords were not explicitly excluded at this stage; though typical lists of English stopwords do not include many nouns.  

[^vocab]: This vocabulary size is relatively small; the entire corpus contained approximately 80,000 distinct nouns.  This means that the topic models provide a relatively coarse-grained representation of "discursive space."  A larger vocabulary might have captured a more clear interdisciplinary "signal" in the language patterns, and thus led to very different findings from those presented below.  But this small vocabulary already taxed the computational resources available for this project.  

Specifically, consider the task of identifying an author (or, more generally, a "document"), selected uniformly at random from the analysis dataset.  For $N$ authors, the  unconditional probability of drawing any given author is $p_N = \frac{1}{N}$; this distribution has entropy 
\begin{align*}
    H(P) &= \sum_{N} - p_N \log_2 p_N\\
         &= - N \frac{1}{N} \log_2 \frac{1}{N}\\
         &= \log_2 N
\end{align*}

Now suppose we are given a term $w$ ("word") drawn from the token distribution of the selected author.  Because the uniform distribution has maximal entropy, the conditional author distribution given the term, $p(author_j | term_w)$, has a lower entropy $H_w \leq \log_2 N$.  Let $\Delta H_w = \log_2 N - H_w$.   $\Delta H_w$ measures the information about the identity of the author gained when we are given the term $w$.  Insofar as we are given a high-information term, we can dramatically narrow down the range of possible authors.  Terms have higher information insofar as they are specific to a smaller group of authors.  

However, typically the most high-information terms will be unique to a single author, such as typos or idiosyncratic terms.  To account for this, I also calculate the order-of-magnitude of the overall occurrence of a term, $\log_{10} n_w$.  I then take the product $\log_{10} n_w \Delta H_w$, which I represent in the code as `ndH`, and select the top terms according to this $\log_{10} n_w \Delta H_w$ statistic.  Table \ref{tab:vocab} shows the top 50 terms selected for the analysis vocabulary.  As the term list suggests, this statistic is effective at identifying terms that are clearly distinctive (in this case, to different disciplines and research fields), meaningful, and occur frequently. 

\input{../data/09_vocab.tex}

A well-known limitation of topic models is that the number of topics, $k$, is a free parameter.  Exploratory analysis of the author-term distributions using principal components found that 80% of the variance could be covered by 63 topics, and 90% of the variance could be covered by 135 topics.  Given this range, I fit models with 5, 25, 45, \ldots, 145 topics.  I calculated 5 goodness-of-fit statistics for each of these models:  semantic coherence, exclusivity [@RobertsStructuralTopicModels2014 6-7], the log likehood of a holdout subset of terms (i.e., not used to fit the model), the standard deviation of the residuals [which should converge to 1 at the "true" value of $k$; @TaddyEstimationSelectionTopic2012], and the number of iterations required by the algorithm to fit the model. 

While these goodness-of-fit statistics have been promoted for use in selecting the number of topics, they all have known limitations.  Semantic coherence favors a small number of "large" topics; exclusivity favors a large number of "small" topics.  The log likelihood and residual methods both assume that there is a "true" correct number of topics, whereas we would expect different numbers of topics at different levels of conceptual granularity (for example, at a coarse level of granularity "biology" might be a single topic, while at a finer level "ecology" and "molcular biology" might be distinct topics).  This last conceptual mismatch is directly related to what Carole Lee has called the "reference class problem for credit valuation in science" [@LeeReferenceClassProblem2020].  Inspecting these various goodness-of-fit statistics, I found that their values generally did not change much for $k \geq 45$, and were very stable for $k \geq 85$.  

Rather than selecting a single "best" topic model, in the analysis below I either (a) conduct and report analyses using all of the topic models, highlighting $k=85$, or (b) conduct analyses for $k = 25, 45, 85, 125$, reporting all four equally.  Approach (b) was generally used when the analysis involved a complex visualization component — to keep the number of plots manageable — or an intensive computation — such as all pairwise distances between authors.  

### Analyses ###

For a given value of $k$, I focused my analysis on the conditional topic distribution $\gamma_{t,i} = p(topic_t | author_i)$.  Recall the three hypotheses for discursive impacts, introduced in §*[ref]*.  For \ref{h1}, I calculated "discursive breadth" for author $i$ as the entropy of the topic distribution $H_i = H(\gamma_{\cdot, i}) = \sum_t -\gamma_{t,i} \log_2 \gamma_{t,i}$.  

\ref{h2} and \ref{h3} both appeal to a measure of "discursive distance."  I operationalize this distance between any pair of topic distributions $\gamma_1, \gamma_2$ as Hellinger distance [@HellingerDistance2021]:  
\begin{align*}
    h(\gamma_1, \gamma_2) &= \frac{1}{\sqrt 2} \sqrt{\sum_{t} (\sqrt{\gamma_{t,1}} - \sqrt{\gamma_{t,2}})^2}\\
    &= \sqrt{1 - \sum_t \sqrt{\gamma_{t,1} \cdot \gamma_{t,2}}}
    &= \sqrt{1 - \left<\sqrt\gamma_1, \sqrt\gamma_2\right>}
\end{align*}
Hellinger distances range from 0 to 1 inclusive, where 0 indicates that two distributions are the same and 1 indicates that the two distributions have completely different support.  Hellinger distance can be understood as a scaled version of the Euclidean distance between the square root vectors $\sqrt \gamma_1, \sqrt \gamma_2$; or, because the square root vectors are all unit length, as a similarity measure corresponding to the cosine similarity $\frac{\left<x, y\right>}{|x||y|}$ between the square root vectors.  Cosine similarity is widely used in bibliometrics [@MingersReviewTheoryPractice2015].  

\ref{h2} requires constructing department-level topic distributions.  For a given set of authors $X$, a conditional topic probability $\gamma_{t,X}$ can be constructed simply as the (unweighted) mean $\bar \gamma_{t, x, x \in X}$.  Note that $\sum_t \gamma_{t,X} = 1$, so that $\gamma_{\cdot, X}$ gives a probability distribution over topics.  So the simplest way to construct a department-level topic distribution might be to take this mean over the set of all members of each department.  

However, for the purposes of investigating \ref{h2}, this simple construction biases the department-level topics towards ORU faculty.  If ORU faculty are distant from the other members of their department, their contributions to the mean will act as outliers, and the resulting distance measures will be biased downward.  A second approach would be to use all and only non-ORU faculty members of the department.  But then these non-ORU faculty would be counted twice:  first they would be used to construct the mean, and then second we would calculate their distances from this mean.  Because ORU faculty would be counted only once, this approach would bias the ORU distances upwards.  

I therefore borrowed an approach from machine learning [@JamesIntroductionStatisticalLearning2013 176ff], and randomly separated non-ORU authors into two discrete subsets.  The first subset — referred to as the "training" set in machine learning — was used to construct the department-level topic distributions.  The second subset — the "testing" subset — was used to make the distance comparisons, using Hellinger distance.  50% of non-ORU authors by departmental affiliation were allocated to each subset, selected uniformly at random.  

After selecting these subsets and constructing department-level topic distributions, I calculated "departmental distance" using Hellinger distance for all ORU-affiliated faculty and all authors in testing subsets.  I used these departmental distance values as dependent variables in a series of regression models, one for each value of $k$, including first publication year, gender, log number of documents and coauthors, and department dummies as controls.  

For \ref{h3}, I calculated pairwise Hellinger distances between all ORU faculty and (a) other members of their same ORU and (b) non-ORU-affiliated members of their same department.  I then took the minimum distance in each of (a) and (b), and compared these minimums using scatterplots.  If the minimum for (a) is greater than the minimum for (b), this indicates that the ORU faculty member is closer to their home department than their ORU, conflicting with \ref{h3}.  

Because these distances are a computationally-intensive step, I did not calculate distances between pairs of non-ORU-affiliated authors, and only used selected values of $k = \{25, 45, 85, 125\}$.  I did calculate distances between all pairs of ORU-affiliated authors, for use in visualizing "discursive space."  



# Results #

## Productivity impacts ##

Regression analyses indicate that ORU affiliation is associated with a substantial increase in the number of coauthors, 1.6-2.4-fold (2.0-fold).[^estimates]  See figure \ref{fig.reg.coauths}. ORU affiliation had a much weaker direct association with the number of publications, 1.0-1.3-fold (1.2-fold), while an order-of-magnitude increase in number of coauthors had a much stronger association, 2.2-3.4-fold (2.7-fold).  See figure \ref{fig.reg.pubs}. 

[^estimates]: Statistical estimates are reported as 95% confidence intervals followed by the maximum likelihood point estimate in parentheses.  No statistical hypothesis testing was done in this paper, and so no p-values are calculated or reported.  Confidence intervals should be interpreted as a range of values that are highly compatible with the observed data, given the modeling assumptions [@AmrheinInferentialStatisticsDescriptive2018].  A confidence interval that contains zero can still be evidence of a substantial (non-zero, say, positive) relationship insofar as the bulk of the interval is greater than zero.  

![Regression estimates for number of coauthors \label{fig.reg.coauths}](../plots/12_coauths_regression.png)

![Regression estimates for number of publications \label{fig.reg.pubs}](../plots/12_pub_regression.png)

The estimates for citations are similar.  Order-of-magnitude increases in number of publications and number of coauthors are both associated with substantial increases in the number of citations a researcher has received to date:  2.7-6.9-fold (4.3-fold) for publications, 1.8-3.7-fold (2.6-fold) for coauthors.  (When number of coauthors is dropped, the point estimate for publications is approximately 10, that is, publishing 10 times as many publications is associated with receiving 10 times as many citations.)  While controlling for these midstream dependent variables, ORU affiliation is, if anything, negatively associated with citations received, .7-1.1-fold change (0.9-fold), as much as a 30% decrease.  See figure \ref{fig.reg.cites}. 

![Regression estimates for number of citations \label{fig.reg.cites}](../plots/12_cites_regression.png)

All together, in causal terms, these regression results suggest that ORU affiliation has a substantial direct effect only on the number of coauthors.  This increase in coauthors in turn leads to increased publications and increased citations; but ORU affiliation has a much smaller direct effect on these two downstream productivity measures.  On this interpretation of the findings, ORU affiliation makes faculty more productive primarily by connecting them with collaborators.  

However, the evidence for this causal interpretation is limited, because we do not have the data to compare a researchers's number of coauthors before and after they join the ORU.  The available data are consistent with a pattern where some unobserved variable is a common cause of both ORU affiliation and coauthor count.  For example, highly gregarious and extraverted faculty members might tend to have more coauthors and also be more likely to be invited to join an ORU.  



## Discursive space ##

Before discussing the discursive impacts of ORUs, I visualize "discursive space," based on the pairwise Hellinger distance between author-topic distributions for all ORU-affiliated faculty.  (Calculating distances for all pairs of researchers in the dataset was not feasible with available computational resources.)  In figure \ref{fig.mds}, these pairwise distances are represented in 2-dimensional space using classical multidimensional scaling (MDS).  If $h(\gamma_1, \gamma_2)$ is the Hellinger distance between authors 1 and 2 and $d(m_1, m_2)$ is the Euclidean distance between their corresponding points in the visualization, classical MDS attempts to find values of $m_1, m_2$ (simultaneously across all pairs) such that $d(m_1, m_2) \approx h(\gamma_1, \gamma_2)$ [@MardiaPropertiesClasicalMultidimesional1978].  That is, distance in the visualization approximates Hellinger distance.  

Figure \ref{fig.mds} shows MDS visualizations across 4 values of $k$ (numbers of topics).  Because MDS solutions are not unique up to rotations and reflections, the overall structure of the visualizations are the same across the 4 values of $k$:  3 arms or axes, roughly corresponding to environmental ORUs (JMIE, ITS, BML), medical ORUs (CCC, CHPR), and CNPRC (which has a primatological-behavioral science focus).  PICN and AQRC are located near where these 3 arms meet, suggesting a mix of environmental and health work.    

![Visualization of ORU "discursive space."  Panels correspond to different values of $k$ (number of topics).  Point positions are calculated using classical multidimensional scaling (MDS) on the pairwise Hellinger distance between author-topic distributions.  MDS solutions are not unique up to translations,rotations, and reflections.  Ellipses indicate bounds on the researchers affiliated with each ORU (based on the convex hull). \label{fig.mds}](../plots/12_mds.png)

Besides the relative positions of ORU authors in "discursive space," figure \ref{fig.mds} also provides qualitative evidence that certain ORUs are spread "further" across this space.  For example, JMIE researchers can be found on the furthest end of the environmental arm, but also near the center of the visualization and partway down the medical and CNPRC arms.  By contrast, BML and ITS researchers are located entirely within the environmental arm.  



## Discursive impacts ##

\ref{h1} states that ORU interdisciplinarity may lead to increased "discursive breadth."  Above I proposed to measure this as the entropy of an author's topic distribution.  Figure \ref{fig.reg.entropy} shows the coefficient estimates for the association between topic entropy and ORU affiliation across all topic models.  Across all models, confidence intervals generally cover from -0.6 to 0.1 bits, with point estimates in the range -0.2 to -0.3 bits.  That is, these models suggest generally indicate *decreased* discursive breadth; this suggests that ORU authors may be more specialized than their departmental peers.  

![Coefficient estimates for association between topic entropy and ORU affiliation across values of $k$ (number of topics).  Whiskers are 95% confidence intervals.  Regression models include as controls attributed gender, first year of publication, number of publications, number of coauthors, and departmental dummies.   $k=85% is highlighted as the single "best" model, though this should not be overinterpreted.  \label{fig.reg.entropy}](../plots/12_entropy_regression.png)  

However, using 0.5 bits ("half of a coin flip") as a threshold for substantive difference (that is, treating any value between -0.5 and 0.5 as too small to be interesting), overall the models indicate that the difference in discursive breadth between ORU authors and their peers is *trivial*.  In either case (whether discursive breadth is narrower or the difference is trivial), \ref{h1} does not seem to be supported.  


\ref{h2} states that ORU interdisciplinarity may lead to increased departmental distance, that is, increased Hellinger distance from the departmental mean topic distribution.  Figure \ref{fig.reg.dept_dist} shows coefficient estimates for the association between topic entropy and ORU affiliation across values of $k$ (number of topics), along with 95% confidence intervals.  Here the estimates may appear to support \ref{h2}, as the estimates are generally positive.  However, most confidence intervals end well below .05, and point estimates almost all approximately .02.  Recall that Hellinger distance is on a 0-1 scale.  On this scale, distances less than .05 would seem to be trivial.  That is, there does not seem to be a meaningful difference between ORU faculty and their departmental peers, and so \ref{h2} does not appear to be supported either.  

![Coefficient estimates for association between distance to departmental mean distribution and ORU affiliation across values of $k$ (number of topics).  Whiskers are 95% confidence intervals.  Regression models include as controls attributed gender, first year of publication, number of publications, number of coauthors, and departmental dummies.   $k=85% is highlighted as the single "best" model, though this should not be overinterpreted.  \label{fig.reg.dept_dist}](../plots/12_dept_dist_reg.png)  

Due to differences ORU and department size, as well as varying research foci, it might be suspected that departmental distance effects could vary across ORUs.  Figure \ref{fig.reg.dept_dist_fixed} reports coefficient estimates for ORU dummy variables, rather than the binary yes/no ORU affiliation used above; "no ORU affiliation" is used as the contrast value for the ORU dummies.  

![Coefficient estimates for association between distance to departmental mean distribution and ORU affiliation across values of $k$ (number of topics).  Whiskers are 95% confidence intervals.  Regression models include as controls attributed gender, first year of publication, number of publications, number of coauthors, and departmental dummies. "No ORU affiliation" is used as the contrast value. Dashed lines indicate the thresholds for trivial values at ±.05.  $k=85% is highlighted as the single "best" model, though this should not be overinterpreted.   \label{fig.reg.dept_dist_fixed}](../plots/12_dept_dist_fixed_reg.png)

Figure \ref{fig.reg.dept_dist_fixed} does indeed suggest that the potential effects of ORU affiliation on departmental distance do vary across ORUs, albeit still to a limited extent.  In no case do we see consistent evidence (across values of $k$) of an effect larger than ±.10.  However, there does seem to be some evidence of a *non-trivial effect for CNPRC*, with point estimates generally just above .05.  Surprisingly, there is also some evidence of a *negative effect for ITS and PICN*.  That is, there is some evidence that these researchers are *closer* to the departmental means than a randomly-selected subset of their departmental peers.  This last signal is far from clear, however; and generally figure \ref{fig.reg.dept_dist_fixed} indicates *no substantive difference* in departmental distance between ORU researchers and their peers.  

\ref{h3} proposes that ORU interdisciplinarity leads researchers to be closer to their ORU peers than their (non-ORU-affiliated) departmental peers in discursive space.  Figure \ref{fig.silhouette} shows scatterplots for minimal distances to both kinds of peers, for each ORU and 4 values of $k$.  In these scatterplots, the dashed line indicates $y=x$.  Points above this line are closer to ORU peers than departmental peers, and so these points would support \ref{h3}.  

![Minimal distance to ORU peers vs. departmental peers.  Both x- and y-axes are on Hellinger distance scale (0-1).  The dashed line in each panel indicates $y=x$.  Points above this line are closer to ORU peers than departmental peers, supporting \ref{h3}.  Note that comparisons to the dashed line should be made *vertically* or *horizontally*.    \label{fig.silhouette}](../plots/12_oru_dept_min_dist.png)

For most ORUs, across values of $k$, most researchers are located near or somewhat below the dashed line.  This means that researchers are typically *equidistant or closer to their closest departmental peers* than their closest ORU peers.  

Because distance comparisons in scatterplots can be misleading (comparing vertical distance to the dashed line, not Euclidean distance), figure \ref{fig.ridges} shows the distribution of these comparisons.  In this figure, positive x-axis values indicate that departmental distance is greater than ORU distance, supporting \ref{h3}.  In this figure, modal and median values are all negative or near 0, thus conflicting.  While there are a few exceptional individuals, ORU faculty are generally *equidistant from or closer to their closest departmental peers*.  As with the other two hypotheses, these findings conflict with \ref{h3}.  

![Comparison of minimal ORU and departmental distances, across ORUs and values of $k$.  Positive x-axis values indicate that departmental distance is greater than ORU distance, supporting \ref{h3}.  Dashed vertical line indicates 0; solid lines within densities indicate median values; and small vertical dashes indicated individual values.\label{fig.ridges}](../plots/12_oru_dept_min_dist_ridges.png)



# Discussion #

The analysis of productivity impacts suggests that ORUs do increase the productivity of affiliated faculty.  Specifically, the sequence of regression models suggests that ORUs increase the number of coauthors that affiliated faculty have; that this increased collaboration leads to increased publications and citations; but that ORUs have at most a small direct effect on publications and citations.  

However, as already noted above, due to the limitations of the study design we cannot be sure whether this relationship is indeed an effect of ORUs on productivity.  The data are also compatible with an effect of productivity on ORUs (i.e., ORUs tend to recruit faculty who already tend to be more productive), an unmeasured common cause (e.g., more extraverted faculty tend both to be more productive and to be recruited to ORUs), or indeed a complex combination of multiple causal relationships.  

Bracketing these concerns about causal inference, the productivity findings of this study suggest that ORUs — and similar infrastructure for interdisciplinary research — may have a key social network formation role.  @HicksNationalRoboticsInitiative2019 and @HicksNetworkAnalysisEvaluate2019 also analyzed specific interdisciplinary research funding programs, finding evidence that these programs appeared to be effective at supporting novel collaborations and stimulating the formation of a new research community, respectively.  

*[1 more paragraph?]*


Turning to discursive impacts, the data do not appear to be compatible with any of the three hypotheses.  In each case, the expected effects of interdisciplinarity (rather than multidisciplinarity) do not appear.  This suggests that ORUs at UC Davis have fostered multidisciplinarity rather than interdisciplinarity.  

- multidisciplinary is often seen as inferior to interdisciplinarity, eg, "mere juxtaposition" (Holbrook 3)
- o:  ID is hard (O'Rourke; Brister)
- o:  ID is incoherent (Holbrook 14)
- o:  discursive MD is compatible with ID interaction (Holbrook 10, on Kuhn-MacI)




Brister, "disciplinary capture"
- might explain the narrowness here
- might be compatible w/ spread of departmental distances


# References #


