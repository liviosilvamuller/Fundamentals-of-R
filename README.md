# Fundametals of R

This is a public repository containing class materials for Fundamentals of R course.

## Instructors 

[Henrique Sposito](http://henriquesposito.com/), Ph.D. Candidate in IRPS, henrique.sposito@graduateinstitute.ch

Henrique holds a bachelor’s degree in Sociology and Political Science from the University of Alabama at Birmingham, and master’s degree in International Relations and Political Science from the Graduate Institute. He is currently a PhD candidate at the International Relations and Political Science Department at the Graduate Institute. His dissertation leverages advanced text analysis techniques in R, such as supervised machine learning, to investigate how authenticity, problem construction, and urgency appear and change over time and across settings in discursive politics. Henrique is also a Research Assistant in the "PANARCHIC: Power and Network and the Rate of Change in Institutional Complexes" project at the Center for International Environmental Studies (CIES). For the project, he develops, contributes, and helps maintain several R packages that assist researchers dealing with multiple, overlapping, and uncertain datasets across various issues domains of Global Governance.

[Livio Silva-Muller](https://www.silvamuller.com/), Ph.D. Candidate in ANSO, livio.silva@graduateinstitute.ch

Livio holds a bachelor’s degree in International Affairs from the University of St. Gallen and a master’s degree in Development Studies from the Geneva Graduate Institute. He is a Ph.D. candidate in Anthropology and Sociology at the Graduate Institute, working on the intersection of climate change, policy effectiveness, and transnational finance. His dissertation utilises longitudinal grant-level data, textual data, and in-depth interviews to answer how governments adopt effective climate mitigation policies. Livio also works as a research assistant at the SNF Elites & Inequality project, which relies on survey data to estimate elites’ support for redistributive projects and the cultural process that enable this support. Finally, Livio provides data-related consulting services to organisations based in Geneva.

## Course Description

R is a programming language and open-source software that allows users to import, transform, and analyse diverse types of data. Academics, governments, and industry use R data collection, data visualisation, and data analysis. This summer school is a hands-on introduction to R, starting from scratch. In separate lectures, this course  covers fundamental tasks in R such as how to import different types of data; how to clean and manipulate objects; and how to create beautiful visualisations. Each lecture is matched with a topical case studies aimed at illustrating a practical application of the fundamentals of R to cover key social science questions related to the environment, conflict, and democracies.

By the end of this course, participants should be able to (1) perform simple data analysis, (2) communicate findings with visualisations, and (3) produce integrated reports using R.

## Course Outline

### Lecture 1: Introduction to R

We start by understanding what R is, how to install and open the software, and a few basic concepts.

- What is R language and software?
- What is R used for?
- How to interact with R studio?

*Suggested Readings:*

- [Getting started with R and RStudio (Chapter 1)](https://intro2r.com/chap1.html) in Douglas, A., Ross, D., Mancini, F., Couto, A. & Lusseau, D. (2022). [An Introduction to R](https://intro2r.com/).
- Introduction and preliminaries (Chapter 1) in Venables, W. N., Smith, D. M., & R
Development Core Team. (2022). [An introduction to R](https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf).

## Lecture 2: Objects, Class, and Data Structures

R holds different objects with diverse classes. This lecture helps participants understand what R classes are and how R functions operate in these. Moreover, we will learn how to properly get external data into R, one of the most important tasks new users should be able to perform. RStudio and several other R packages have made these tasks easier and more flexible.

- What are object classes and functions in R?
- How to perform arithmetic operations in R?
- How to import text (.csv) and excel (.xlsx) files to R?
- How to explore newly imported data in R?

*Suggested Readings:*:

- [Some R basics (Chapter 2)](https://intro2r.com/basics_r.html) and [Data in R (Chapter 3)](https://intro2r.com/data_r.html) in Douglas, A., Ross, D., Mancini, F., Couto, A. & Lusseau, D. (2022). [An Introduction to R](https://intro2r.com/)
- [Data import (Chapter 11)](https://r4ds.had.co.nz/data-import.html) in Wickham, H., & Grolemund, G. (2017).  [R for data science: import, tidy, transform, visualize, and model data.](https://r4ds.had.co.nz/index.html)

## Lecture 3: Cleaning and Wrangling Data

Data often does not come ready for the analysis you want to perform. Names within
variables might not match (e.g. USA, US, United States), observations are missing (e.g. NA), among other human or programmatic coding errors. In this lecture, we will go through the fundamentals of (re)shaping data for our purposes using the dplyr package.

- How to create and modify variables in data?
- How to group, filter, and summarise information contained in variables?
- How to merge multiple datasets?
- How to detect inconsistencies in data and harmonise strings?
- How to treat missing data?

*Suggested Readings:*

- [Data transformation (Chapter 5)](https://r4ds.had.co.nz/transform.html) in Wickham, H., & Grolemund, G. (2017).  [R for data science: import, tidy, transform, visualize, and model data.](https://r4ds.had.co.nz/index.html)
- [Data Wrangling with dplyr in R (Gonzalez, K. 2021)](https://www.youtube.com/watch?v=L1kRVGv6NC8) - Video
- [Regular Expressions (Chapter 17)](https://bookdown.org/rdpeng/rprogdatascience/regular-expressions.html) in Peng, R. D. (2022). [R programming for data science.](https://bookdown.org/rdpeng/rprogdatascience/)

## Lecture 4: Principles and Practices of Data Visualization

Visualizing your data is the bread and butter of any analysis you will perform. In this lecture, we will learn the principles of good data visualization with the ggplot2 package. We introduce how to create different types of plots in R as well as how to customize plots with trends, annotations, and labels to make them “tell a story”.

- How to create bar charts, scatter plots, box plots, and line trends?
- How to quickly visualise variation using facets?
- How to include trend lines in plots?
- How to plot multiple relationships between variables?
- How to annotate information to make plots more informative?

*Suggested Readings:*

- [Visualization (Chapter 3)](https://r4ds.had.co.nz/data-visualisation.html) and [Graphics for communication (Chapter 28)](https://r4ds.had.co.nz/graphics-for-communication.html) in Wickham, H., & Grolemund, G. (2017). in Wickham, H., & Grolemund, G. (2017). [R for data science: import, tidy, transform, visualize, and model data.](https://r4ds.had.co.nz/index.html)
- [First steps (Chapter 2)](https://ggplot2-book.org/getting-started.html) in Wickham, H. (2016). [ggplot2: Elegant Graphics for Data Analysis.](https://ggplot2-book.org/)

## Lecture 5: Exchanging with the community

Now that you know the fundamentals, you will want to share results and ask for help. In this lecture, we will learn how to use [R Markdown(https://rmarkdown.rstudio.com/lesson-1.html) to export reproducible reports in different file formats (e.g. .pdf, .docx, and HTML). We will also discuss how to ask for help online using minimal reproducible examples.

- What is R Markdown?
- How to export integrated reports ready for sharing?
- How to create minimal reproducible examples?

*Suggested readings:*

- [Basics (Chapter 2)](https://bookdown.org/yihui/rmarkdown/basics.html) in Xie, Y., Allaire, J. J., & Grolemund, G. (2018). [R markdown: The definitive guide.](https://bookdown.org/yihui/rmarkdown/)
- [How to make a reprex?](https://www.r-bloggers.com/2020/10/how-to-make-a-reprex/) - Blog Post
- [R Markdown (Chapter 27)](https://r4ds.had.co.nz/r-markdown.html) in Wickham, H., & Grolemund, G. (2017). [R for data science: import, tidy, transform, visualize, and model data.](https://r4ds.had.co.nz/index.html)

## Adittional Materials

Besides the great open-source books mentioned above, we also recommend recommend the following books:

Mesquita, E. B., & Fowler, A. (2021). Thinking clearly with data: A guide to quantitative reasoning and analysis. Princeton University Press.

Healy, K. (2018). Data Visualisation: a practical introduction. Princeton University Press.
