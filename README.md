# Latin American Public Opinion Project (LAPOP) Analysis

Project to better understand __population movement drivers in the Northern Triangle__ (_Honduras, El Salvador & Guatemala_). The __[Analysis Executive summary is here](summary.html)__

The analysis is based on longitudinal analysis of [Latin American Public Opinion Project (LAPOP)](http://datasets.americasbarometer.org/database/index.php) survey from 2004 to 2018 (_i.e. 8 survey dataset per country_) and complements the exploration already available for [Honduras](https://www.vanderbilt.edu/lapop/honduras/AB2018-19_Honduras_RRR_W_09.25.19.pdf), [El Salvador](https://www.vanderbilt.edu/lapop/es/AB2018-19_El_Salvador_RRR_Presentation_W_09.25.19.pdf) & [Guatemala](https://www.vanderbilt.edu/lapop/guatemala/AB2018-19_Guatemala_RRR_Presentation_W_09.25.19.pdf).

The [AmericasBarometer](https://www.vanderbilt.edu/lapop/about-americasbarometer.php) is a periodic study of 34 countries in the Western Hemisphere, with stratified nationally representative samples drawn in each country, a common questionnaire core, and country-specific modules. It is the only scientifically rigorous comparative survey of democratic values and behaviors that covers all independent countries in North, Central, and South America, as well as a significant number of countries in the Caribbean. The Americas Barometer measures attitudes, evaluations, experiences, and behavior in the Americas using national probability samples of voting-age adults. Standardizing methods and a common core questionnaire permit valid comparisons across countries and over time on topics including the economy, rule of law, state capacity, trust in institutions, individual values, corruption, security, and more.

Each country survey is implemented based on a national probability design. In some cases, oversamples are collected to allow precise analysis of opinion within sub-national regions. These data were supplied by the Latin American Public Opinion Project at Vanderbilt University, which takes no responsibility for any interpretation of the data. 

This projects build from analysis of other shared repositories such as [LAPOP-SMA](https://github.com/ccjolley/LAPOP-SMA) from [Craig Jolley](jolleycraig@gmail.com), Snr Data Scientist @ USAID, [lapop-dashboard](https://github.com/vanderbilt-data-science/lapop-dashboard) from Vanderbilt Data Science Team or [LAPOP-predictive-models](https://github.com/carmen-canedo/LAPOP-predictive-models)
 
The scripts from those various repository were re-organized in an R package in order to ease analysis reproducibility. This package also aims at building capacity of humanitarian data analyst in operations

## Key questions

The main focus of the study is organized around 3 linked questions that can influence population movement mitigation program:

|      Reearch      |  Programme Design         |
|:-------------|:-------------|
|   What influence the intention to move to another country: push & pull factor? How different measurement of perception are correlated: protection-related push factor vs economic pull factor? Are economic conditions sufficient to motivate the intention to move? | Resources allocation to protection intervention vs economic support |
|     Can we identify displacement hot-spots? What make those hot-spots specific?  |   Geographic targeting and area-based approach for service delivery design |
|  Are there specific profiles that are intending vs those who do not intend to move? |  Household targeting for cash intervention |


 
## Getting Started
These are the steps you should take to begin using this repo.

### Prerequisites 
You will need the current version of [R Statistical Language](https://www.r-project.org/) & [RStudio](https://www.rstudio.com/products/rstudio/#Desktop) to run this code.

### Install

The package is still under development - if you want to fix- improve - contribute - please fork it in github and install locally the package. 
```{r}
devtools::install()
```

In order to use the package, the following packages will also be installed [tidyverse](https://www.tidyverse.org/packages/), [haven](https://cran.r-project.org/web/packages/haven/haven.pdf), [labelled](https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html), [sjmisc](https://cran.r-project.org/web/packages/sjmisc/sjmisc.pdf), [assertr](https://cran.r-project.org/web/packages/assertr/vignettes/assertr.html), [janitor](https://cran.r-project.org/web/packages/janitor/janitor.pdf), [rlang](https://cran.r-project.org/web/packages/rlang/rlang.pdf).

You will be then able to use all functions with the prefix: `AmericasBarometer::` 
Once installed, you need to download and reshape the data with 

```{r}
AmericasBarometer::get_dataLAPOP()
```

### Vignettes

Vignettes are R Markdown file (.rmd) that runs interactively stored int the `vignettes` folder. It does not require any additional installation, as it is a built-in function in R Studio. For an in-depth tutorial, visit this [R Notebook Guide](https://bookdown.org/yihui/rmarkdown/notebook.html). There are also [cheat sheets](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf) available for how to use R Markdown.

Once data have been downloaded, you can then knit the vignettes and the full site with
```{r}
devtools::document()

pkgdown::build_site()
```

This will generate the full package documentation in the `docs` folder. 

For the sake of analysis, __7 composite indicators__ are created.

 1. Authoritarianism index `aut_idx`  
 2. Community Activity index `ca_idx`  
 3. Sympathy with government critics index `crit_idx`  
 4. Fear index `fear_idx` 
 5. Transparency index `transp_idx`   
 6. Trust in Government Index `trust_idx` 
 7. Wealth index `weahlt_idx`  


In technical terms, this implies to perform for each of those indices missing value imputation and principal component analysis. Then for each indices, a prediction model is created, spatial heterogeneity and time trends are explored.

In addition the following are also available:

 * Calculating some high-level data quality points
 * Verify correlations between all indices
 
Specific variable in relation with protection are detailed:

 * Out of fear of being a crime victim
 * Statistics on bribery

Last to support both geographic and household targeting the last two analysis are performed:  

 * Cluster Analysis
 * Spatial heterogeneity


### Package functions

The package includes functions stored in the `R` folder:

 1. Pull survey data - download all of the merged files that exist for each country individually from the [LAPOP Datasets](http://datasets.americasbarometer.org/database/index.php). Because of the [dataset licence](http://datasets.americasbarometer.org/database/agreement.html), data are not included in the package

 2. Map and merge all survey from different year and country. This implies:
 * Adding Wave Column: Creates a column that contains the correct wave for datasets.
 * Creating Unique ID: Makes a unique ID for merged country files that follows the unique ID of 2016/17 files.
 * Lengthening and Joining: Lengthens countries into tidy format & Joins the questions and category columns by column_name
 * Finding Common Questions: Flag questions that are asked across all countries
    
    
 3. Generate a series of indices, calculated for each region based on common variable to explore trends and statistical clusters. Each of the indices are described in a dedicated vignette.
 
 4. Generate a series of pre-built charts to display the results and get ground ready for Joint Data Interpretation by subject mater expert
 
If you change the functions, regenerate the documentation before rebuilding the package 
```{r}
devtools::document()
devtools::check(document = FALSE)
```

### Reference dataset / code book
The package includes for convenience the question __categories__ and __labels__ from different versions, based on [Merge_Codebook_v1.0](http://datasets.americasbarometer.org/database/files/2004-2018%20LAPOP%20AmericasBarometer%20Merge_Codebook_v1.0_FREE_W.pdf)

The package also include a geographic dataset to ease geo-visualization & analysis

