# Latin American Public Opinion Project (LAPOP) Analysis

Project to better understand __population movement drivers in the Northern Triangle__(Honduras, El Salvador & Guatemala).

The analysis is based on longitudinal analysis of [Latin American Public Opinion Project (LAPOP)](http://datasets.americasbarometer.org/database/index.php) survey from 2004 to 2018 (_i.e. 8 survey dataset per country_).

This projects build from analysis of other shared repositories:

 * [LAPOP-SMA](https://github.com/ccjolley/LAPOP-SMA) from [Craig Jolley](jolleycraig@gmail.com), Snr Data Scientist @ USAID
 * [lapop-dashboard](https://github.com/vanderbilt-data-science/lapop-dashboard) from Vanderbilt Data Science Team
 * [LAPOP-predictive-models](https://github.com/carmen-canedo/LAPOP-predictive-models)
 
The scripts from those various repository were re-organized in an R package in order to ease analysis reproducibility.

## Key Research question

* What influence the perception of being forcibly displaced
* Are there specific profiles that are moving Vs not able to move
* Are economic conditions sufficient to motivate the intention to move?
* Can we identify displacement hot-spots?

 
## Package functions

The package:

 1. Pull survey data - download all of the merged files that exist for each country individually from the [LAPOP Datasets](http://datasets.americasbarometer.org/database/index.php). Because of the [dataset licence](datasets.americasbarometer.org/database/agreement.html), data are not included in the package

 2. Map and merge all survey from different year and country. This implies:
    * Adding Wave Column: Creates a column that contains the correct wave for datasets.
    * Creating Unique ID: Makes a unique ID for merged country files that follows the unique ID of 2016/17 files.
    * Lengthening and Joining: Lengthens countries into tidy format & Joins the questions and category columns by column_name
    * Finding Common Questions: Flag questions that are asked across all countries
    
    
 3. Generate a series of indices, calculated for each region based on common variable to explore trends and statistical clusters. Each of the following index are explained in a dedicated vignette:
    * Authoritarianism index `aut_idx`
    * Community Activity index `ca_idx`
    * Sympathy with government critics index `crit_idx`
    * Fear index `fear_idx`
    * Transparency index `tr_idx`
    * Trust in Government Index `tr_idx`
    * Wealth index `w_idx`
    * Possession index `p_idx`
 
 4. Generate a series of pre-built charts to display the results and get ground ready for Joint Data Interpretation by subject mater expert
 
 
## Getting Started
These are the steps you should take to begin using this repo.

### Prerequisites
#### R Studio
You will need the current version of [RStudio](https://www.rstudio.com/products/rstudio/#Desktop) to run this code.

## Install

The package is still under development - need to fix the scrapping functions

```{r}
remotes::install_github(unhcr-americas/LAPOP-SMA)
```

In order to use the package, the following packages will also be installed [tidyverse](https://www.tidyverse.org/packages/), [haven](https://cran.r-project.org/web/packages/haven/haven.pdf), [labelled](https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html), [sjmisc](https://cran.r-project.org/web/packages/sjmisc/sjmisc.pdf), [assertr](https://cran.r-project.org/web/packages/assertr/vignettes/assertr.html), [janitor](https://cran.r-project.org/web/packages/janitor/janitor.pdf), [rlang](https://cran.r-project.org/web/packages/rlang/rlang.pdf).

## Vignettes
Vignettes are R Markdown file (.rmd) that runs interactively. It does not require any additional installation, as it is a built-in function in R Studio. For an in-depth tutorial, visit this [R Notebook Guide](https://bookdown.org/yihui/rmarkdown/notebook.html). There are also [cheat sheets](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf) available for how to use R Markdown.

## Reference dataset / code book
The package includes for convenience the question __categories__ and __labels__ from different versions, based on [Merge_Codebook_v1.0](http://datasets.americasbarometer.org/database/files/2004-2018%20LAPOP%20AmericasBarometer%20Merge_Codebook_v1.0_FREE_W.pdf)

The package also include a geographic dataset to ease geo-visualization & analysis


#### Building package documentation 

`devtools::document()`
`devtools::check(document = FALSE)`
`pkgdown::build_site()`
------------