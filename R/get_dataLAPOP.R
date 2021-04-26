#' make_codebook
#' 
#' Create a codebook
#' https://stackoverflow.com/questions/39671621/extract-the-labels-attribute-from-labeled-tibble-columns-from-a-haven-import-f
#'
#' @md
#' @param tbl labeled dataframe
#' @examples
#' \dontrun{
#' elsalvador2018 <- read_dta("http://datasets.americasbarometer.org/database/files/ElSalvador%20LAPOP%20AmericasBarometer%202018%20v1.0_W.dta")
#' elsalvador2018_codebook <- make_codebook(elsalvador2018)
#' }
#' @export

make_codebook <- function(tbl){
  n <- ncol(tbl)
  labels_list <- map(1:n, function(x) attr(tbl[[x]], "label") )
  # if a vector of character strings is preferable
  labels_vector <- map_chr(1:n, function(x) attr(tbl[[x]], "label") )
  vars_vector <- labels(tbl)[[2]]
  levels_vector <- lapply(lapply(tbl, table), length) %>% as_vector()
  return(tibble("var" = vars_vector, "label" = labels_vector, "levels" = levels_vector))
}







#' data_raw_dir
#' 
#' This function creates a data folder if one doesn't already exist.
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export


data_raw_dir <- function() {
  if(!dir.exists("data-raw")) dir.create("data-raw")
}

#' get_dataLAPOP
#' 
#' Get all country specific dataset and do a full merge and get the The full Grand Merge data set
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export

get_dataLAPOP <- function(){
  
  # http://datasets.americasbarometer.org/database/files/2004-2018%20LAPOP%20AmericasBaometer%20Merge_FREE_V1.0_W.zip
  #data <- read_dta("data-raw/down/2004-2018 LAPOP AmericasBarometer Merge (v1.0FREE).dta")
  
 srcs <- 
   list(guatemala2019 = "http://datasets.americasbarometer.org/database/files/Guatemala%20LAPOP%20AmericasBarometer%202019%20v1.0_W.dta",
        
        
        honduras2018 = "http://datasets.americasbarometer.org/database/files/Honduras%20LAPOP%20AmericasBarometer%202018%20v1.0_W.dta",
        honduras2004 ="http://datasets.americasbarometer.org/database/files/1997861390honduras%202004%20export%20version.dta",
        honduras2006 ="http://datasets.americasbarometer.org/database/files/1631591650honduras_lapop_final%202006%20data%20set%20092906.dta",
        honduras2008 ="http://datasets.americasbarometer.org/database/files/1698154699el_salvador_lapop_dims_2008_final_lapop08_v10.dta",
        honduras2010 ="http://datasets.americasbarometer.org/database/files/316969358El%20Salvador_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20v3.dta",
        honduras2012 ="http://datasets.americasbarometer.org/database/files/1126250629ElSalvador%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta",
        honduras2014 ="http://datasets.americasbarometer.org/database/files/1234285428ElSalvador%20LAPOP%20AmericasBarometer%202014%20v3.0_W.dta",
        honduras2016 ="http://datasets.americasbarometer.org/database/files/381267663ElSalvador%20LAPOP%20AmericasBarometer%202016%20V1.0_W.dta",
        
        
        elsalvador2018 = "http://datasets.americasbarometer.org/database/files/ElSalvador%20LAPOP%20AmericasBarometer%202018%20v1.0_W.dta",
        elsalvador2004 ="http://datasets.americasbarometer.org/database/files/1762554388el%20salvador%202004%20export%20version.dta",
        elsalvador2006 ="http://datasets.americasbarometer.org/database/files/223466081el%20salvador_lapop_final%202006%20data%20set%20092906.dta",
        elsalvador2008 ="http://datasets.americasbarometer.org/database/files/1698154699el_salvador_lapop_dims_2008_final_lapop08_v10.dta",
        elsalvador2010 ="http://datasets.americasbarometer.org/database/files/316969358El%20Salvador_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20v3.dta",
        elsalvador2012 ="http://datasets.americasbarometer.org/database/files/1126250629ElSalvador%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta",
        elsalvador2014 ="http://datasets.americasbarometer.org/database/files/1234285428ElSalvador%20LAPOP%20AmericasBarometer%202014%20v3.0_W.dta",
        elsalvador2016 ="http://datasets.americasbarometer.org/database/files/381267663ElSalvador%20LAPOP%20AmericasBarometer%202016%20V1.0_W.dta")
 
 data <- srcs %>% map_dfr(compose(as_factor, read_dta))
 
 ## Get a consolidated data dictionnary
 dico <- srcs %>% map_dfr(compose(make_codebook(read_dta)))
 
 
 
 library(foreign) # needed to import STATA files
 library(plyr)
 library(ggplot2)
 library(stringr)
 library(reshape2)
 library(RColorBrewer)
 library(stringi)
 
 
 ###############################################################################
 # Data cleaning: Create a data frame called lapop.trends containing all 
 # indicators that are consistent across countries and years.
 ###############################################################################
 yrs <- c('2004','2006','2008','2010','2012','2014')
 fnames <- sapply(c('GTM','HND','SLV'),function(x) paste(yrs,x,sep ='-'))
 rownames(fnames) <- yrs
 fnames[1:5,] <- paste(fnames[1:5,],'.dta',sep='')
 fnames[6,] <- paste(fnames[6,],'.csv',sep='')
 
 # this isn't the most compact way to do this, but as long as the datasets
 # are heterogeneous it sort of makes sense
 lapop.2004.GTM <- read.dta(fnames['2004','GTM'],convert.factors=FALSE)
 lapop.2004.HND <- read.dta(fnames['2004','HND'],convert.factors=FALSE)
 lapop.2004.SLV <- read.dta(fnames['2004','SLV'],convert.factors=FALSE)
 lapop.2006.GTM <- read.dta(fnames['2006','GTM'],convert.factors=FALSE)
 lapop.2006.HND <- read.dta(fnames['2006','HND'],convert.factors=FALSE)
 lapop.2006.SLV <- read.dta(fnames['2006','SLV'],convert.factors=FALSE)
 lapop.2008.GTM <- read.dta(fnames['2008','GTM'],convert.factors=FALSE)
 lapop.2008.HND <- read.dta(fnames['2008','HND'],convert.factors=FALSE)
 lapop.2008.SLV <- read.dta(fnames['2008','SLV'],convert.factors=FALSE)
 lapop.2010.GTM <- read.dta(fnames['2010','GTM'],convert.factors=FALSE)
 lapop.2010.HND <- read.dta(fnames['2010','HND'],convert.factors=FALSE)
 lapop.2010.SLV <- read.dta(fnames['2010','SLV'],convert.factors=FALSE)
 lapop.2012.GTM <- read.dta(fnames['2012','GTM'],convert.factors=FALSE)
 lapop.2012.HND <- read.dta(fnames['2012','HND'],convert.factors=FALSE)
 lapop.2012.SLV <- read.dta(fnames['2012','SLV'],convert.factors=FALSE)
 lapop.2014.GTM <- read.csv(fnames['2014','GTM'])
 lapop.2014.HND <- read.csv(fnames['2014','HND'])
 lapop.2014.SLV <- read.csv(fnames['2014','SLV'])
 
 # before we can check for consistent indicators, we need to make them all lowercase
 names(lapop.2004.GTM) <- tolower(names(lapop.2004.GTM))
 names(lapop.2004.HND) <- tolower(names(lapop.2004.HND))
 names(lapop.2004.SLV) <- tolower(names(lapop.2004.SLV))
 names(lapop.2006.GTM) <- tolower(names(lapop.2006.GTM))
 names(lapop.2006.HND) <- tolower(names(lapop.2006.HND))
 names(lapop.2006.SLV) <- tolower(names(lapop.2006.SLV))
 names(lapop.2008.GTM) <- tolower(names(lapop.2008.GTM))
 names(lapop.2008.HND) <- tolower(names(lapop.2008.HND))
 names(lapop.2008.SLV) <- tolower(names(lapop.2008.SLV))
 names(lapop.2010.GTM) <- tolower(names(lapop.2010.GTM))
 names(lapop.2010.HND) <- tolower(names(lapop.2010.HND))
 names(lapop.2010.SLV) <- tolower(names(lapop.2010.SLV))
 names(lapop.2012.GTM) <- tolower(names(lapop.2012.GTM))
 names(lapop.2012.HND) <- tolower(names(lapop.2012.HND))
 names(lapop.2012.SLV) <- tolower(names(lapop.2012.SLV))
 names(lapop.2014.GTM) <- tolower(names(lapop.2014.GTM))
 names(lapop.2014.HND) <- tolower(names(lapop.2014.HND))
 names(lapop.2014.SLV) <- tolower(names(lapop.2014.SLV))
 
 # add years 
 lapop.2004.GTM$year <- 2004; lapop.2004.HND$year <- 2004; lapop.2004.SLV$year <- 2004
 lapop.2006.GTM$year <- 2006; lapop.2006.HND$year <- 2006; lapop.2006.SLV$year <- 2006
 lapop.2008.GTM$year <- 2008; lapop.2008.HND$year <- 2008; lapop.2008.SLV$year <- 2008
 lapop.2010.GTM$year <- 2010; lapop.2010.HND$year <- 2010; lapop.2010.SLV$year <- 2010
 lapop.2012.GTM$year <- 2012; lapop.2012.HND$year <- 2012; lapop.2012.SLV$year <- 2012
 lapop.2014.GTM$year <- 2014; lapop.2014.HND$year <- 2014; lapop.2014.SLV$year <- 2014
 
 # The idnum field wasn't used in 2004 (or in 2006 in SLV); this can be handy 
 # if I need to join the complete dataset with individual ones again.
 set.seed(12345)
 lapop.2004.GTM$idnum <- stri_rand_strings(nrow(lapop.2004.GTM),60)
 lapop.2004.SLV$idnum <- stri_rand_strings(nrow(lapop.2004.SLV),60)
 lapop.2006.SLV$idnum <- stri_rand_strings(nrow(lapop.2006.SLV),60)
 lapop.2004.HND$idnum <- stri_rand_strings(nrow(lapop.2004.HND),60)
 # sanity check
 # nrow(lapop.2004.GTM)+nrow(lapop.2004.SLV)+nrow(lapop.2006.SLV)+nrow(lapop.2004.HND)
 # length(unique(c(lapop.2004.GTM$idnum,lapop.2004.SLV$idnum,
 #                 lapop.2006.SLV$idnum,lapop.2004.HND$idnum)))
 
 
 # I <3 recursive functions
 intersection <- function(x, y, ...){
   if (missing(...)) intersect(x, y)
   else intersect(x, intersection(y, ...))
 }
 
 trend.all <- intersection(names(lapop.2004.GTM),names(lapop.2004.HND),names(lapop.2004.SLV),
                           names(lapop.2006.GTM),names(lapop.2006.HND),names(lapop.2006.SLV),
                           names(lapop.2008.GTM),names(lapop.2008.HND),names(lapop.2008.SLV),
                           names(lapop.2010.GTM),names(lapop.2010.HND),names(lapop.2010.SLV),
                           names(lapop.2012.GTM),names(lapop.2012.HND),names(lapop.2012.SLV),
                           names(lapop.2014.GTM),names(lapop.2014.HND),names(lapop.2014.SLV))
 
 lapop.trends <- rbind(lapop.2004.GTM[trend.all],lapop.2004.HND[trend.all],lapop.2004.SLV[trend.all],
                       lapop.2006.GTM[trend.all],lapop.2006.HND[trend.all],lapop.2006.SLV[trend.all],
                       lapop.2008.GTM[trend.all],lapop.2008.HND[trend.all],lapop.2008.SLV[trend.all],
                       lapop.2010.GTM[trend.all],lapop.2010.HND[trend.all],lapop.2010.SLV[trend.all],
                       lapop.2012.GTM[trend.all],lapop.2012.HND[trend.all],lapop.2012.SLV[trend.all],
                       lapop.2014.GTM[trend.all],lapop.2014.HND[trend.all],lapop.2014.SLV[trend.all])
 # In 2004-2012, missing values are conveyed by 'NA'. In 2014, they used 888888 
 # and 988888; these will do very bad things if you leave them in!
 # Some years also used 8 and 9 as no-response codes; be more careful with those...
 is.na(lapop.trends)[lapop.trends==888888] <- TRUE
 is.na(lapop.trends)[lapop.trends==988888] <- TRUE
 is.na(lapop.trends)[lapop.trends==999999] <- TRUE
 is.na(lapop.trends)[lapop.trends==88] <- TRUE
 
 # clean up
 rm(lapop.2004.GTM,lapop.2004.SLV,lapop.2004.HND,
    lapop.2006.GTM,lapop.2006.SLV,lapop.2006.HND,
    lapop.2008.GTM,lapop.2008.SLV,lapop.2008.HND,
    lapop.2010.GTM,lapop.2010.SLV,lapop.2010.HND,
    lapop.2012.GTM,lapop.2012.SLV,lapop.2012.HND,
    lapop.2014.GTM,lapop.2014.SLV,lapop.2014.HND,
    fnames,trend.all,yrs,intersection)
 
 ## save in raw-data folder
 data_raw_dir()
 write.csv(data, "data-raw/dataLAPOP.csv", row.names = FALSE)
 
 
  }