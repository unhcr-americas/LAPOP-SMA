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
#' Get all country specific dataset and do a full merge and get the 
#' full Grand Merge data set.
#' Create a data frame called lapop.trends containing all indicators 
#' that are consistent across countries and years.
#'
#' @md
#' @param src list of direct URL to dataset - dataset name to be normalised
#'  with 3 letters country code plus yer 
#'  like GTM2019
#' @examples
#' \dontrun{
#' }
#' @export

get_dataLAPOP <- function( ){
  
  # http://datasets.americasbarometer.org/database/files/2004-2018%20LAPOP%20AmericasBaometer%20Merge_FREE_V1.0_W.zip
  #data <- read_dta("data-raw/down/2004-2018 LAPOP AmericasBarometer Merge (v1.0FREE).dta")
 #require(tidyverse)
 #require(haven)
  # library(foreign) # needed to import STATA files
  # library(plyr)
  # library(ggplot2)
  # library(stringr)
  # library(reshape2)
  # library(RColorBrewer)
  # library(stringi)  
  
 srcs <- list(
        lapop.2004.GTM ="http://datasets.americasbarometer.org/database/files/1111117573guatemala%202004%20export%20version.dta",
        lapop.2006.GTM ="http://datasets.americasbarometer.org/database/files/784250406guatemala_%20lapop_final%202006%20data%20set%20092906.dta",
        lapop.2008.GTM ="http://datasets.americasbarometer.org/database/files/130872853guatemala_lapop_dims_2008_final_data_set_v10.dta",
        lapop.2010.GTM ="http://datasets.americasbarometer.org/database/files/305797627Guatemala_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20V3.dta",
        lapop.2012.GTM ="http://datasets.americasbarometer.org/database/files/2041873797Guatemala%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta",
        lapop.2014.GTM ="http://datasets.americasbarometer.org/database/files/1519863689Guatemala%20LAPOP%20AmericasBarometer%202014%20v3.0_W.dta",
        lapop.2016.GTM ="http://datasets.americasbarometer.org/database/files/823416394Guatemala%20LAPOP%20AmericasBarometer%202017%20V1.0_W.dta",
        lapop.2019.GTM ="http://datasets.americasbarometer.org/database/files/Guatemala%20LAPOP%20AmericasBarometer%202019%20v1.0_W.dta",
       # test <- foreign::read.dta("http://datasets.americasbarometer.org/database/files/Guatemala%20LAPOP%20AmericasBarometer%202019%20v1.0_W.dta")
       # test <- haven::read_dta("http://datasets.americasbarometer.org/database/files/Guatemala%20LAPOP%20AmericasBarometer%202019%20v1.0_W.dta")
       
        lapop.2004.HND ="http://datasets.americasbarometer.org/database/files/1997861390honduras%202004%20export%20version.dta",
        lapop.2006.HND ="http://datasets.americasbarometer.org/database/files/1631591650honduras_lapop_final%202006%20data%20set%20092906.dta",
        lapop.2008.HND ="http://datasets.americasbarometer.org/database/files/1645121539honduras_lapop_dims_2008_final_data_set_v10.dta",
        lapop.2010.HND ="http://datasets.americasbarometer.org/database/files/1418722138Honduras_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20v3.dta",
        lapop.2012.HND ="http://datasets.americasbarometer.org/database/files/42768395Honduras%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta",
        lapop.2014.HND ="http://datasets.americasbarometer.org/database/files/1492610684Honduras%20LAPOP%20AmericasBarometer%202014%20v3.0_W.dta",
        lapop.2016.HND ="http://datasets.americasbarometer.org/database/files/1252173714Honduras%20LAPOP%20AmericasBarometer%202016%20V1.0_W.dta",
        lapop.2018.HND = "http://datasets.americasbarometer.org/database/files/Honduras%20LAPOP%20AmericasBarometer%202018%20v1.0_W.dta",
        
        
        lapop.2004.SLV ="http://datasets.americasbarometer.org/database/files/1762554388el%20salvador%202004%20export%20version.dta",
        lapop.2006.SLV ="http://datasets.americasbarometer.org/database/files/223466081el%20salvador_lapop_final%202006%20data%20set%20092906.dta",
        lapop.2008.SLV ="http://datasets.americasbarometer.org/database/files/1698154699el_salvador_lapop_dims_2008_final_lapop08_v10.dta",
        lapop.2010.SLV ="http://datasets.americasbarometer.org/database/files/316969358El%20Salvador_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20v3.dta",
        lapop.2012.SLV ="http://datasets.americasbarometer.org/database/files/1126250629ElSalvador%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta",
        lapop.2014.SLV ="http://datasets.americasbarometer.org/database/files/1234285428ElSalvador%20LAPOP%20AmericasBarometer%202014%20v3.0_W.dta",
        lapop.2016.SLV ="http://datasets.americasbarometer.org/database/files/381267663ElSalvador%20LAPOP%20AmericasBarometer%202016%20V1.0_W.dta",
        lapop.2018.SLV = "http://datasets.americasbarometer.org/database/files/ElSalvador%20LAPOP%20AmericasBarometer%202018%20v1.0_W.dta"
        )
 
 

  scrcframe <- as.data.frame(t(as.data.frame(srcs)))
  names(scrcframe)[1] <- "url"
  scrcframe$year <- as.integer(substr(row.names(scrcframe),7,10 ))
  scrcframe$ctry <- as.character(substr(row.names(scrcframe),12,14 ))

  ## save in raw-data folder
  if(!dir.exists("data-raw")) dir.create("data-raw") 
  
for (i in 18:nrow(scrcframe))  {
    # i <- 2
    url1 <- scrcframe[i,c("url")]
    year1  <- scrcframe[i,c("year")]
    ctrycollect1 <- scrcframe[i,c("ctry")]  
    
    ## just get some error handling...
    possibleError <- tryCatch(
      haven::read_dta(url1),
      error=function(e) e
    )
    ## In case of error push it next
    if(inherits(possibleError, "error")) next
    
    #REAL WORK
        lapop <- haven::read_dta(url1);
        names(lapop) <- tolower(names(lapop));
        lapop$year  <- year1;
        lapop$ctrycollect <- ctrycollect1;
        
        cat(paste0("Reading data for ", ctrycollect1, " in ", year1, " : ", nrow(lapop), " observations  & ", ncol(lapop), " variables.\n"))
    
        # In 2004-2012, missing values are conveyed by 'NA'. In 2014, they used 888888 
        # and 988888; these will do very bad things if you leave them in!
        # Some years also used 8 and 9 as no-response codes; be more careful with those...
        is.na(lapop)[lapop==888888] <- TRUE;
        is.na(lapop)[lapop==988888] <- TRUE;
        is.na(lapop)[lapop==999999] <- TRUE;
        is.na(lapop)[lapop==88] <- TRUE;
        
        # The idnum field wasn't used in 2004 (or in 2006 in SLV); this can be handy 
        # if I need to join the complete dataset with individual ones again.
        set.seed(12345);
        if(year1 == "2004" | (year1 == "2006" & ctrycollect1 == "SLV" )) {
        lapop$idnum <- stringi::stri_rand_strings(nrow(lapop),60)
        #lapop$idnum <- row.names(lapop)
        };
        lapop$idnum <- as.character(lapop$idnum);
        ## Re-encode the data 
      
        ## get dico and save it
        #write.csv(make_codebook(lapop), paste0(getwd(),"/data-raw/",row.names(scrcframe)[i],"-dico.csv"), row.names = FALSE) ;
        ## Save frame
        write.csv(lapop, paste0(getwd(),"/data-raw/",row.names(scrcframe)[i],".csv"), row.names = FALSE) ;
        
        #dummy while loop for a  10 sec sleep
        date_time <-Sys.time()
        while((as.numeric(Sys.time()) - as.numeric(date_time))< 10){}
    
  } ## End loop
  
 ## grand merge
  #data <- srcs %>% map_dfr(compose(as_factor, read_dta))
  
  ## Get a consolidated data dictionnary
  #dico <- srcs %>% map_dfr(compose(make_codebook(read_dta)))  
  
 datalist <-  list.files(paste0(getwd(),"/data-raw"),full.names=T)
 lapop.trend <- read.csv(datalist[1])
 lapop.trend$ti <- as.character(lapop.trend$ti)
 lapop.trend$fecha <- " "
 lapop.trend$annotations <- " "
 lapop.trend.inter <- lapop.trend
 
 # I <3 recursive functions
 intersection <- function(x, y, ...){
   if (missing(...)) intersect(x, y)
   else intersect(x, intersection(y, ...))
 }
 
 for (f in 2:length(datalist)) {
   cat(paste0("reading :",f, datalist[f], "\n"))
    # f <- 3
    lapop.trend1 <- read.csv(datalist[f])
    lapop.trend1$idnum <- as.character(lapop.trend1$idnum)
    
     if ( !(is.null(lapop.trend1$ti) ) ) {
     lapop.trend1$ti <- as.character(lapop.trend1$ti)}
    
    
    if ( !(is.null(lapop.trend1$clusterdesc) ) ) {
      lapop.trend1$clusterdesc <- as.character(lapop.trend1$clusterdesc)}
    
    if ( !(is.null(lapop.trend1$fecha) ) ) {
      lapop.trend1$fecha <- as.character(lapop.trend1$fecha)}
    
    if ( !(is.null(lapop.trend1$annotations) ) ) {
      lapop.trend1$annotations <- as.character(lapop.trend1$annotations)}
    
    ## All merge
    lapop.trend <- dplyr::bind_rows(lapop.trend, lapop.trend1)
    
    ## Only intersection
    commonvar <- intersection(names(lapop.trend.inter), names(lapop.trend1))
    lapop.trend.inter <- rbind(lapop.trend.inter[,  commonvar],
                               lapop.trend1[ ,  commonvar])
    
  }
   
 names.lapop.trend  <- as.data.frame(names(lapop.trend))
 names(names.lapop.trend)[1] <- "column_name"
 names.lapop.trend.inter <- as.data.frame(names(lapop.trend.inter))
 names(names.lapop.trend.inter)[1] <- "column_name"
 
 questions <- read.csv("data/questions_categories_v1-2.csv")
 vmodalities <- read.csv("data/values_labels_v1-2.csv")
 
 names.lapop.trend  <- merge(x = names.lapop.trend,
                             y = questions, all.x = TRUE)
 names.lapop.trend.inter  <- merge(x = names.lapop.trend.inter,
                             y = questions, all.x = TRUE)
 

 ## save in raw-data folder
 if(!dir.exists("data")) dir.create("data") 
 write.csv(lapop.trend.inter, "data/dataLAPOPinter.csv", row.names = FALSE)
 write.csv(lapop.trend, "data/dataLAPOP.csv", row.names = FALSE)
 

 
}
