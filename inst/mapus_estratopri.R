# Generate a couple of Excel spreadsheets with a bunch of fear-related variables.
# This version of the file is designed for choropleth maps to be created with our estratopri shapefile.

#source('inst/make_indices.R')
#source('inst/make_geo.R')
library(plyr)
library(XLConnect)
library(ggmap)

# These variables were found (by stepwise regression) to explain the fear index
# well; we'll want to map them.

map_us <- lapop.2014.all[,c('clien1n','d6','ed2','for6','it1','mil10c','pol1',
                         'pole2n','sd3new2','ur','w14a','www1')]
categ <- lapop.2014.all[,c('a4','aoj22','for4','for1n','for5','ocup4a','q3c','vb20')]
categ[categ>800000] <- NA
map_us$a4_5     <- as.numeric(categ$a4==5)
map_us$aoj22_1  <- as.numeric(categ$aoj22==1)
map_us$for4_4   <- as.numeric(categ$for4==4)
map_us$for1n_7  <- as.numeric(categ$for1n==7)
map_us$for5_6   <- as.numeric(categ$for5==6)
map_us$ocup4a_3 <- as.numeric(categ$ocup4a==3)
map_us$q3c_5    <- as.numeric(categ$q3c==5)
map_us$vb20_4   <- as.numeric(categ$vb20==4)
map_us$aut_idx <- aut_all
map_us$ca_idx <- ca_all
map_us$tr_idx <- tr_all
map_us$fear_idx <- fear_all
map_us[map_us>800000] <- NA

map_us$estratopri <- my_geo$estratopri
out_geo <- ddply(map_us,.(estratopri),numcolwise(function(x) mean(x,na.rm=TRUE)))

write.csv(out_geo,file='data/mapus-estratopri.csv',row.names=FALSE)


l <- 'less'
g <- 'greater'
e <- out_geo$estratopri
sig <- data.frame(estratopri=e,
                  clien1n_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,'clien1n',g)),
                  d6_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"d6",g)),       
                  ed2_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"ed2",g)),       
                  for6_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"for6",l)),     
                  it1_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"it1",g)),      
                  mil10c_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"mil10c",l)),   
                  pol1_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"pol1",l)),     
                  pole2n_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"pole2n",g)),   
                  sd3new2_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"sd3new2",g)),  
                  ur_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"ur",l)),       
                  w14a_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"w14a",l)),     
                  www1_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"www1",l)),     
                  a4_5_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"a4_5",g)),     
                  aoj22_1_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"aoj22_1",g)),  
                  for4_4_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"for4_4",g)),   
                  for1n_7_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"for1n_7",g)),  
                  for5_6_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"for5_6",g)),   
                  ocup4a_3_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"ocup4a_3",g)), 
                  q3c_5_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"q3c_5",g)),    
                  vb20_4_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"vb20_4",g)),   
                  aut_idx_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"aut_idx",g)),  
                  ca_idx_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"ca_idx",g)),   
                  tr_idx_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"tr_idx",l)),   
                  fear_idx_sig=sapply(e, function(x) AmericasBarometer::sig_diff(x,"fear_idx",g)))


write.csv(sig,'data/sig-estratopri.csv',row.names=FALSE)
AmericasBarometer::get_outliers('fear_idx',g,pmax=0.05)

