#' sig_diff
#' 
#' flag the entries that are significantly above or below average
#' @md
#' 
#' @param estr  frame to look at
#' @param var variable to look at
#' @param direction should be 'less', 'greater', or 'two.sided'
#' 
#' @examples
#' \dontrun{
#' l <- 'less'
#' g <- 'greater'
#' e <- out_geo$estratopri
#' sig <- data.frame(estratopri=e,
#'                   clien1n_sig=sapply(e, function(x) sig_diff(x,'clien1n',g)),
#'                   d6_sig=sapply(e, function(x) sig_diff(x,"d6",g)),       
#'                   ed2_sig=sapply(e, function(x) sig_diff(x,"ed2",g)),       
#'                   for6_sig=sapply(e, function(x) sig_diff(x,"for6",l)),     
#'                   it1_sig=sapply(e, function(x) sig_diff(x,"it1",g)),      
#'                   mil10c_sig=sapply(e, function(x) sig_diff(x,"mil10c",l)),   
#'                   pol1_sig=sapply(e, function(x) sig_diff(x,"pol1",l)),     
#'                   pole2n_sig=sapply(e, function(x) sig_diff(x,"pole2n",g)),   
#'                   sd3new2_sig=sapply(e, function(x) sig_diff(x,"sd3new2",g)),  
#'                   ur_sig=sapply(e, function(x) sig_diff(x,"ur",l)),       
#'                   w14a_sig=sapply(e, function(x) sig_diff(x,"w14a",l)),     
#'                   www1_sig=sapply(e, function(x) sig_diff(x,"www1",l)),     
#'                   a4_5_sig=sapply(e, function(x) sig_diff(x,"a4_5",g)),     
#'                   aoj22_1_sig=sapply(e, function(x) sig_diff(x,"aoj22_1",g)),  
#'                   for4_4_sig=sapply(e, function(x) sig_diff(x,"for4_4",g)),   
#'                   for1n_7_sig=sapply(e, function(x) sig_diff(x,"for1n_7",g)),  
#'                   for5_6_sig=sapply(e, function(x) sig_diff(x,"for5_6",g)),   
#'                   ocup4a_3_sig=sapply(e, function(x) sig_diff(x,"ocup4a_3",g)), 
#'                   q3c_5_sig=sapply(e, function(x) sig_diff(x,"q3c_5",g)),    
#'                   vb20_4_sig=sapply(e, function(x) sig_diff(x,"vb20_4",g)),   
#'                   aut_idx_sig=sapply(e, function(x) sig_diff(x,"aut_idx",g)),  
#'                   ca_idx_sig=sapply(e, function(x) sig_diff(x,"ca_idx",g)),   
#'                   tr_idx_sig=sapply(e, function(x) sig_diff(x,"tr_idx",l)),   
#'                   fear_idx_sig=sapply(e, function(x) sig_diff(x,"fear_idx",g)))
# 
#' #write.csv(sig,'sig-estratopri.csv',row.names=FALSE)
#' }
#' @export
sig_diff <- function(estr, var, direction) {
  # direction should be 'less', 'greater', or 'two.sided'
  tt <- t.test(map_us[map_us$estratopri==estr,var],
               map_us[map_us$estratopri!=estr,var],
               alternative=direction)
  n <- length(unique(map_us$estratopri))
  as.numeric(tt$p.value < 0.01/n)
}



#' get_outliers
#' 
#' For the above-average areas, which individual cities are above average?
#' This often will not return anything.
#' 
#' @param var variable to look at
#' @param direction should be 'less', 'greater', or 'two.sided'
#' @param pmax  max value
#' @md
#' @examples
#' \dontrun{
#' get_outliers('fear_idx',g,pmax=0.05)
#' }
#' @export
get_outliers <- function(var, direction, pmax=0.01) {
  s <- paste(var,'_sig',sep='')
  hi_estr <- sig[sig[,s]==1,'estratopri']
  hi_muni <- unique(my_geo[my_geo$estratopri %in% hi_estr,'muni_text'])
  sig_muni <- sapply(hi_muni, function(x) {
    tt <- t.test(map_us[map_us$estratopri %in% hi_estr & my_geo$muni_text==x,var],
                 map_us[map_us$estratopri %in% hi_estr & my_geo$muni_text!=x,var],
                 alternative=direction)
    tt$p.value < pmax/length(hi_muni)
  })
  muni_df <- ldply(hi_muni[sig_muni],function(x) {
    m <- mean(map_us[map_us$estratopri %in% hi_estr & my_geo$muni_text==x,var])
    hi <- max(map_us[map_us$estratopri %in% hi_estr & my_geo$muni_text==x,var])
    lo <- min(map_us[map_us$estratopri %in% hi_estr & my_geo$muni_text==x,var])
    lat <- mean(my_geo[my_geo$estratopri %in% hi_estr & my_geo$muni_text==x,'lat'])
    long <- mean(my_geo[my_geo$estratopri %in% hi_estr & my_geo$muni_text==x,'long'])
    data.frame(name=x,mean=m,max=hi,min=lo,lat=lat,long=long)
  })
  if (nrow(muni_df) > 0) {
    fname <- paste(var,'_outliers.csv',sep='')
    write.csv(muni_df,fname,row.names=FALSE)
  }
  muni_df
}

#' sanity_check
#' 
#' Return the average values of 'var' in 'data' for the lowest and highest quartiles of idx
#' @md
#' @param data Given dataset containing variables 
#' @param var  variable used construct a composite index from the first principal  
#' @param idx Consutructed Index
#' 
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
sanity_check <- function(data,idx,var) {
  # Return the average values of 'var' in 'data' for the lowest and highest
  # 
  v <- data[,var]
  is.na(v[v>800000]) <- TRUE
  lo <- mean(v[idx <= quantile(idx)[2]],na.rm=TRUE)
  hi <- mean(v[idx >= quantile(idx)[4]],na.rm=TRUE)
  c(lo,hi)
}


# This function checks if pais column exists in the dataframe
check_pais <- function(df, filename){
  # inputs : 
  # df - dataframe to be used
  # filename - the name of the file being imported
  # output : dataframe with correct country as a column
  if(! "pais" %in% colnames(df)){
    print("'pais' missing!")
  }
  return(df)
}



# This function checks if year column exists in the dataframe, and if it does then it makes sure that the year matches the year in filename.  If it doesn't then it adds the year from the name of file. 
add_year <- function(df, filename){
  # inputs : 
  # df - dataframe to be used
  # filename - the name of the file being imported
  # output : dataframe with correct year as a column
  if(!("year" %in% names(df))){
    year <- str_extract(filename, "\\d{4}")
    df$year <- year
    cat(paste0("\nSet year to ",year))
  }
  return(df)
}


# Function to generate unique id's that contain year, country and observation info
add_uniqueID <- function(df) {
  df <- df %>%
    mutate(person_id = paste(year, pais, idnum,sep = "_")) %>%
    group_by(person_id) %>% 
    mutate(id_count = row_number()) %>% 
    ungroup() %>%
    mutate(person_id = paste(person_id, id_count, sep = "_"),
           id_count = NULL)
  #Adding more stuff to id because some files have non-unique ids
  assert(df, is_uniq, person_id)
  return(df)
}

#function to generate/get weights OLD}
add_weight1500 <- function(country_df) {
  #creates final output variable
  if (!"weight1500"%in%names(country_df)){
    country_df <- mutate(country_df, weight1500=wt/n()*1500)
  }
  return(country_df)
}



#' fixdata
#' @md 
#'  1. vic1exta asks about the frequency of crime victimization and is NR if 
#' 2. respondant was never victimized; change this to zero  
#' 3. vicbar7f asks about the number of occurences of murders (SLV only)
#' 4. vicbar7f asks about the number of occurences of murders (SLV only)
#' 5. set uniq_id to 0 (so it doesn't become NA)
#' 6. somehow ages got imported as strings
#' 7. set non-responses to NA
#' @md
#' @param x Given dataset containing variables
#' 
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
fixdata <- function(x) {
  y <- x
  # vic1exta asks about the frequency of crime victimization and is NR if 
  # respondant was never victimized; change this to zero  
  y$vic1exta[y$vic1exta == 999999] <- 0
  # vicbar1f is similar; asks about the number of occurrences of burglaries
  y$vicbar1f[y$vicbar1f == 999999] <-  0
  # vicbar7f asks about the number of occurences of murders (SLV only)
  if ('vicbar7f' %in% names(y)) {
    y$vicbar7f[y$vicbar7f == 999999] <- 0
  }
  # set uniq_id to 0 (so it doesn't become NA)
  y$uniq_id <- 0
  # somehow ages got imported as strings
  y$q2 <- as.numeric(y$q2)
  # set non-responses to NA
  y[y > 800000] <- NA
  y
}

#' make_idx
#' 
#' Functions we need to construct indices
#' @md
#' @param data Given dataset containing variables
#' 
#' @param vars variable used construct a composite index from the first principal 
#'  component after multiple imputation. 
#'  
#' @param sgn Direction -  Set sgn=-1 to reverse the direction of the index.
#' 
#' @param scale Scale - Set scale=TRUE to use scaling during the PCA calculation
#' @param seed for reproducibility
#' 
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
make_idx <- function(data,
                     vars,
                     sgn=1,
                     scale=FALSE,
                     seed=12345) {

  my_data <- data[,vars]
  is.na(my_data[my_data>800000]) <- TRUE
  my_data <- my_data[,order(names(my_data))] # alphabetize columns
  my_imp <- mice(my_data,printFlag=F,seed=seed)
  my_pr <- lapply(1:5,function(x) 
    prcomp(complete(my_imp,x),scale=scale,center=TRUE))
  all_pc1 <- data.frame(llply(1:5, function(i) my_pr[[i]]$x[,1]))
  for (i in 2:ncol(all_pc1)) {  
    if (cor(all_pc1[,1],all_pc1[,i]) < 0) {
      all_pc1[,i] <- -1 * all_pc1[,i]
    }
  }
  avg <- rowMeans(all_pc1)
  scale(sgn*avg)
}
 


#' binom.low
#' 
#' use these functions to make error bars with binomial test
#' @md
#' @param n total observations in that year
#' @param x  number of 1's observed that year 
#' 
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
# 
binom.low <- function(x,n) { binom.test(x,n,alternative='two.sided')$conf.int[1] }

#' binom.high
#' 
#' use these functions to make error bars with binomial test
#' @md 
#' @param n total observations in that year
#' @param x  number of 1's observed that year 
#' 
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
binom.high <- function(x,n) { binom.test(x,n,alternative='two.sided')$conf.int[2] }

#' binom_plot
#' use this to make plots of a binomial variable (one that takes only 0/1 values)
#' @md
#' @param  f  a data frame that must contain the following columns:
#'   year = year of observations 
#'    x = number of 1's observed that year 
#'    n = total observations in that year
#' @param label
#' @param label_y
#' @examples
#' \dontrun{
#'  
#' }
#' @export
binom_plot <- function(f, 
                       label='', 
                       label_y=0) {
  f$mean <- f$x / f$n
  if (label_y == 0 ) {
    label_y <- 1.1*max(f$mean)
  }
  f$ymin <- mapply(binom.low,f$x,f$n)
  f$ymax <- mapply(binom.high,f$x,f$n)
  ggplot(data=f,aes(x=year,y=mean)) +
    geom_line(size=1.5,color='red') +
    geom_point(size=5) +
    geom_errorbar(aes(ymin=ymin,
                      ymax=ymax,
                      width=0.5)) +
    scale_x_continuous(breaks=seq(2004,2018,2)) +
    annotate('text',
             label=str_wrap(label,width=32),
             size=8,
             x=2003,
             y=label_y,
             hjust=0,
             vjust=0) +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.y=element_blank(),
          axis.title.x=element_blank()) 
}

#' bar_plot
#' 
#' ggplot2 bar plot: plot distribution of x (no time resolution here)
#' use this to make bar plots of a categorical indicator 
#' with >2 categories
#' @md
#' @param  f data frame f needs to contain a year column all other columns will 
#' become the names of data series that will be plotted together
#' 
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
bar_plot <- function(f) {
  ggplot(f,aes(factor(x))) + 
    geom_bar() +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.y=element_blank(),
          axis.title.x=element_blank()) 
}

#' multi_lines
#' 
#' ggplot2 multi lines
#' @md
#' @param  f data frame f needs to contain a year column all other columns will 
#' become the names of data series that will be plotted together
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
multi_lines <- function(f) {

  m <- reshape2::melt(f,id.vars='year')
  n <- length(unique(m$variable))
  col <- brewer.pal(n,"Dark2")  
  ggplot(m,aes(x=year,y=value,group=variable,color=variable)) + 
    geom_line(size=1.5) +
    scale_color_manual(values=col) +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.y=element_blank()) +
    xlab("Year") 
}


#' cor_data_bin
#' 
#' cor_data_bin
#' @md
#' @param  f data frame f needs to contain a year column all other columns will 
#' become the names of data series that will be plotted together
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
cor_data_bin <- function(d,my_var,bin_vars,ord_vars,idx_vars,unord_vars) {
  cor_bin <- ldply(bin_vars, function(x) bin_bin(d,x,my_var)) 
  cor_ord <- ldply(ord_vars, function(x) ord_bin(d,x,my_var)) 
  cor_idx <- ldply(idx_vars, function(x) ord_bin(d,x,my_var)) 
  cor_unord <- ldply(unord_vars, function(x) unord_bin(d,x,my_var))
  cor_all <- rbind(cor_bin,cor_ord,cor_unord,cor_idx)
  cor_all <- cor_all[order(cor_all$pval),]
  unord_list <- ldply(cor_unord$var1, function(x)
    data.frame(var=strsplit(x,'_')[[1]][1],
               val=as.numeric(strsplit(x,'_')[[1]][2]),
               str=x,
               stringsAsFactors=FALSE))
  for (i in 1:nrow(unord_list)) {
    d[,unord_list$str[i]] <- as.numeric(d[,unord_list$var[i]] == unord_list$val[i])
  }
  res <- d[,cor_all$var1]
  res[,my_var] <- d[,my_var]
  res
}


get_vars <- function(x,var) {
  # gets variables that appear consistently in stepwise regression
  # x should be an object returned by MICE
  lm1 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,1)))
  lm2 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,2)))
  lm3 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,3)))
  lm4 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,4)))
  lm5 <- lm(paste(var,' ~ .'),data=na.omit(complete(x,5)))
  step1 <- stepAIC(lm1,trace=F) 
  step2 <- stepAIC(lm2,trace=F) 
  step3 <- stepAIC(lm3,trace=F) 
  step4 <- stepAIC(lm4,trace=F)
  step5 <- stepAIC(lm5,trace=F) 
  counts <- as.data.frame(table(c(names(coef(step1)),names(coef(step2)),
                                  names(coef(step3)),names(coef(step4)),
                                  names(coef(step5)))),stringsAsFactors=F)
  counts[counts$Freq==5,'Var1']
}



# Assume a function is binary if only two responses are present
is_binary <- function(data,var) {
  u <- unique(data[,var])
  length(u[u<888888]) == 2
}



ord_bin <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between a variable var1 and a binary variable 
  # var2. This uses a t-test conditioned on the values of var2, and will
  # be applicable if var1 is continuous (i.e. one of our composite indices).
  res = data.frame(var1=character(),var2=character(),est=numeric(),
                   pval=numeric(),stringsAsFactors=FALSE)
  if (var1 != var2) {
    if (cutoff == 0) {
      cutoff <- 1 / ncol(data)
    }
    tmp <- data.frame(v1=data[,var1],v2=data[,var2])
    is.na(tmp[tmp>800000]) <- TRUE
    tmp$v2 <- tmp$v2 - min(tmp$v2,na.rm=TRUE) # convert to 0-1 scale
    #if (sum(tmp$v2==0,na.rm=TRUE) > 1 & sum(tmp$v2==1,na.rm=TRUE) > 1) {
    if (sum(!is.na(tmp[tmp$v2==0,'v1'])) > 1 & 
        sum(!is.na(tmp[tmp$v2==1,'v1'])) > 1) {
      tt <- t.test(tmp$v1[tmp$v2==0],tmp$v1[tmp$v2==1])
      if (tt$p.value < cutoff) {
        res=data.frame(var1=var1,var2=var2,est=tt$estimate[2]-tt$estimate[1],
                       pval=tt$p.value,stringsAsFactors=FALSE)
      }
    }
  }
  res
}

ord_ord <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between two ordered variables, var1 and var2. 
  # This uses linear regression, and will be applicable when we're dealing
  # with ordered categorical variables or composite indices.
  # As a matter of convention, use the composite index as var1 so that the
  # output makes sense.
  res = data.frame(var1=character(),var2=character(),est=numeric(),
                   pval=numeric(),stringsAsFactors=FALSE)
  if (var1 != var2) {
    if (cutoff == 0) {
      cutoff <- 1 / ncol(data)
    }
    tmp <- data.frame(v1=data[,var1],v2=data[,var2])
    is.na(tmp[tmp>800000]) <- TRUE
    reg <- lm(v1 ~ v2,data=tmp)
    p <- summary(reg)$coefficients[2,4]
    if (p < cutoff) {
      res=data.frame(var1=var1,var2=var2,est=summary(reg)$coefficients[2,1],
                     pval=p,stringsAsFactors=FALSE)
    }
  }
  res
}

ord_unord <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between an ordered variable var1 and an unordered
  # categorical variable var2. Do this by creating a binary variable for each
  # possible value of var2 and calling ord_bin().
  vals <- na.omit(unique(data[data[,var2] < 800000,var2]))
  if (cutoff == 0) {
    cutoff <- 1 / ncol(data)
  }
  tmp <- data.frame(v1=data[,var1])
  is.na(tmp[tmp>800000]) <- TRUE
  for (x in vals) {
    tmp[,paste(var2,x,sep='_')] <- as.numeric(data[,var2] == x)
  }
  ldply(names(tmp),function(x) ord_bin(tmp,'v1',x,cutoff))
}

bin_bin <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between two binary variables, var1 and var2.
  # Uses a Fisher's exact test. This test reports an odds ratio rather than
  # the coefficient reported by other tests; return the log-odds ratio so 
  # that they're somewhat more comparable.
  res = data.frame(var1=character(),var2=character(),est=numeric(),pval=numeric(),
                   stringsAsFactors=FALSE)
  if (var1 != var2) {
    if (cutoff == 0) {
      cutoff <- 1 / ncol(data)
    }
    tmp <- data.frame(v1=data[,var1],v2=data[,var2])
    is.na(tmp[tmp>800000]) <- TRUE
    tmp$v1 <- tmp$v1 - min(tmp$v1,na.rm=TRUE) # convert to 0-1 scale
    tmp$v2 <- tmp$v2 - min(tmp$v2,na.rm=TRUE) # convert to 0-1 scale
    if (sum(tmp$v1==0,na.rm=TRUE) > 1 & sum(tmp$v1==1,na.rm=TRUE) > 1 &
        sum(tmp$v2==0,na.rm=TRUE) > 1 & sum(tmp$v2==1,na.rm=TRUE)) {
      t <- table(tmp$v1,tmp$v2)  
      # rows of t are v1, columns are v2
      ft <- fisher.test(t) 
      if (ft$p.value < cutoff) {
        res=data.frame(var1=var1,var2=var2,est=log10(ft$estimate),
                       pval=ft$p.value,stringsAsFactors=FALSE)
      }
    }
  }
  res
}

unord_bin <- function(data,var1,var2,cutoff=0) {
  # Determine the correlation between a binary variable var2 and an unordered
  # categorical variable var1. Do this by creating a binary variable for each
  # possible value of var1 and calling bin_bin().
  vals <- na.omit(unique(data[data[,var1] < 800000,var1]))
  if (cutoff == 0) {
    cutoff <- 1 / ncol(data)
  }
  tmp <- data.frame(v2=data[,var2])
  names(tmp) <- var2
  is.na(tmp[tmp>800000]) <- TRUE
  for (x in vals) {
    tmp[,paste(var1,x,sep='_')] <- as.numeric(data[,var1] == x)
  }
  ldply(names(tmp),function(x) bin_bin(tmp,x,var2,cutoff))
}



#' lm_plot
#' 
#' ggplot2 object with results of a regression
#' @md
#' @param x must be the output from a multiple regression on MICE output
#' @param orig must be the original, unimputed datasetas
#' @param p_max is the maximum p-value that will be included in the viz
#' @param text is a 2-column data frame containing variable names and 
#'      the text with which they should be replaced
#' @param offsets is a 3-column data frame (var,x,y) detailing how 
#'     labels should be offset for readability.
#' @param flip is a list of variables whose directionality should be 
#       flipped for better comprehension                      
#' @param width is the width (in characters) to be used for wrapping of long labels
#' @examples
#' \dontrun{
#' 
#' }
#' @export
lm_plot <- function(x,orig,title,p_max=0.01,text=NULL,offsets=NULL,flip=NULL,
                    width=15) {
  
  s <- summary(pool(x))
  plotme <- data.frame(s[-1,c(1,5)])
  plotme$log_p <- -log10(plotme[,2])
  m <- max(plotme$log_p[plotme$log_p < Inf])
  plotme$log_p[plotme$log_p > m] <- 1.1*m
  plotme$scale <- sapply(rownames(plotme),
                         function(x) max(orig[,x],na.rm=TRUE) - min(orig[,x],na.rm=TRUE))
  plotme$scale[plotme$scale %% 1 > 0] <- 1 # don't rescale composite indices
  plotme$est <- plotme$est * plotme$scale
  plotme$label <- rownames(plotme)
  # get rid of non-significant points
  plotme <- plotme[plotme$log_p > -log10(p_max),]
  if (nrow(plotme) == 0) {
    print('ERROR: nothing to plot.')
    print('Try using a higher value for p_max!')
  }
  # flip as needed
  if (!is.null(flip)) {
    plotme[plotme$label %in% flip,'est'] <- -1*plotme[plotme$label %in% flip,'est']
  }
  # move labels if needed
  plotme$label_x <- plotme$log_p
  plotme$label_y <- plotme$est
  if (!is.null(offsets)) {
    j <- join(plotme,offsets,by='label')
    plotme[!is.na(j$x),'label_x'] <- plotme[!is.na(j$x),'label_x'] + 
      j[!is.na(j$x),'x']
    plotme[!is.na(j$y),'label_y'] <- plotme[!is.na(j$y),'label_y'] + 
      j[!is.na(j$y),'y']
  }
  # replace labels
  if (!is.null(text)) {
    j <- join(plotme,text,by='label')
    plotme[!is.na(j$long),'label'] <- str_wrap(j[!is.na(j$long),'long'],width)
  }
  # now build the plot
  x1 = min(plotme$log_p)
  x2 = max(plotme$log_p)
  y1 = min(plotme$est)
  y2 = max(plotme$est)
  p <- ggplot(data=plotme,aes(x=log_p,y=est,color=est,label=label)) +
    #geom_point(size=20,alpha=0.2) +
    geom_point(alpha=0.2,aes(size=log_p)) +
    scale_size_continuous(range=c(15,50)) +
    geom_text(color='black',lineheight=0.8,aes(x=label_x,y=label_y)) +
    scale_color_gradientn(colours=c('cornflowerblue','seashell3',
                                    'firebrick1')) +
    geom_hline(yintercept=0,color='seashell3') +
    scale_x_continuous(breaks=c(x1+0.1*(x2-x1),x1+0.9*(x2-x1)),
                       labels=c('Lower confidence','Higher confidence'),
                       limits=c(x1,1.05*x2)) +
    scale_y_continuous(breaks=c(y1+0.1*(y2-y1),y1+0.9*(y2-y1)),
                       labels=c(str_wrap('Less',10),
                                str_wrap('More',10)),
                       limits=c(1.1*y1,1.1*y2)) +
    theme_classic() +
    theme(legend.position='none',
          text=element_text(size=20),
          axis.ticks = element_blank(), 
          axis.title = element_blank()) +
    ggtitle(title)
  if (p_max > 0.01 & max(plotme[,2]) > 0.01) {
    p <- p + geom_segment(x=-log10(0.01),xend=-log10(0.01),
                          y=1.2*min(plotme$est),yend=0.95*max(plotme$est),color='red') +
      annotate("text",x=-log10(0.01)+0.2,y=max(plotme$est),label='p=0.01',
               color='red') 
  }
  if (p_max > 0.05 & max(plotme[,2]) > 0.05) {
    p <- p + geom_segment(x=-log10(0.05),xend=-log10(0.05),
                          y=1.2*min(plotme$est),yend=0.95*max(plotme$est),color='blue') +
      annotate("text",x=-log10(0.05)-0.2,y=max(plotme$est),label='p=0.05',
               color='blue') 
  }
  print(p)
  plotme # return this so I can re-use it below
}


geo_variance <- function(data,x,make_plot=FALSE,xlab='Value') {
  tmp <- my_geo
  tmp$muni_uniq <- paste(tmp$pais_text,tmp$muni_text,sep='_')  
  tmp$x <- x
  tmp$x[tmp$x > 800000] <- NA
  muni_avg <- ddply(tmp,~muni_uniq,summarize,x=mean(x,na.rm=TRUE))  
  tmp$muni_avg <- muni_avg$x[match(tmp$muni_uniq,muni_avg$muni_uniq)] 
  my_lm <- lm(data=tmp,x ~ muni_avg)
  if (make_plot==TRUE) {
    tmp <- na.omit(tmp)
    p <- ggplot(tmp,aes(x=x,y=muni_avg)) +
      geom_point(color='tomato',alpha=0.2,size=5) +
      geom_smooth(method='lm',size=2,color='royalblue') +
      annotate('text',label=paste("R-sq:",round(summary(my_lm)$r.squared,2)), 
               size=10,x=min(tmp$x) + 0.1*(max(tmp$x)-min(tmp$x)),
               y=0.9*max(tmp$muni_avg),hjust=0,vjust=0,color='royalblue') +
      theme_classic() +
      ylab('Muni-level average') +
      xlab(xlab) +
      theme(text=element_text(size=20))
    print(p)
  }
  summary(my_lm)$r.squared
}

