# comp <- janitor::compare_df_cols(RemoteInterview,
#                                  Colombia_rem,
#                                  Link2_rem,
#                                  Link3_ipsos,
#                                  self_administered,
#                                  FromProgres,
#                                  return = "mismatch")
# 

big_data = do.call(rbind, )
# or big_data <- dplyr::bind_rows(datalist)
# or big_data <- data.table::rbindlist(datalist)

datalist1 <- as.list(datalist)

data <- datalist1 %>% 
  purrr::map_dfr(as_factor, compose( read.csv))

data2 <- datalist1 %>% 
  dplyr::bind_rows(compose(as_factor, read.csv))







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


# country_list <- lapply(file_data$dta_files, get_country_dfs)
country.dfs <- sapply(country.filenames, get_country_df)

# Combining the list of dataframes into one data frame.
country.dfs.tidied <- sapply(country.dfs, function(df){
  df %>%
    add_uniqueID() %>% 
    add_weight1500() %>%
    select(person_id, one_of(target_vars)) %>%  
    mutate_at(vars(contains('idnum')), as.character) %>% 
    mutate_at(vars(contains('pais')), as.numeric) %>% 
    mutate_at(vars(contains('year')), as.numeric) %>% 
    mutate_at(vars(contains('clusterdesc')), as.character) 
})

all.df <- reduce(country.dfs.tidied, bind_rows)


# Adding full country names to data frame.
all.df <- left_join(all.df,
                    (
                      response_labels %>%
                        filter(column_name == "pais") %>%
                        mutate_at(vars(("value")), as.double) %>%
                        select(value, label)
                    ),
                    by = c("pais" = "value")) %>%
  mutate(country = label)

all.df %>% 
  select("person_id","wt","weight1500", everything()) %>% 
  head()
all.df %>% 
  assert(is_uniq, person_id)

