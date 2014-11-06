# load packages


require(plyr)
require(reshape2)
require(stringr)
require(ggplot2)
require(gdata)
require(foreign)


##########################################################################
# Link datazones to postcodes

# Suggestion by Christina

# Dropbox link to file:
# https://www.dropbox.com/s/373otkkuo1fdpht/latestpcinfowithlinkpc.sav?dl=0
areal_unit_local_location <- "G:/dropbox/Dropbox/Data/Links_between_Areal_Units/latestpcinfowithlinkpc.sav"
areal_unit_link <- "https://www.dropbox.com/s/373otkkuo1fdpht/latestpcinfowithlinkpc.sav?dl=0"

tmp <- read.spss(areal_unit_local_location, to.data.frame=T)

datazone_to_postcode <- tmp[,c("PostcodeFull", "PCSector", "PCDistrict","Datazone")]
names(datazone_to_postcode) <- tolower(names(datazone_to_postcode))
write.csv(datazone_to_postcode, file="output_data/postcode_links/datazone_to_postcode.csv")
########################################################################
# add sns_dir locations

# home dir
sns_dir <- "G:/dropbox/Dropbox/Data/SNS_FullData_CSV_14_3_2013"

# office dir
#sns_dir <- "E:/Dropbox/Data/SNS_FullData_CSV_14_3_2013"


##########################################################################


# Want to know how many files have ZN in their title
length(list.files(path=sns_dir))

# length is 2498 files

# Now how many have _ZN_ in their title?

length(list.files(
  path=sns_dir,
  pattern="_ZN_"
  )
)

relevant_filenames <- list.files(
  path=sns_dir,
  pattern="_ZN_"
  )
# length is 1096

# structure is 
# [GROUP]_[GROUP CODE] _ [table name]_[Year OR {YEAR}Q{QUARTER}]_[OTHERSTUFF]

file_df <- ldply(str_split(relevant_filenames, "_", 5))
names(file_df) <- c(
  "group_name",
  "group_code",
  "table_name",
  "table_period",
  "excess"
  )
file_df$full_filename <- relevant_filenames

file_df$period_structure <- "misc"

file_df$period_structure[
  str_length(file_df$table_period)==4
  ] <- "year"

tmp <- str_sub(file_df$table_period, 5,5)
file_df$period_structure[tmp=="Q"] <- "year_quarter"
file_df$period_structure[tmp=="M"] <- "year_month"


file_df$period_structure[
  tmp=="-" & str_length(file_df$table_period)==9
  ] <- "year_start-end"

file_df$period_structure[
  str_length(file_df$table_period)==8
  ] <- "year_startend"


# Let's start just with year_structure variables

fn_year <- function(x, dir_loc){
  
  this_table_name <- x$table_name[1]
  this_table_name <- tolower(this_table_name)
  this_table_name <- str_replace_all(this_table_name, " ", "_")
  fn_inner <- function(xx, dir_loc_){
    
    this_filename <- xx$full_filename[1]
    this_year <- xx$table_period[1]
    dta <- read.csv(
        paste(
          dir_loc_,
          this_filename,
          sep="/"
        ),
        header=T
      )

      dta <- dta[-1,]
      names(dta[1]) <- "datazone"
      dta$year <- this_year
      dta[,-1] <- apply(dta[,-1], 2, as.numeric)
      return(dta)      
  }
  
  assign(
    this_table_name, 
    ddply(x, .(table_period), fn_inner, dir_loc_=dir_loc)
  )     
  return(get(this_table_name))
}

tables_year <- dlply(
  subset(file_df, subset=period_structure=="year"),
  .(table_name),
  fn_year,
  dir_loc=sns_dir
  )

tables_year <- llply(tables_year, remove.vars, names="table_period", info=F)
tables_year <- llply(tables_year, rename.vars, from="X", to="datazone", info=F)
tables_year <- llply(tables_year, arrange, year, datazone)


names(tables_year) <- tolower(
  str_replace_all(
    names(tables_year),  " ", "_"
    )
  )

# Spool to csv files

fn <- function(this_table_name){
  this_table <- tables_year[[this_table_name]]
  
  write.csv(
    this_table,
    file=paste0(
      "output_data/by_year/",
      this_table_name,
      ".csv"
      ),
    row.names=F
    )
}

l_ply(names(tables_year), fn, .progress="text")


# Now with quarter

file_df_qtrs <- subset(
  file_df,
  subset=period_structure=="year_quarter"
  )

year_qtrs <- str_split(
  file_df_qtrs$table_period,
  "Q"
  )

year_qtrs <- ldply(year_qtrs)
names(year_qtrs) <- c("year", "quarter")
year_qtrs <- apply(year_qtrs, 2, as.numeric)

file_df_qtrs <- cbind(
  file_df_qtrs,
  year_qtrs
  )


fn_year_qtr <- function(x, dir_loc){
  
  this_table_name <- x$table_name[1]
  this_table_name <- tolower(this_table_name)
  this_table_name <- str_replace_all(this_table_name, " ", "_")
  
  fn_inner <- function(xx, dir_loc_){
    
    this_filename <- xx$full_filename[1]
    this_year <- xx$year[1]
    this_quarter <- xx$quarter
    dta <- read.csv(
      paste(
        dir_loc_,
        this_filename,
        sep="/"
      ),
      header=T
    )
    
    dta <- dta[-1,]
    names(dta[1]) <- "datazone"
    dta$year <- this_year
    dta$quarter <- this_quarter
    dta[,-1] <- apply(dta[,-1], 2, as.numeric)
    return(dta)      
  }
  
  assign(
    this_table_name, 
    ddply(x, .(table_period), fn_inner, dir_loc_=dir_loc)
  )     
  return(get(this_table_name))
}

tables_year_qtr <- dlply(
  file_df_qtrs,
  .(table_name),
  fn_year_qtr,
  dir_loc=sns_dir
)


tables_year_qtr <- llply(tables_year_qtr, remove.vars, names="table_period", info=F)
tables_year_qtr <- llply(tables_year_qtr, rename.vars, from="X", to="datazone", info=F)
tables_year_qtr <- llply(tables_year_qtr, arrange, year, quarter, datazone)


names(tables_year_qtr) <- tolower(
  str_replace_all(
    names(tables_year_qtr),  " ", "_"
  )
)

# Spool to csv files

fn <- function(this_table_name){
  this_table <- tables_year_qtr[[this_table_name]]
  
  write.csv(
    this_table,
    file=paste0(
      "output_data/by_year_quarter/",
      this_table_name,
      ".csv"
    ),
    row.names=F
  )
}

l_ply(names(tables_year_qtr), fn, .progress="text")


#########################################################################

# Now with months

file_df_mnths <- subset(
  file_df,
  subset=period_structure=="year_month"
)

year_mnths <- str_split(
  file_df_mnths$table_period,
  "M"
)

year_mnths <- ldply(year_mnths)
names(year_mnths) <- c("year", "month")
year_mnths <- apply(year_mnths, 2, as.numeric)

file_df_mnths <- cbind(
  file_df_mnths,
  year_mnths
)


fn_year_mnth <- function(x, dir_loc){
  
  this_table_name <- x$table_name[1]
  this_table_name <- tolower(this_table_name)
  this_table_name <- str_replace_all(this_table_name, " ", "_")
  
  fn_inner <- function(xx, dir_loc_){
    
    this_filename <- xx$full_filename[1]
    this_year <- xx$year[1]
    this_month <- xx$month[1]
    dta <- read.csv(
      paste(
        dir_loc_,
        this_filename,
        sep="/"
      ),
      header=T
    )
    
    dta <- dta[-1,]
    names(dta[1]) <- "datazone"
    dta$year <- this_year
    dta$month <- this_month
    dta[,-1] <- apply(dta[,-1], 2, as.numeric)
    return(dta)      
  }
  
  assign(
    this_table_name, 
    ddply(x, .(table_period), fn_inner, dir_loc_=dir_loc)
  )     
  return(get(this_table_name))
}

tables_year_months <- dlply(
  file_df_mnths,
  .(table_name),
  fn_year_mnth,
  dir_loc=sns_dir
)


tables_year_months <- llply(tables_year_months, remove.vars, names="table_period", info=F)
tables_year_months <- llply(tables_year_months, rename.vars, from="X", to="datazone", info=F)
tables_year_months <- llply(tables_year_months, arrange, year, month, datazone)


names(tables_year_months) <- tolower(
  str_replace_all(
    names(tables_year_months),  " ", "_"
  )
)

# Spool to csv files

fn <- function(this_table_name){
  this_table <- tables_year_months[[this_table_name]]
  
  write.csv(
    this_table,
    file=paste0(
      "output_data/by_year_month/",
      this_table_name,
      ".csv"
    ),
    row.names=F
  )
}

l_ply(names(tables_year_months), fn, .progress="text")


###############################################################
# Now to aggregate the data that are by year and quarter
# or year and month, to year only

fn <- function(x){
  x$quarter <- NULL
  x <- recast(x, year + datazone ~ ..., id.var=c("year", "datazone"), 
              fun=sum,
              na.rm=T
              )
  return(x)
}


tables_year_qtr_to_year <- llply(
  tables_year_qtr,
  fn,
  .progress="text"
  )


fn <- function(this_table_name){
  this_table <- tables_year_qtr_to_year[[this_table_name]]
  
  write.csv(
    this_table,
    file=paste0(
      "output_data/by_year_aggregated/",
      this_table_name,
      ".csv"
    ),
    row.names=F
  )
}

l_ply(names(tables_year_qtr_to_year), fn, .progress="text")

# Now for month

fn <- function(x){
  x$month <- NULL
  x <- recast(x, year + datazone ~ ..., id.var=c("year", "datazone"), 
              fun=sum,
              na.rm=T
              )
  return(x)
}


tables_year_month_to_year <- llply(
  tables_year_months,
  fn
)

fn <- function(this_table_name){
  this_table <- tables_year_month_to_year[[this_table_name]]
  
  write.csv(
    this_table,
    file=paste0(
      "output_data/by_year_aggregated/",
      this_table_name,
      ".csv"
    ),
    row.names=F
  )
}

l_ply(names(tables_year_month_to_year), fn, .progress="text")



######################################################################
# So, how many files have I accessed?
table(file_df$period_structure)

# year : 491
# year_month : 27
# year_ 373 : 373

# so, 81% of the tables


# With year_startend and year_start_end period structures covered
# I now have 99.3% coverage

# Now to look at year_startend and year_start-end


#year_startend
file_df_year_startend <- subset(
  file_df,
  subset=period_structure=="year_startend"
)


fn_year_startend <- function(x, dir_loc){
  
  this_table_name <- x$table_name[1]
  this_table_name <- tolower(this_table_name)
  this_table_name <- str_replace_all(this_table_name, " ", "_")
  fn_inner <- function(xx, dir_loc_){
    
    this_filename <- xx$full_filename[1]
    this_year_range <- xx$table_period[1]
    this_year_first <- as.numeric(
      str_sub(this_year_range, 1,4)
      )
    
    this_year_last <- as.numeric(
      str_sub(this_year_range, 5,8)
      )
    
    
        
    dta <- read.csv(
      paste(
        dir_loc_,
        this_filename,
        sep="/"
      ),
      header=T
    )
    
    dta <- dta[-1,]
    names(dta[1]) <- "datazone"
    dta$year_first <- this_year_first
    dta$year_last <- this_year_last
    dta$year <- this_year_first + ((this_year_last - this_year_first)/2)    
    dta[,-1] <- apply(dta[,-1], 2, as.numeric)
    return(dta)      
  }
  
  assign(
    this_table_name, 
    ddply(x, .(table_period), fn_inner, dir_loc_=dir_loc)
  )     
  return(get(this_table_name))
}


tables_year_startend <- dlply(
  file_df_year_startend,
  .(table_name),
  fn_year_startend,
  dir_loc=sns_dir,
  .progress="text"
)

tables_year_startend <- llply(tables_year_startend, remove.vars, names="table_period", info=F)
tables_year_startend <- llply(tables_year_startend, rename.vars, from="X", to="datazone", info=F)
tables_year_startend <- llply(tables_year_startend, arrange, year, datazone)


names(tables_year_startend) <- tolower(
  str_replace_all(
    names(tables_year_startend),  " ", "_"
  )
)

# Spool to csv files

fn <- function(this_table_name){
  this_table <- tables_year_startend[[this_table_name]]
  
  write.csv(
    this_table,
    file=paste0(
      "output_data/by_year_startend/",
      this_table_name,
      ".csv"
    ),
    row.names=F
  )
}

l_ply(names(tables_year_startend), fn, .progress="text")



#################
#year_start_end
file_df_year_start_end <- subset(
  file_df,
  subset=period_structure=="year_start-end"
)


fn_year_start_end <- function(x, dir_loc){
  
  this_table_name <- x$table_name[1]
  this_table_name <- tolower(this_table_name)
  this_table_name <- str_replace_all(this_table_name, " ", "_")
  fn_inner <- function(xx, dir_loc_){
    
    this_filename <- xx$full_filename[1]
    this_year_range <- xx$table_period[1]
    tmp <- str_split(this_year_range, "-")
    this_year_first <- as.numeric(tmp[[1]][1])
    this_year_last <- as.numeric(tmp[[1]][2])

    
    
    
    dta <- read.csv(
      paste(
        dir_loc_,
        this_filename,
        sep="/"
      ),
      header=T
    )
    
    dta <- dta[-1,]
    names(dta[1]) <- "datazone"
    dta$year_first <- this_year_first
    dta$year_last <- this_year_last
    dta$year <- this_year_first + ((this_year_last - this_year_first)/2)    
    dta[,-1] <- apply(dta[,-1], 2, as.numeric)
    return(dta)      
  }
  
  assign(
    this_table_name, 
    ddply(x, .(table_period), fn_inner, dir_loc_=dir_loc)
  )     
  return(get(this_table_name))
}


tables_year_start_end <- dlply(
  file_df_year_start_end,
  .(table_name),
  fn_year_start_end,
  dir_loc=sns_dir,
  .progress="text"
)

tables_year_start_end <- llply(tables_year_start_end, remove.vars, names="table_period", info=F)
tables_year_start_end <- llply(tables_year_start_end, rename.vars, from="X", to="datazone", info=F)
tables_year_start_end <- llply(tables_year_start_end, arrange, year, datazone)


names(tables_year_start_end) <- tolower(
  str_replace_all(
    names(tables_year_start_end),  " ", "_"
  )
)

# Spool to csv files

fn <- function(this_table_name){
  this_table <- tables_year_start_end[[this_table_name]]
  
  write.csv(
    this_table,
    file=paste0(
      "output_data/by_year_start_end/",
      this_table_name,
      ".csv"
    ),
    row.names=F
  )
}

l_ply(names(tables_year_start_end), fn, .progress="text")


################################################################

require(plyr)
require(stringr)

# Collect and compile list of variables names 

sns_dir <- "G:/dropbox/Dropbox/Data/SNS_FullData_CSV_14_3_2013"
meta_files <- dir(sns_dir, pattern="^FullC[0-9]R0IndicatorMetaData")
all_inputs <- lapply(paste(sns_dir, meta_files, sep="/"), readLines)

# 4: identifier
# 12: title
# 13: ShortTitle
# 14: ValueType

fn <- function(x){
  identifiers <- str_replace_all(str_split(x[4], ",")[[1]], '\"', "")
  titles <- str_replace_all(str_split(x[12], ",")[[1]], '\"', "")
  short_titles <- str_replace_all(str_split(x[13], ",")[[1]], '\"', "")
  value_types <- str_replace_all(str_split(x[14], ",")[[1]], '\"', "")
  
  max_length <- max(
    length(identifiers) ,
    length(titles) ,
    length(short_titles),
    length(value_types)
    )
  
  identifier <- rep("", max_length)
  title <- rep("", max_length)
  short_title <- rep("", max_length)
  value_type <- rep("", max_length)
  
  identifier[1:(length(identifiers)-1)] <- identifiers[-1]
  title[1:(length(titles)-1)] <- titles[-1]
  short_title[1:(length(short_title)-1)] <- short_title[-1]
  value_type[1:(length(value_type)-1)] <- value_type[-1]
  
  out <- data.frame(
    identifier = identifier,
    title = title,
    short_title = short_title,
    value_type = value_type
    )
  return(out)
}

tmp <- ldply(all_inputs, fn)



#####################################################################################################
#####################################################################################################
#################################################################
# Want to process separately by structure of period variables
# First, I want to just find the variables that are years: these 
# are just four characters long

# Second, I want to process year_quarter variables. 
# these are 7 characters long, with Q as the 5th character

# Third, I want to process year_month variables. 
# these are 7 characters long, with M as the 5th character

# Forth, I want to process adjacent year variables
# These are in a range of formats .... misc for now




# Council house sales 1980 to 2005

years_to_read <-   1980:2005

fn <- function(this_year){
  dta <- read.csv(
    paste0(
      "G:/dropbox/Dropbox/Data/SNS_FullData_CSV_14_3_2013/Housing_1764_Council House Sales_",
      this_year,
      "_ZN_C0R0_2_7_2012.csv"
      ),
    header=T
    )
  dta <- dta[-1,]
  names(dta)[1] <- "datazone"
  dta[,2] <- as.numeric(dta[,2])
  dta[,3] <- as.numeric(dta[,3])
  dta[,4] <- as.numeric(dta[,4])
  dta$year <- this_year
  
  return(dta)
}

sales_all <- ldply(
  years_to_read,
  fn
  )


# Council house sales for all of scotland

sales_all_scotland <- ddply(
  sales_all,
  .(year),
  summarise,
  all_tenant_sales=sum(HO.alltenantsale),
  flat_tenant_sales=sum(HO.Flattenantsale),
  house_tenant_sales=sum(HO.Housetenantsale)
  )

sales_all_scotland$area <- "scotland"

qplot(x=year, y=all_tenant_sales, data=sales_all_scotland, geom="line")
# Now just for east end of glasgow

sales_east_end <- subset(
  sales_all,
  subset=datazone %in% east_end_dzs
  )

sales_east_end_combined <- ddply(
  sales_east_end,
  .(year),
  summarise,
  all_tenant_sales=sum(HO.alltenantsale),
  flat_tenant_sales=sum(HO.Flattenantsale),
  house_tenant_sales=sum(HO.Housetenantsale)
)

sales_east_end_combined$area <- "east_end"
qplot(x=year, y=all_tenant_sales, data=sales_east_end_combined, geom="line")

######################

sales_west_end <- subset(
  sales_all,
  subset=datazone %in% west_end_dzs
)

sales_west_end_combined <- ddply(
  sales_west_end,
  .(year),
  summarise,
  all_tenant_sales=sum(HO.alltenantsale),
  flat_tenant_sales=sum(HO.Flattenantsale),
  house_tenant_sales=sum(HO.Housetenantsale)
)

sales_west_end_combined$area <- "west_end"

qplot(x=year, y=all_tenant_sales, data=sales_west_end_combined, geom="line")


# council house sales 
council_house_sales_combined <- rbind(
  sales_all_scotland,
  sales_east_end_combined,
  sales_west_end_combined
  )


#### How about house price sales?


years_to_read <-   1993:2010

fn <- function(this_year){
  dta <- read.csv(
    paste0(
      "G:/dropbox/Dropbox/Data/SNS_FullData_CSV_14_3_2013/Housing_2094_House sales and prices_",
      this_year,
      "_ZN_C0R0_2_7_2012.csv"
    ),
    header=T
  )
  dta <- dta[-1,]
  names(dta)[1] <- "datazone"
  dta[,2] <- as.numeric(dta[,2])
  dta[,3] <- as.numeric(dta[,3])
  dta[,4] <- as.numeric(dta[,4])
  dta[,5] <- as.numeric(dta[,5])
  
  dta$year <- this_year
  
  return(dta)
}

house_prices_all <- ldply(
  years_to_read,
  fn
)


# Council house sales for all of scotland

house_prices_all_scotland <- ddply(
  house_prices_all,
  .(year),
  summarise,
  house_price_lower_quartile=median(HO.hpricelquartile),
  house_price_mean = median(HO.hpricemean),
  house_price_median = median(HO.hpricemedian),
  house_price_upper_quartile = median(HO.hpriceuquartile),
  number_of_house_sales = sum(HO.hsalesno)
)
house_prices_all_scotland$area <- "scotland"


qplot(x=year, y=number_of_house_sales, data=house_price_all_scotland, geom="line")
# Now just for east end of glasgow

house_prices_east_end <- subset(
  house_prices_all,
  subset=datazone %in% east_end_dzs
)

house_prices_all_east_end <- ddply(
  house_prices_east_end,
  .(year),
  summarise,
  house_price_lower_quartile=median(HO.hpricelquartile),
  house_price_mean = median(HO.hpricemean),
  house_price_median = median(HO.hpricemedian),
  house_price_upper_quartile = median(HO.hpriceuquartile),
  number_of_house_sales = sum(HO.hsalesno)
)

house_prices_all_east_end$area <- "east_end"


qplot(x=year, y=number_of_house_sales, data=house_prices_all_east_end, geom="line")

#####
house_prices_west_end <- subset(
  house_prices_all,
  subset=datazone %in% west_end_dzs
)

house_prices_all_west_end <- ddply(
  house_prices_west_end,
  .(year),
  summarise,
  house_price_lower_quartile=median(HO.hpricelquartile),
  house_price_mean = median(HO.hpricemean),
  house_price_median = median(HO.hpricemedian),
  house_price_upper_quartile = median(HO.hpriceuquartile),
  number_of_house_sales = sum(HO.hsalesno)
)

house_prices_all_west_end$area <- "west_end"

qplot(x=year, y=number_of_house_sales, data=house_prices_all_west_end, geom="line")

qplot(x=year, y=house_price_median, data=house_prices_all_west_end, geom="line")

qplot(x=year, y=house_price_median, data=house_prices_all_east_end, geom="line")

house_prices_combined <- rbind(
  house_prices_all_scotland,
  house_prices_all_east_end,
  house_prices_all_west_end
)


# Now JSA


years_to_read <-   1999:2011
quarters <- 1:4
df <- expand.grid(
  year=years_to_read,
  quarter=quarters
  )

fn <- function(x){
  this_year <- x$year
  this_quarter <- x$quarter
  
  
   dta <- try(
     read.csv(
       paste0(
         "G:/dropbox/Dropbox/Data/SNS_FullData_CSV_14_3_2013/Economic Activity Benefits and Tax Credits_2250_Job Seekers Allowance_",
         this_year,
         "Q0",
         this_quarter,
         "_ZN_C0R0_2_7_2012.csv"
       ),
       header=T
     )
   )
  
   if (class(dta)!="try-error"){
     dta <- dta[-1,]
     names(dta)[1] <- "datazone"
     dta[,2] <- as.numeric(dta[,2])
     dta[,3] <- as.numeric(dta[,3])
     dta[,4] <- as.numeric(dta[,4])
     dta[,5] <- as.numeric(dta[,5])
     dta[,6] <- as.numeric(dta[,6])
     dta[,7] <- as.numeric(dta[,7])
     dta$year <- this_year
     dta$quarter <- this_quarter
     
     return(dta)
          
   }
}

jsa_all <- ddply(
  df,
  .(year, quarter),
  fn
)


write.csv(jsa_all, "jsa_merged.csv")
jsa_scotland <- ddply(
  jsa_all,
  .(year),
  summarise,
  jsa_claimants_16_to_24 = sum(CS.JSA_16to24),
  jsa_claimants_25_to_49 = sum(CS.JSA_25to49),
  jsa_claimants_50_plus = sum(CS.JSA_50plus),
  jsa_claimants_female = sum(CS.JSA_female),
  jsa_claimants_male = sum(CS.JSA_male),
  jsa_claimants_total = sum(CS.JSA_total)
  )

jsa_scotland$area <- "scotland"


jsa_east_end <- subset(
  jsa_all,
  subset=datazone %in% east_end_dzs
)

jsa_all_east_end <- ddply(
  jsa_east_end,
  .(year),
  summarise,
  jsa_claimants_16_to_24 = sum(CS.JSA_16to24),
  jsa_claimants_25_to_49 = sum(CS.JSA_25to49),
  jsa_claimants_50_plus = sum(CS.JSA_50plus),
  jsa_claimants_female = sum(CS.JSA_female),
  jsa_claimants_male = sum(CS.JSA_male),
  jsa_claimants_total = sum(CS.JSA_total)
)

jsa_all_east_end$area <- "east_end"

#####
jsa_west_end <- subset(
  jsa_all,
  subset=datazone %in% west_end_dzs
)

jsa_all_west_end <- ddply(
  jsa_west_end,
  .(year),
  summarise,
  jsa_claimants_16_to_24 = sum(CS.JSA_16to24),
  jsa_claimants_25_to_49 = sum(CS.JSA_25to49),
  jsa_claimants_50_plus = sum(CS.JSA_50plus),
  jsa_claimants_female = sum(CS.JSA_female),
  jsa_claimants_male = sum(CS.JSA_male),
  jsa_claimants_total = sum(CS.JSA_total)
)

jsa_all_west_end$area <- "west_end"

###########

# Combine all into a single dataset

jsa_combined <- rbind(
  jsa_scotland,
  jsa_all_east_end,
  jsa_all_west_end
  )



# join all to a single dataset

all_simplified_data <- join(
  council_house_sales_combined,
  house_prices_combined  
  )

all_simplified_data <- join(
  all_simplified_data,
  jsa_combined
  )

write.csv(all_simplified_data, "all_simplified_data.csv")


###################################





# datazones for east end of Glasgow
east_end_dzs <- c(
  "S01003205",
  "S01003248",
  "S01003251",
  "S01003254",
  "S01003263",
  "S01003270",
  "S01003271",
  "S01003328",
  "S01003335",
  "S01003313",
  "S01003331",
  "S01003333",
  "S01003355",
  "S01003368",
  "S01003279",
  "S01003347",
  "S01003201",
  "S01003217",
  "S01003342",
  "S01003353",
  "S01003253",
  "S01003269",
  "S01003273",
  "S01003289",
  "S01003296",
  "S01003299",
  "S01003314"
)
# 27 Dzs 
# Source: http://www.scotland.gov.uk/Topics/ArtsCultureSport/Sport/MajorEvents/Glasgow-2014/Commonwealth-games/Indicators/S9

# 27 West End Datazones
west_end_dzs  <- c(
  "S01003437",
  "S01003450",
  "S01003453",
  "S01003454",
  "S01003460",
  "S01003464",
  "S01003466",
  "S01003468",
  "S01003470",
  "S01003474",
  "S01003478",
  "S01003479",
  "S01003484",
  "S01003485",
  "S01003487",
  "S01003497",
  "S01003501",
  "S01003503",
  "S01003504",
  "S01003509",
  "S01003513",
  "S01003514",
  "S01003520",
  "S01003521",
  "S01003522",
  "S01003542",
  "S01003545"
)


