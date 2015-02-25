



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
  dir_loc=sns_dir,
  .progress="text"
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
      "mapped/from_",currrent_au,"/output_data/by_year/",
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
    this_quarter <- xx$quarter[1]
    dta <- read.csv(
      paste(
        dir_loc_,
        this_filename,
        sep="/"
      ),
      header=T
    )
    
    dta <- dta[-1,] 
    names(dta)[1] <- "datazone"
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
  dir_loc=sns_dir,
  .progress="text"
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
      "mapped/from_",currrent_au,"/output_data/by_year_quarter/",
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
      "mapped/from_",currrent_au,"/output_data/by_year_month/",
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
      "mapped/from_",currrent_au,"/output_data/by_year_aggregated/",
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
      "mapped/from_",currrent_au,"/output_data/by_year_aggregated/",
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
      "mapped/from_",currrent_au,"/output_data/by_year_startend/",
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
      "mapped/from_",currrent_au,"/output_data/by_year_start_end/",
      this_table_name,
      ".csv"
    ),
    row.names=F
  )
}

l_ply(names(tables_year_start_end), fn, .progress="text")