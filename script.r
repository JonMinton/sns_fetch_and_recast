# load packages
rm(list=ls())
gc()

require(plyr)
require(reshape2)
require(stringr)
require(ggplot2)
require(gdata)
require(foreign)


# ##########################################################################
# # Link datazones to postcodes
# 
# # Suggestion by Christina
# 
# # Dropbox link to file:
# # https://www.dropbox.com/s/373otkkuo1fdpht/latestpcinfowithlinkpc.sav?dl=0
# areal_unit_local_location <- "G:/dropbox/Dropbox/Data/Links_between_Areal_Units/latestpcinfowithlinkpc.sav"
# areal_unit_link <- "https://www.dropbox.com/s/373otkkuo1fdpht/latestpcinfowithlinkpc.sav?dl=0"
# 
# tmp <- read.spss(areal_unit_local_location, to.data.frame=T)
# 
# datazone_to_postcode <- tmp[,c("PostcodeFull", "PCSector", "PCDistrict","Datazone")]
# names(datazone_to_postcode) <- tolower(names(datazone_to_postcode))
# write.csv(datazone_to_postcode, file="output_data/postcode_links/datazone_to_postcode.csv")
# ########################################################################
# add sns_dir locations

# home dir
#sns_dir <- "G:/dropbox/Dropbox/Data/SNS_FullData_CSV_14_3_2013"
sns_dir <- "E:/Dropbox/Data/SNS_FullData_CSV_19_1_2015"
# office dir
#sns_dir <- "E:/Dropbox/Data/SNS_FullData_CSV_14_3_2013"


##########################################################################


# Want to know how many files have ZN in their title
length(list.files(path=sns_dir))

# length is 2498 files [2013]
# Length is now 6238 files!

# Now how many have _ZN_ in their title?

length(list.files(
  path=sns_dir,
  pattern="_ZN_"
  )
)

#1651 (2015)
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
rm(list=ls())
require(plyr)
require(stringr)

# Collect and compile list of variables names 

sns_dir <- "E:/Dropbox/Data/SNS_FullData_CSV_19_1_2015"
meta_files <- dir(sns_dir, pattern="^FullC[0-9]R0IndicatorMetaData")

all_inputs <- lapply(paste(sns_dir, meta_files, sep="/"), read.table, sep=",")
all_inputs <- ldply(all_inputs, 
                    function(x) {
                      out <- as.matrix(x); 
                      out <- t(out); 
                      colnms <- out[1,]; 
                      out <- out[-1,]; 
                      colnames(out) <- colnms; 
                      out <- data.frame(out); 
                      return(out)
                      }
                    ) 

# Now to link to the new tables




require(dplyr)

base_dir <- "output_data"


all_files <- list.files(base_dir, include.dirs=T, recursive=T, pattern=".csv$")
all_files <- all_files[-1]
all_files <- all_files[-length(all_files)]

fn <- function(x){
  file_name <- paste(base_dir, x, sep="/")
  
  this_file <- read.csv(file_name)
  
  min_year <- min(this_file$year)
  max_year <- max(this_file$year)
  var_names <- this_file %>% 
    select(-datazone, -year) %>% 
    names
  
  tmp <- str_split(x, "/")[[1]]
  if (length(tmp)!=2){ 
    out <- NULL
  } else {
    dir_name <- tmp[1]
    file_name <- str_replace(tmp[2], ".csv$", "")
    
    out <- data.frame(
      dir_name=dir_name,
      file_name=file_name,
      var_name=var_names,
      min_year=min_year,
      max_year=max_year
    ) 
  }    
  return(out)
}

output <- ldply(all_files, fn, .progress="text")

output <- output %>% tbl_df %>% filter(!(var_name %in% c("year_first", "year_last", "quarter")))

# 
# write.csv(output, file="G:/dropbox/Dropbox/Data/SNS/rearranged_data/var_names_by_location.csv", row.names=F)
# 

location_id <- output 
id_desc <- all_inputs
id_desc <- id_desc %>% tbl_df
id_desc$simple_id <- id_desc$Identifier %>% tolower %>% str_replace_all("[-.]", "_")

location_id$simple_id <- location_id$var_name %>% tolower %>% str_replace_all("[-.]", "_")

location_id_desc <- location_id %>% left_join(id_desc)
location_id_desc <- location_id_desc %>% select(var_name, description=ShortTitle,min_year, max_year, file_name, dir_name)

write.csv(
  location_id_desc, 
  file="output_data/metadata/var_names_and_dir_locations.csv", 
  row.names=F
  )

write.csv(
  id_desc,
  file="output_data/metadata/all_available_var_details.csv",
  row.names=F
  )


###############################################################################################################################
###############################################################################################################################

# 19/2/2015

# Some code to merge the data on urban_rural classifications found by Gavin

rm(list=ls())

# install.packages("plyr")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("dplyr")


require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

base_dir <- "E:/Dropbox/Data/SNS/urban_rural"

files_to_load <- list.files(path=paste(base_dir, "raw", sep="/"))

fn <- function(x){
  in_file <- read.csv(
    paste(base_dir, "raw", x, sep="/")
    ) %>% tbl_df()
  
  out <- in_file %>% 
    rename(datazone=GeographyCode) %>% 
    gather(key=year, value=urban_rural_class, -datazone)
  
  return(out)
}

output <- ldply(files_to_load, fn) %>% tbl_df
output$year <- output$year %>% 
  str_replace_all("X", "") %>% 
  str_replace_all("\\.", "_") 

write.csv(output, paste(base_dir, "tidied", "urban_rural.csv", sep="/"), row.names=F)




###########

# Var_identification ------------------------------------------------------


# Not all variables are available at datazone level. The purpose of the code below is to:
# report at what Geographic levels different variables are available.

rm(list=ls())

require(stringr)
require(tidyr)
require(plyr)
require(dplyr)

base_dir <- "E:/Dropbox/Data/SNS_FullData_CSV_19_1_2015/"


files_to_parse <- list.files(base_dir)
files_not_to_parse <- list.files(base_dir, pattern="^QuarterlyC[0-9]{1}R[0-9]{1}")
files_to_parse <- files_to_parse[!(files_to_parse %in% files_not_to_parse)]
files_not_to_parse <- list.files(base_dir, pattern="MetaData")
files_to_parse <- files_to_parse[!(files_to_parse %in% files_not_to_parse)]


fn <- function(x){
  first_two_lines <- readLines(
    paste0(base_dir, x),
    n=2
    ) %>%
    str_split(",")
  
  file_name <- x
  tokens <- file_name %>%
    str_replace("_C[0-9]{1}R[0-9]{1,2}_[0-9]{1,2}_[0-9]{1,2}_[0-9]{1,4}\\.csv$", "") %>%
    str_split("_") %>%
    unlist


  vars <- first_two_lines[[1]][-1]
  years <- first_two_lines[[2]][-1] %>%
    unique
  au <- tokens[length(tokens)]  
  
  out <- expand.grid(var=vars, year=years)
  if(dim(out)[1]>0){
    out <- data.frame(out, outer_group=tokens[1], inner_group=tokens[2], areal_unit=au, file_name=file_name)    
  } else {out <- NULL}
  
  
  return(out)
}

var_compilation <- ldply(files_to_parse, fn, .progress="text") %>%
  tbl_df

var_by_au <- var_compilation  %>% 
  distinct  %>% 
  mutate(tmp=1)  %>% 
  spread(areal_unit, tmp, fill=FALSE) %>%
  select(-inner_group, -file_name) %>%
  group_by(var) %>%
  summarise(
    LA=ifelse(sum(LA) > 0, 1, 0),
    SC=ifelse(sum(SC) > 0, 1, 0),
    ZN=ifelse(sum(ZN) > 0, 1, 0),
    IG=ifelse(sum(IG) > 0, 1, 0),
    H2=ifelse(sum(H2) > 0, 1, 0),
    CH=ifelse(sum(CH) > 0, 1, 0),
    HB=ifelse(sum(HB) > 0, 1, 0),
    P2=ifelse(sum(P2) > 0, 1, 0),
    RC=ifelse(sum(RC) > 0, 1, 0),
    SP=ifelse(sum(SP) > 0, 1, 0),
    MW=ifelse(sum(MW) > 0, 1, 0),
    RL=ifelse(sum(RL) > 0, 1, 0),
    W2=ifelse(sum(W2) > 0, 1, 0)
    ) %>%
  ungroup
  
var_details <- read.csv("E:/Dropbox/Data/SNS/2015_release/metadata/all_available_var_details.csv")  %>% 
  select(var=Identifier, ShortTitle) %>% 
  tbl_df

var_by_au <- var_by_au  %>% 
  left_join(var_details)  %>% 
  select(1,15, 2:14)

write.csv(var_by_au, file="E:/Dropbox/Data/SNS/2015_release/metadata/variables_by_areal_unit_availability.csv", row.names=F)

# Merge with title 

var_details <- read.csv("E:/Dropbox/Data/SNS/2015_release/metadata/all_available_var_details.csv")


