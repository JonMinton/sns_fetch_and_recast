# R script for extracting and compiling variables available at areal units other than dz


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
aus<- c(
  "LA", "SC", "IG", "H2", "CH", "HB", "P2",
"RC", "SP", "MW", "RL",  "W2")

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
# length is 1096


fn_outer <- function(x){
  current_au <- x
  pattern <- paste0("_", current_au, "_")
  
  relevant_filenames <<- list.files(
    path=sns_dir,
    pattern=pattern
  )
  browser()
  
  dir.create(paste0("mapped/from_",current_au,"/output_data/by_year/"),recursive=TRUE)
  dir.create(paste0("mapped/from_",current_au,"/output_data/by_year_quarter/"),recursive=TRUE)
  dir.create(paste0("mapped/from_",current_au,"/output_data/by_year_month/"),recursive=TRUE)
  dir.create(paste0("mapped/from_",current_au,"/output_data/by_year_aggregated/"),recursive=TRUE)
  dir.create(paste0("mapped/from_",current_au,"/output_data/by_year_aggregated/"),recursive=TRUE)
  dir.create(paste0("mapped/from_",current_au,"/output_data/by_year_startend/"),recursive=TRUE)
  dir.create(paste0("mapped/from_",current_au,"/output_data/by_year_start_end/"),recursive=TRUE)
  
  source("scripts/other_au_spooler_inner.r")
  
}

l_ply(aus, fn_outer)


