
setwd('/Users/deigha/Documents/Projects/CC_Longitudinal_IF/qc_and_data_processing/bodyweight/')

################################################################################
# Load libraries etc ####

options(stringsAsFactors = FALSE)
library(tidyverse)

#####


################################################################################
# Helper functions ####

tab <- function(...,
                useNA = 'ifany'){
  x <- table(... = ...,
             useNA = useNA)
  return(x)
}

#####


################################################################################
# Load data ####

ORIGINAL_DATA <- REVISED_DATA  <- read_csv(
  'data/BodyWeight_20210719.csv',
  col_types = 'iiccccciccciccccccccccn',
  na = c('', 'NA', 'na', 'Na', 'nA', '-999')
)
# # A tibble: 68,860 x 23
#    TaskID OutputRowID AnimalName   LineName  Sex    AnimalStatus DateBorn     AgeInWeeks DateExit     ExitReason   Marker HousingID Comments                                                                       DateDue    JobID   StudyName             CollectionDateT… CollectedBy CompletedBy CompletedDate TaskName TaskNotes `Body Weight`
#     <int>       <int> <chr>        <chr>     <chr>  <chr>        <chr>             <int> <chr>        <chr>        <chr>      <int> <chr>                                                                          <chr>      <chr>   <chr>                 <chr>            <chr>       <chr>       <chr>         <chr>    <chr>             <dbl>
#  1   6495     1352170 CC IL16188-… CC IL161… Female Dead         5/4/2017 10…          5 5/13/2019 2… Found Dead … N             NA tumor forming on back right leg 2/26/19                                        6/6/2017 … CC Tue… Collaborative Cross … 6/6/2017 8:00:0… NA          NA          NA            Body We… NA                 18.5
#  2   6495     1352202 CC IL16188-… CC IL161… Female Dead         5/4/2017 10…          5 10/8/2018 9… Found Dead … R             NA tumor like mass forming under front arms(ventral side) 9/25/1 10/2/18 draging… 6/6/2017 … CC Tue… Collaborative Cross … 6/6/2017 8:00:0… NA          NA          NA            Body We… NA                 17.3
#  3   6495     1352234 CC IL16188-… CC IL161… Female Dead         5/4/2017 10…          5 3/25/2019 1… Found Dead … L             NA tumor forming on left leg 2/19/19                                              6/6/2017 … CC Tue… Collaborative Cross … 6/6/2017 8:00:0… NA          NA          NA            Body We… NA                 17.6
#  4   6495     1352266 CC IL16188-… CC IL161… Female Dead         5/4/2017 10…          5 7/13/2019 7… Found Dead … B             NA tumor like mass on left ventral side 12/26/18. Case 123-B8-2019-Severe Tumor-… 6/6/2017 … CC Tue… Collaborative Cross … 6/6/2017 8:00:0… NA          NA          NA            Body We… NA                 18.5
#  5   6495     1352422 CC IL16188-… CC IL161… Male   Dead         5/4/2017 10…          5 9/18/2018 8… Found Dead … NA            NA NA                                                                             6/6/2017 … CC Tue… Collaborative Cross … 6/6/2017 8:00:0… NA          NA          NA            Body We… NA                 20.9
#  6   6495     1352454 CC IL16188-… CC IL161… Male   Dead         5/4/2017 10…          5 10/2/2018 8… Found Dead … R             NA kyphosis, BCS 2, piloerection, thin and frail 9/25/18                          6/6/2017 … CC Tue… Collaborative Cross … 6/6/2017 8:00:0… NA          NA          NA            Body We… NA                 21.1
#  7   6495     1366940 CC IL16557-… CC IL165… Female Dead         5/10/2017 1…          4 10/28/2019 … Found Dead … N             NA NA                                                                             6/6/2017 … CC Tue… Collaborative Cross … 6/8/2017 8:00:0… NA          NA          NA            Body We… NA                 16.0
#  8   6495     1366974 CC IL16557-… CC IL165… Female Dead         5/10/2017 1…          4 7/1/2019 9:… Found Dead … R             NA ruffled fur, kyphosis right eye damage 12/26/18                                6/6/2017 … CC Tue… Collaborative Cross … 6/8/2017 8:00:0… NA          NA          NA            Body We… NA                 15.3
#  9   6495     1367008 CC IL16557-… CC IL165… Female Dead         5/10/2017 1…          4 5/13/2019 2… Found Dead … L             NA NA                                                                             6/6/2017 … CC Tue… Collaborative Cross … 6/8/2017 8:00:0… NA          NA          NA            Body We… NA                 14.4
# 10   6495     1367042 CC IL16557-… CC IL165… Female Dead         5/10/2017 1…          4 1/14/2019 2… Found Dead … B             NA NA                                                                             6/6/2017 … CC Tue… Collaborative Cross … 6/8/2017 8:00:0… NA          NA          NA            Body We… NA                 13.9
# # … with 68,850 more rows

#####


################################################################################
# Drop unnecessary columns ####

REVISED_DATA <- REVISED_DATA %>% 
  select(-c(
    TaskID, 
    LineName, Sex, AnimalStatus, DateBorn, AgeInWeeks, DateExit, ExitReason, Marker, HousingID, Comments,
    StudyName,TaskName,
    # CompletedBy is mising for all records, CompletedDate is not
    CompletedBy
  ))

#####


################################################################################
# rename columns ####

REVISED_DATA <- REVISED_DATA %>% 
  rename(
    MouseID = AnimalName,
    BWDay_Test = JobID,
    DateCollect = CollectionDateTime,
    DateComplete = CompletedDate,
    Tech = CollectedBy,
    BatchComments = TaskNotes,
    BW = `Body Weight`
  )

#####


################################################################################
# Fix MouseID names #### 

REVISED_DATA <- REVISED_DATA %>% 
  mutate(MouseID = str_replace(MouseID, 'CC ', ''))

#####


################################################################################
# Fix BW day #### 

REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    BWDay_Test = str_replace(
      BWDay_Test,
      '^CC ([[:alpha:]]+) BW$',
      '\\1'
    )
  )

#####


################################################################################
# Convert dates to Date type ####

REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    DateDue = as.Date(DateDue, format = '%m/%d/%Y'),
    DateCollect = as.Date(DateCollect, format = '%m/%d/%Y'),
    DateComplete = as.Date(DateComplete, format = '%m/%d/%Y')
  )

#####


################################################################################
# Fix technician column ####

REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    Tech = str_split(Tech, ' ', simplify = TRUE)[,1]
  )

#####


################################################################################
# order columns ####

REVISED_DATA <- REVISED_DATA %>% 
  select(
    OutputRowID, 
    MouseID, BWDay_Test, Tech, BatchComments,
    DateDue, DateCollect, DateComplete,
    BW,
    everything()
  )

#####


################################################################################
# Save data ####

REVISED_DATA %>% 
  write_csv('data/FORMATTED_BW_20210719.csv')
# # A tibble: 68,860 x 9
#    OutputRowID MouseID      BWDay_Test Tech  BatchComments DateDue    DateCollect DateComplete    BW
#          <int> <chr>        <chr>      <chr> <chr>         <date>     <date>      <date>       <dbl>
#  1     1352170 IL16188-5048 Tuesday    NA    NA            2017-06-06 2017-06-06  NA            18.5
#  2     1352202 IL16188-5049 Tuesday    NA    NA            2017-06-06 2017-06-06  NA            17.3
#  3     1352234 IL16188-5050 Tuesday    NA    NA            2017-06-06 2017-06-06  NA            17.6
#  4     1352266 IL16188-5051 Tuesday    NA    NA            2017-06-06 2017-06-06  NA            18.5
#  5     1352422 IL16188-5056 Tuesday    NA    NA            2017-06-06 2017-06-06  NA            20.9
#  6     1352454 IL16188-5057 Tuesday    NA    NA            2017-06-06 2017-06-06  NA            21.1
#  7     1366940 IL16557-5452 Tuesday    NA    NA            2017-06-06 2017-06-08  NA            16.0
#  8     1366974 IL16557-5453 Tuesday    NA    NA            2017-06-06 2017-06-08  NA            15.3
#  9     1367008 IL16557-5454 Tuesday    NA    NA            2017-06-06 2017-06-08  NA            14.4
# 10     1367042 IL16557-5455 Tuesday    NA    NA            2017-06-06 2017-06-08  NA            13.9
# # … with 68,850 more rows

#####


################################################################################
## clear workspace ###########################################

rm(list = ls())
pacman::p_unload('all')
graphics.off()

#####
