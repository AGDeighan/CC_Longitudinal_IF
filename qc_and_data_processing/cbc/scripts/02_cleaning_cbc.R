
setwd('/Users/deigha/Documents/Projects/CC_Longitudinal_IF/qc_and_data_processing/cbc/')

################################################################################
# Load libraries etc ####

options(stringsAsFactors = FALSE)
options(max.print = 5000)
library(lubridate)
library(tidyverse)
library(e1071)
COLORS <- c(
  'darkolivegreen', 'dodgerblue', 'darkorchid4', 'firebrick', 'orange', 
  'darkolivegreen3', 'lightskyblue', 'orchid4', 'tomato2', 'darkgoldenrod',
  'seagreen', 'darksalmon', 
  'mediumaquamarine', 'navyblue'
)

#####


################################################################################
# Helper functions ####

tab <- function(...,
                useNA = 'ifany'){
  x <- table(... = ...,
             useNA = useNA)
  return(x)
}

get_bw <- function(mouseid, date, bw_data = BW_DATA){
  
  # Retrieve the estimated bodyweight (from the weekly bodyweight data) for a
  # specific mouse at a specific date. This is done by taking the mean of 
  # temporarily closest preceeding and following LOESS-fitted bodyweights 
  # within two months of the specified date. If the specified date corresponds
  # exactly to a day with a weekly BW, than rather than a mean, just the 
  # LOESS-fitted BW from that day is returned. If there is no preceeding BW 
  # within two months than just the following LOESS-fitted BW is used. If there 
  # is no following LOESS-fitted BW within two months, than just the preceeding 
  # LOESS-fitted BW is used. If there is neither a preceeding or following BW
  # within two months, than an NA is returned.
  
  if(length(mouseid) > 1){
    BW <- mapply(get_bw, mouseid, date)
    BW <- unlist(BW)
    return(BW)
  }
  
  DATA <- bw_data %>%
    filter(
      MouseID == mouseid,
      !is.na(BW_LOESS),
      !is.na(DateCollect),
      abs(as.numeric(difftime(date, DateCollect, units = 'days'))) <= 62
    ) %>% 
    arrange(abs(as.numeric(difftime(date, DateCollect, units = 'days'))))
  
  DATA_EXACT <- DATA %>% 
    filter(
      DateCollect == date
    )
  
  if(nrow(DATA_EXACT) > 0){
    if(nrow(DATA_EXACT) > 1){
      stop('Should only be one row for an exact date match')
    }
    return(DATA_EXACT$BW_LOESS)
  } else{
    DATA_PRE <- DATA %>% 
      filter(
        DateCollect < date
      )
    DATA_FOL <- DATA %>% 
      filter(
        DateCollect > date
      )
    
    if(nrow(DATA_PRE) > 0 & nrow(DATA_FOL) > 0){
      return(
        mean(c(DATA_PRE$BW_LOESS[1], DATA_FOL$BW_LOESS[1]), na.rm = TRUE)
      )
    } else if(nrow(DATA_PRE) > 0){
      message(paste0(
        'No following weekly BWs within 1 month of ', date, ' assay for ', mouseid
      ))
      return(DATA_PRE$BW_LOESS[1])
    } else if(nrow(DATA_FOL) > 0){
      message(paste0(
        'No preceeding weekly BWs within 1 month of ', date, ' assay for ', mouseid
      ))
      return(DATA_FOL$BW_LOESS[1])
    } else{
      message(paste0(
        'No weekly BWs within 1 month of ', date, ' assay for ', mouseid
      ))
      return(NA)
    }
  }
  
}

logit_of_perc <- function(x, shift = 0.01, inverse = FALSE){
  if(inverse){
    # returns inverse log-odds of x
    ILT <- exp(x)/(1+exp(x))
    PERC <- ILT * 100
    UNSHIFT <- PERC - shift
    return(UNSHIFT)
  } else{
    # returns log-odds of x
    SHIFTED <- x + shift
    PROP <- SHIFTED/100
    LT <- log(PROP/(1-PROP))
    return(LT)
  }
}

log_tf <- function(x, shift = 0.01, inverse = FALSE){
  if(inverse){
    # returns inverse log-odds of x
    ILT <- exp(x)
    UNSHIFT <- ILT - shift
    return(UNSHIFT)
  } else{
    # returns log-odds of x
    SHIFTED <- x + shift
    LT <- log(SHIFTED)
    return(LT)
  }
}

#####


################################################################################
# Load data ####

ORIGINAL_DATA <- REVISED_DATA  <- read_csv(
  'data/FORMATTED_CBC_20210719.csv',
  col_types = paste0(
    'iicDDDcciccncn',
    paste0(rep('n', 32), collapse = ''),
    collapse = ''
  )
)
# # A tibble: 1,074 x 46
#    OutputRowID TaskID MouseID  DateDue    DateCollect DateComplete Tech  Comments  HID_Test Coat_Test EN_Test BW_Test Anesthesia NumClumps NumRBC NumRetic PercRetic   Hct   Hgb CalcHgb   MCV  CHCM  MCHC    CH   MCH   CHm   CHr RDWcv HDWsd NumWBC NumLymph NumNeut NumMono NumEos
#          <int>  <int> <chr>    <date>     <date>      <date>       <chr> <chr>        <int> <chr>     <chr>     <dbl> <chr>          <dbl>  <dbl>    <dbl>     <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>   <dbl>   <dbl>  <dbl>
#  1     1252746   3307 IL16188… 2017-03-28 2017-03-28  NA           John  Batch co…     3705 other     N          40.0 proparaca…      1241   9.31     404.      4.34  44.9  14.9    13.7  48.3  30.5  33    14.7  16    NA    16.4  14.6  2.06   3.15     1.9     0.62    0.09   0.51
#  2     1251082   3304 IL16188… 2018-03-27 2018-03-28  2018-10-23   Rach… Batch co…     3705 other     N          44.4 proparaca…        60   4.3      861.     20.0   26.7   8.1     7.6  62.1  28.3  30.1  17.5  18.7  17.9  18.1  19.7  2.8   12.6      2.88    8.97    0.45   0.26
#  3     1252747   3307 IL16188… 2017-03-28 2017-03-28  NA           John  Batch co…     3705 other     R          31.5 proparaca…      1516   9.77     481.      4.92  46.6  15.3    14    47.7  30.1  32.9  14.3  15.7  NA    16.4  15.2  2.04   3.72     1.87    0.86    0.14   0.82
#  4     1252748   3307 IL16188… 2017-03-28 2017-03-28  NA           John  Batch co…     3705 other     L          26.7 proparaca…       419   9.91     379.      3.83  47.6  15.5    14.2  48    29.9  32.6  14.3  15.7  NA    16.1  14    1.99   1.67     1.02    0.41    0.04   0.15
#  5     1251083   3304 IL16188… 2018-03-27 2018-03-28  2018-10-23   Rach… Batch co…     3705 other     L          32.2 proparaca…        88   7.91     461.      5.82  39.4  12.3    11.8  49.8  29.8  31.1  14.8  15.5  15.2  17.4  19.5  2.33   8.72     1.65    6.54    0.36   0.14
#  6     1252749   3307 IL16188… 2017-03-28 2017-03-28  NA           John  Batch co…     3705 other     B          43.5 proparaca…      1289  10.2      177.      1.73  48.7  16.1    14.8  47.6  30.3  32.9  14.4  15.7  NA    15.5  13.8  1.99   2.26     0.8     0.63    0.08   0.73
#  7     1252750   3307 IL16188… 2017-03-28 2017-03-28  NA           John  Batch co…     3708 other     N          24.3 proparaca…      1045   8.98     555.      6.18  44.6  14.4    13.6  49.7  30.4  32.2  15    16    NA    16.5  17.5  2.24   3.53     2.03    0.97    0.14   0.36
#  8     1252751   3307 IL16188… 2017-03-28 2017-03-28  NA           John  Batch co…     3708 other     R          23.4 proparaca…       608   9.54     359.      3.77  45.8  15.1    14    48    30.5  32.9  14.6  15.8  NA    17.4  16.2  1.99   1.99     0.96    0.57    0.06   0.38
#  9     1252752   3307 IL16188… 2017-03-28 2017-03-28  NA           John  Batch co…     3708 other     L          31.4 proparaca…      1459   9.57     410.      4.29  47    15.1    13.7  49.2  29.2  32.1  14.3  15.8  NA    17.1  16.4  1.95   2.04     1.04    0.52    0.07   0.4 
# 10     1251084   3304 IL16188… 2018-03-27 2018-03-28  2018-10-23   Rach… Batch co…     3708 other     L          30.0 proparaca…      1809   9.26     278.      3     43    14.4    12.9  46.4  30    33.6  13.9  15.6  14.3  15.8  12.9  1.87   2.37     0.54    0.89    0.08   0.85
# # … with 1,064 more rows, and 12 more variables: NumBaso <dbl>, NumLUC <dbl>, PercLymph <dbl>, PercNeut <dbl>, PercMono <dbl>, PercEos <dbl>, PercBaso <dbl>, PercLUC <dbl>, NumPlt <dbl>, MPV <dbl>, MPM <dbl>, PDWcv <dbl>



ANIMAL_DATA <- read_csv(
  'data/animal_data_processed_20210719.csv',
  col_types = 'ccccccccciicccDDcil'
) %>% 
  mutate(
    Sex = factor(Sex, levels = c('Female', 'Male')),
    Diet = factor(Diet, levels = c('AL', 'IF'))
  )

BW_DATA <- read_csv(
  'data/SpeedCleaned_BW_20210719.csv',
  col_types = 'cicccDDDinn'
)

#####


################################################################################
# Missing dates and date differences ####

# No records are missing either their due date or collection date. Many records
# are missing their completed by date (I don't know what this field is, it is
# new). There are some large differences between the completion date and 
# collection date. For all records for which the collection and completion
# date differ by more than a day, the collection date matches the date in the
# CSV filename (the CSV file of the raw data in the shared shock center folder 
# contains the date of the assay) in the shared folder. Similarly there are 
# some differences between the due dates and collection dates, but when 
# cross-checking with the files in the shared drive the collection dates all
# look correct. Since the collection dates appear correct, I will just drop the 
# due dates and completion dates from the table

## No missing due or collection dates, many missing completed by dates
REVISED_DATA %>% 
  filter(
    is.na(DateDue)
  ) %>% 
  nrow()
# [1] 0

REVISED_DATA %>% 
  filter(
    is.na(DateCollect)
  ) %>% 
  nrow()
# [1] 0

REVISED_DATA %>% 
  filter(
    is.na(DateComplete)
  ) %>% 
  nrow()
# [1] 426

REVISED_DATA %>% 
  filter(
    !is.na(DateComplete)
  ) %>% 
  nrow()
# [1] 648

# ---------------------------------------------------------------------------- #


## The collection and completion dates differ greatly for many of the records,
## but all the collection dates appear to be correct
REVISED_DATA %>% 
  filter(
    !is.na(DateComplete)
  ) %>% 
  ggplot() +
  theme_minimal() +
  geom_point(
    aes(x = DateCollect, y = DateComplete),
    alpha = 0.5
  )

REVISED_DATA %>% 
  filter(!is.na(MouseID)) %>% 
  filter(!is.na(DateComplete)) %>% 
  mutate(
    CompleteDateDiff = as.numeric(difftime(DateCollect, DateComplete, units = 'days'))
  ) %>% 
  filter(
    abs(CompleteDateDiff) > 1
  ) %>% 
  left_join(
    ANIMAL_DATA
  ) %>% 
  group_by(
    CompleteDateDiff, DateCollect, DateComplete, JobGroup
  ) %>% 
  summarise(
    N = n()
  ) %>% 
  arrange(
    desc(abs(CompleteDateDiff))
  ) %>% 
  data.frame()
#    CompleteDateDiff DateCollect DateComplete   JobGroup  N
# 1              -209  2018-03-28   2018-10-23 W1G1 Day 1 18    # Checked the shared drive, collection date matches filename
# 2              -209  2018-03-28   2018-10-23 W1G1 Day 2 15    # Checked the shared drive, collection date matches filename
# 3              -191  2018-04-16   2018-10-24 W4G1 Day 1 37    # Checked the shared drive, collection date matches filename
# 4              -190  2018-04-17   2018-10-24 W4G1 Day 2 39    # Checked the shared drive, collection date matches filename
# 5              -176  2018-04-30   2018-10-23 W1G2 Day 1 16    # Checked the shared drive, collection date matches filename
# 6              -175  2018-05-01   2018-10-23 W1G2 Day 2 28    # Checked the shared drive, collection date matches filename
# 7              -154  2018-05-23   2018-10-24 W4G2 Day 1 34    # Checked the shared drive, collection date matches filename
# 8              -153  2018-05-24   2018-10-24 W4G2 Day 2 33    # Checked the shared drive, collection date matches filename
# 9               -32  2019-08-19   2019-09-20 W5G1 Day 1 15    # Checked the shared drive, collection date matches filename
# 10              -31  2019-08-20   2019-09-20 W5G1 Day 2 17    # Checked the shared drive, collection date matches filename
# 11              -24  2019-05-06   2019-05-30 W1G2 Day 2  1    # The collection date is correct, this mouse (OR13067-5807) was tested with the 35 month timepoint of the G23W1 DO mice in the longitudinal DO diet project with Calico  
# 12              -17  2018-09-04   2018-09-21 W5G1 Day 1 35    # Checked the shared drive, collection date matches filename
# 13              -16  2018-09-05   2018-09-21 W5G1 Day 2 37    # Checked the shared drive, collection date matches filename
# 14              -15  2018-08-14   2018-08-29 W2G1 Day 1 16    # Checked the shared drive, collection date matches filename
# 15              -15  2019-10-01   2019-10-16 W2G2 Day 2  1    # The collection date is correct, this mouse (OR13067-5821) was tested with the W5G2 Day 1 mice
# 16              -15  2019-10-01   2019-10-16 W5G2 Day 1 15    # Checked the shared drive, collection date matches filename
# 17              -14  2018-08-15   2018-08-29 W2G1 Day 2 13    # Checked the shared drive, collection date matches filename
# 18              -14  2019-10-02   2019-10-16 W5G2 Day 2 24    # Checked the shared drive, collection date matches filename
# 19              -11  2018-09-24   2018-10-05 W5G2 Day 1 28    # Checked the shared drive, collection date matches filename
# 20              -10  2018-09-25   2018-10-05 W5G2 Day 2 37    # Checked the shared drive, collection date matches filename
# 21              -10  2018-12-17   2018-12-27 W3G1 Day 1 16    # Checked the shared drive, collection date matches filename
# 22              -10  2018-12-17   2018-12-27 W3G1 Day 2 17    # Checked the shared drive, collection date matches filename
# 23               -9  2018-09-26   2018-10-05 W2G2 Day 2 24    # Checked the shared drive, collection date matches filename
# 24               -9  2019-05-21   2019-05-30 W4G2 Day 1 16    # Checked the shared drive, collection date matches filename
# 25               -9  2019-05-21   2019-05-30 W4G2 Day 2 22    # Checked the shared drive, collection date matches filename
# 26               -8  2018-09-26   2018-10-04 W2G2 Day 1 18    # Checked the shared drive, collection date matches filename
# 27               -8  2019-01-08   2019-01-16 W3G2 Day 1 16    # Checked the shared drive, collection date matches filename
# 28               -8  2019-01-08   2019-01-16 W3G2 Day 2 28    # Checked the shared drive, collection date matches filename
# 29               -8  2019-04-16   2019-04-24 W4G1 Day 1 15    # Checked the shared drive, collection date matches filename
# 30               -8  2019-04-16   2019-04-24 W4G1 Day 2 16    # Checked the shared drive, collection date matches filename

# ---------------------------------------------------------------------------- #


## There are 4 collection dates (111 records) that differ from their due dates
## by more than 3 weeks. There are another two collection dates (65 records)
## that differ from their due dates by more than 2 weeks, and another 7 
## collection dates (144 records) that differ from their due dates by more than
## one week. We need to talk to Laura and the animal care technicians to
## determine if the dates are correct and the differences are due to scheduling
## issues. All the collection dates appear to be correct
REVISED_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_point(
    aes(x = DateCollect, y = DateDue),
    alpha = 0.5
  )

REVISED_DATA %>% 
  filter(!is.na(MouseID)) %>% 
  mutate(
    DueDateDiff = as.numeric(difftime(DateCollect, DateDue, units = 'days'))
  ) %>% 
  filter(
    abs(DueDateDiff) > 1
  ) %>% 
  left_join(
    ANIMAL_DATA
  ) %>% 
  group_by(
    DueDateDiff, DateCollect, DateDue, JobGroup
  ) %>% 
  summarise(
    N = n()
  ) %>% 
  arrange(
    desc(abs(DueDateDiff))
  ) %>% 
  data.frame()
#    DueDateDiff DateCollect    DateDue   JobGroup  N
# 1           26  2018-09-05 2018-08-10 W5G1 Day 2 37   # Checked the shared drive, collection date matches filename
# 2           25  2018-09-04 2018-08-10 W5G1 Day 1 35   # Checked the shared drive, collection date matches filename
# 3           24  2019-10-01 2019-09-07 W5G2 Day 1 15   # Checked the shared drive, collection date matches filename
# 4           24  2019-10-02 2019-09-08 W5G2 Day 2 24   # Checked the shared drive, collection date matches filename
# 5           16  2018-09-24 2018-09-08 W5G2 Day 1 28   # Checked the shared drive, collection date matches filename
# 6           16  2018-09-25 2018-09-09 W5G2 Day 2 37   # Checked the shared drive, collection date matches filename
# 7           12  2019-10-01 2019-09-19 W2G2 Day 2  1   # The collection date is correct, this mouse (OR13067-5821) was tested with the W5G2 Day 1 mice
# 8           11  2019-08-20 2019-08-09 W5G1 Day 2 17   # Checked the shared drive, collection date matches filename
# 9           10  2019-08-19 2019-08-09 W5G1 Day 1 15   # Checked the shared drive, collection date matches filename
# 10           9  2017-09-27 2017-09-18 W2G2 Day 1 31   # Checked the shared drive, collection date matches filename
# 11           9  2017-09-28 2017-09-19 W2G2 Day 2 37   # Checked the shared drive, collection date matches filename
# 12           9  2018-09-26 2018-09-17 W2G2 Day 1 18   # Checked the shared drive, collection date matches filename
# 13           8  2018-09-26 2018-09-18 W2G2 Day 2 24   # Checked the shared drive, collection date matches filename
# 14          -7  2017-04-24 2017-05-01 W1G2 Day 1 33   # Checked the shared drive, collection date matches filename
# 15          -7  2017-04-25 2017-05-02 W1G2 Day 2 37   # Checked the shared drive, collection date matches filename
# 16          -7  2019-01-08 2019-01-15 W3G2 Day 2 28   # Checked the shared drive, collection date matches filename
# 17          -6  2019-01-08 2019-01-14 W3G2 Day 1 16   # Checked the shared drive, collection date matches filename
# 18           4  2019-05-06 2019-05-02 W1G2 Day 2  1   # The collection date is correct, this mouse (OR13067-5807) was tested with the 35 month timepoint of the G23W1 DO mice in the longitudinal DO diet project with Calico  
# 19           2  2017-08-16 2017-08-14 W2G1 Day 1 38   # Checked the shared drive, collection date matches filename    
# 20           2  2017-08-17 2017-08-15 W2G1 Day 2 32   # Checked the shared drive, collection date matches filename
# 21           2  2018-03-28 2018-03-26 W1G1 Day 1 18   # Collection date is corrected, day 1 and 2 groups were tested on same day
# 22           2  2018-05-23 2018-05-21 W4G2 Day 1 34   # Checked the shared drive, collection date matches filename
# 23           2  2018-05-24 2018-05-22 W4G2 Day 2 33   # Checked the shared drive, collection date matches filename

# ---------------------------------------------------------------------------- #

REVISED_DATA <- REVISED_DATA %>% 
  select(-c(
    DateDue, DateComplete
  ))

#####


################################################################################
# Remove records missing both mouse ID and CBC data ####

# One record is missing both its mouse ID and all its CBC data

REVISED_DATA %>% 
  filter_at(
    vars(MouseID, NumClumps:PDWcv), all_vars(is.na(.))
  )
# A tibble: 1 x 44
#   OutputRowID TaskID MouseID DateCollect Tech  Comments HID_Test Coat_Test EN_Test BW_Test Anesthesia NumClumps NumRBC NumRetic PercRetic   Hct   Hgb CalcHgb   MCV  CHCM  MCHC    CH   MCH   CHm   CHr
#         <int>  <int> <chr>   <date>      <chr> <chr>       <int> <chr>     <chr>     <dbl> <chr>          <dbl>  <dbl>    <dbl>     <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1      693321   3710 NA      2018-09-26  Hann… Batch c…       NA NA        NA           NA NA                NA     NA       NA        NA    NA    NA      NA    NA    NA    NA    NA    NA    NA    NA
# # … with 19 more variables: RDWcv <dbl>, HDWsd <dbl>, NumWBC <dbl>, NumLymph <dbl>, NumNeut <dbl>, NumMono <dbl>, NumEos <dbl>, NumBaso <dbl>, NumLUC <dbl>, PercLymph <dbl>, PercNeut <dbl>,
# #   PercMono <dbl>, PercEos <dbl>, PercBaso <dbl>, PercLUC <dbl>, NumPlt <dbl>, MPV <dbl>, MPM <dbl>, PDWcv <dbl>

REVISED_DATA %>% 
  filter_at(
    vars(MouseID, NumClumps:PDWcv), all_vars(is.na(.))
  ) %>% 
  write_csv(
    'issues/records_missing_mouseid_and_cbc.csv'
  )

REVISED_DATA <- REVISED_DATA %>% 
  filter_at(
    vars(MouseID, NumClumps:PDWcv), any_vars(!is.na(.))
  )

#####


################################################################################
# Records missing mouse ID only ####

# No records are missing just the mouse ID

REVISED_DATA %>% 
  filter(
    is.na(MouseID)
  ) %>% 
  nrow()
# [1] 0

#####


################################################################################
# Records missing test BW only ####

# No records are missing their test BW

REVISED_DATA %>% 
  filter(
    is.na(BW_Test)
  ) %>% 
  nrow()
# [1] 19

#####


################################################################################
# Records missing CBC only ####

# There are two records that have their mouse ID but are missing all their CBC
# data. They both have a sample comment indicating that the data is truly 
# missing so this is not an error. We will drop these two records.

REVISED_DATA %>% 
  filter_at(
    vars(NumClumps:PDWcv), all_vars(is.na(.))
  ) %>% 
  nrow()
# [1] 2

REVISED_DATA %>% 
  filter_at(
    vars(NumClumps:PDWcv), all_vars(is.na(.))
  ) %>% 
  select(
    MouseID:Anesthesia
  ) %>% 
  data.frame()
#        MouseID DateCollect      Tech                                                      Comments HID_Test Coat_Test EN_Test BW_Test   Anesthesia
# 1 IL16188-5015  2017-03-28      John Batch comments: none |  Sample comments: Mishandled No sample     3707     other       B   42.90 proparacaine
# 2 IL16750-5763  2018-05-23 Mackenzie           Batch comments: none |  Sample comments: No Sample?     6088     other       B   27.26         <NA>

REVISED_DATA <- REVISED_DATA %>% 
  filter_at(
    vars(NumClumps:PDWcv), any_vars(!is.na(.))
  ) 

#####


################################################################################
# Remove complete duplications ####

# No complete duplications

REVISED_DATA[
  (
    duplicated(
      REVISED_DATA[,c('MouseID', 'DateCollect', names(REVISED_DATA)[12:44])]
    ) |
      duplicated(
        REVISED_DATA[,c('MouseID', 'DateCollect', names(REVISED_DATA)[12:44])],
        fromLast = TRUE
      )
  ),
] %>% 
  nrow()
# [1] 0

#####


################################################################################
# Multiple records for one date ####

# No multiple measures for a mouse on the same date

REVISED_DATA[
  (
    duplicated(
      REVISED_DATA[,c('MouseID', 'DateCollect')]
    ) |
      duplicated(
        REVISED_DATA[,c('MouseID', 'DateCollect')],
        fromLast = TRUE
      )
  ),
] %>% 
  nrow()
# [1] 0

#####


################################################################################
# Multiple records per year ####

# No mice have more than one record per year (assays are about one year apart)

X <- REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, DOB
      ),
    by = 'MouseID'
  ) %>% 
  mutate(
    AgeInMonths = as.numeric(difftime(DateCollect, DOB, units = 'days')) / 30.4,
    Timepoint = ifelse(
      AgeInMonths <= 13, 'Y1',
      ifelse(
        AgeInMonths <= 25, 'Y2',
        ifelse(
          AgeInMonths < 37, 'Y3',
          'ERROR!'
        )
      )
    )
  )

X[
  (
    duplicated(
      X[,c('MouseID', 'DateCollect', 'Timepoint')]
    ) |
      duplicated(
        X[,c('MouseID', 'DateCollect', 'Timepoint')],
        fromLast = TRUE
      )
  ),
] %>% 
  nrow()
# [1] 0

rm(X)

#####


################################################################################
# Other duplicated output row IDs ####

# No records with duplicated output row IDs

REVISED_DATA[
  (
    duplicated(REVISED_DATA$OutputRowID) | 
      duplicated(REVISED_DATA$OutputRowID, fromLast = TRUE)
  ),
] %>% 
  nrow()
# [1] 0

#####


################################################################################
# Records after death ####

# No records measured after the death of a mouse

REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, DOE
      ),
    by = 'MouseID'
  ) %>% 
  filter(
    DateCollect > DOE
  ) %>% 
  nrow()
# [1] 0

#####


################################################################################
# Records before birth ####

# No records measured before the birth of a mouse

REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, DOB
      ),
    by = 'MouseID'
  ) %>% 
  filter(
    DateCollect < DOB
  ) %>% 
  nrow()
# [1] 0

#####


################################################################################
# Impossible measures ####

# CBC metrics should all be positive. There are no records with negative values

REVISED_DATA %>% 
  filter_at(
    vars(NumClumps:PDWcv), any_vars(. < 0)
  ) %>% 
  nrow()
# [1] 0

#####


################################################################################
# Impossibly high test bodyweights ####

# There is one impossibly high test bodyweight. This is the result of a typo
# in the original files:
#   - ShockCenter/Current Files/Projects - Ongoing/Collaborative Cross Mice/CC Project 3_Shock Grant/Bleeds/Blood 3/49 weeks/20170327-CC-W1G1-Day 1.xlsx
#   - ShockCenter/Current Files/Projects - Ongoing/Collaborative Cross Mice/CC Project 3_Shock Grant/Bleeds/Blood 3/CSV Data/49 weeks/Upload_20170327_CC_W1G1_Day1.csv
# I don't think it is necessary to fix, recompile, and then reimport the 
# original files to Climb. If it is easy to fix it within Climb than that may
# be worth doing, but not if it risks messing something else up

REVISED_DATA$BW_Test %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.51   25.40   29.15   33.59   33.60 3658.00 

REVISED_DATA %>% 
  filter(BW_Test < 200) %>% 
  select(BW_Test) %>% 
  unlist() %>% 
  hist(
    breaks = 300
  )

REVISED_DATA %>% 
  filter(BW_Test < 200) %>% 
  select(BW_Test) %>% 
  summary()
#    BW_Test       
# Min.   : 2.51  
# 1st Qu.:25.39  
# Median :29.14  
# Mean   :30.21  
# 3rd Qu.:33.59  
# Max.   :53.75 

(
  HIGH_BW <- REVISED_DATA %>% 
    filter(
      BW_Test > 100
    )
) %>% 
  select(MouseID:BW_Test)
# # A tibble: 1 x 8
#   MouseID      DateCollect Tech  Comments                                      HID_Test Coat_Test EN_Test BW_Test
#   <chr>        <date>      <chr> <chr>                                            <int> <chr>     <chr>     <dbl>
# 1 IL16557-5406 2017-03-27  Daria Batch comments: none |  Sample comments: none     3613 other     2R1L       3658

HIGH_BW %>% 
  write_csv(
    'issues/impossibly_high_test_bw.csv'
  )

REVISED_DATA$BW_Test[REVISED_DATA$OutputRowID == HIGH_BW$OutputRowID[1]] <- 36.58

hist(
  REVISED_DATA$BW_Test,
  breaks = 300
)

rm(HIGH_BW)

#####


################################################################################
# Impossibly low test bodyweights ####

# There is one impossibly low test bodyweight. This is the result of a typo
# in the original files:
#   - ShockCenter/Current Files/Projects - Ongoing/Collaborative Cross Mice/CC Project 3_Shock Grant/Bleeds/Blood 3/49 weeks/20180905-CC-W5G1-Day 2.xlsx
#   - ShockCenter/Current Files/Projects - Ongoing/Collaborative Cross Mice/CC Project 3_Shock Grant/Bleeds/Blood 3/CSV Data/49 weeks/Upload_20180905_CC_W5G1_Day2.csv
# However, it is not clear what the true value should be, we will replace this
# test BW with an NA

REVISED_DATA$BW_Test %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.51   25.40   29.15   30.21   33.60   53.75 

(
  LOW_BW <- REVISED_DATA %>% 
    filter(
      BW_Test < 5
    )
) %>% 
  select(MouseID:BW_Test)
# # A tibble: 1 x 8
#   MouseID      DateCollect Tech   Comments                                      HID_Test Coat_Test EN_Test BW_Test
#   <chr>        <date>      <chr>  <chr>                                            <int> <chr>     <chr>     <dbl>
# 1 IL16188-5064 2018-09-05  Rachel Batch comments: none |  Sample comments: none     6409 other     N          2.51

LOW_BW %>% 
  write_csv(
    'issues/impossibly_low_test_bw.csv'
  )




REVISED_DATA$BW_Test[REVISED_DATA$OutputRowID == LOW_BW$OutputRowID[1]] <- as.numeric(NA)


hist(
  REVISED_DATA$BW_Test,
  breaks = 300
)

rm(LOW_BW)

#####


################################################################################
# Create a timepoint column ####

REVISED_DATA <- REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, DOB
      ),
    by = 'MouseID'
  ) %>% 
  mutate(
    AgeInDays = as.numeric(difftime(DateCollect, DOB, units = 'days')),
    Timepoint = ifelse(
      AgeInDays/30.4 <= 13, 'Year 1',
      ifelse(
        AgeInDays/30.4 <= 25, 'Year 2',
        ifelse(
          AgeInDays/30.4 < 37, 'Year 3',
          'ERROR!'
        )
      )
    )
  ) %>% 
  select(-c(
    DOB
  )) %>% 
  select(
    OutputRowID:MouseID,
    Timepoint, AgeInDays,
    everything()
  )

REVISED_DATA %>% 
  group_by(Timepoint) %>% 
  summarise(
    Mean = mean(AgeInDays/30.4),
    Median = median(AgeInDays/30.4),
    Min = min(AgeInDays/30.4),
    Max = max(AgeInDays/30.4)
  ) %>% 
  data.frame()
#   Timepoint     Mean   Median       Min      Max
# 1    Year 1 11.37840 11.41447  9.210526 12.13816
# 2    Year 2 23.30804 23.35526 21.414474 24.21053
# 3    Year 3 34.70395 34.70395 33.585526 35.82237

#####


################################################################################
# Verify collection data ####

# This is somewhat redundant to the previous step where we inspected differences
# between due, completion, and colecction dates, but is still needs to be done
# because we need to verify that ALL the collection dates are accurate to the
# day. We do this by comparing the collection dates in the data downloaded from
# Climb to the collection dates shown in the filenames of the CSV files in the
# JAC shared folder: 
#   ShockCenter/Current Files/Projects - Ongoing/Collaborative Cross Mice/CC Project 3_Shock Grant/Bleeds/Blood 3/CSV Data/
# There are two batches that need to be shifted a day earlier
#   - W3G2 Day 1, Year 1, 2018-01-16, 35 mice: off by +1 day (should be 2018-01-15)
#   - W3G2 Day 2, Year 1, 2018-01-17, 40 mice: off by +1 day (should be 2018-01-16)
# We will need to recalculate the age in days after making the fix, and should
# also recalculate the timepoints (though they shouldn't change) because they
# a derived from the age in days


REVISED_DATA %>% 
  select(
    MouseID, Timepoint, DateCollect
  ) %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(MouseID, JobGroup)
  ) %>% 
  group_by(
    JobGroup, Timepoint, DateCollect
  ) %>% 
  summarise(
    N = n()
  ) %>% 
  ungroup() %>% 
  arrange(Timepoint, JobGroup, DateCollect) %>% 
  data.frame()
#      JobGroup Timepoint DateCollect  N
# 1  W1G1 Day 1    Year 1  2017-03-27 34  # Matches CSV filename in JAC shared folder
# 2  W1G1 Day 2    Year 1  2017-03-28 36  # Matches CSV filename in JAC shared folder
# 3  W1G2 Day 1    Year 1  2017-04-24 33  # Matches CSV filename in JAC shared folder
# 4  W1G2 Day 2    Year 1  2017-04-25 37  # Matches CSV filename in JAC shared folder
# 5  W2G1 Day 1    Year 1  2017-08-16 38  # Matches CSV filename in JAC shared folder
# 6  W2G1 Day 2    Year 1  2017-08-17 32  # Matches CSV filename in JAC shared folder
# 7  W2G2 Day 1    Year 1  2017-09-27 31  # Matches CSV filename in JAC shared folder
# 8  W2G2 Day 2    Year 1  2017-09-28 37  # Matches CSV filename in JAC shared folder
# 9  W3G1 Day 1    Year 1  2017-12-18 36  # Matches CSV filename in JAC shared folder
# 10 W3G1 Day 2    Year 1  2017-12-19 36  # Matches CSV filename in JAC shared folder
# 11 W3G2 Day 1    Year 1  2018-01-16 35  # Off by +1 day (should be 2018-01-15)
# 12 W3G2 Day 2    Year 1  2018-01-17 40  # Off by +1 day (should be 2018-01-16)
# 13 W4G1 Day 1    Year 1  2018-04-16 37  # Matches CSV filename in JAC shared folder
# 14 W4G1 Day 2    Year 1  2018-04-17 39  # Matches CSV filename in JAC shared folder
# 15 W4G2 Day 1    Year 1  2018-05-23 33  # Matches CSV filename in JAC shared folder
# 16 W4G2 Day 2    Year 1  2018-05-24 33  # Matches CSV filename in JAC shared folder
# 17 W5G1 Day 1    Year 1  2018-09-04 35  # Matches CSV filename in JAC shared folder
# 18 W5G1 Day 2    Year 1  2018-09-05 37  # Matches CSV filename in JAC shared folder
# 19 W5G2 Day 1    Year 1  2018-09-24 28  # Matches CSV filename in JAC shared folder
# 20 W5G2 Day 2    Year 1  2018-09-25 37  # Matches CSV filename in JAC shared folder

# 21 W1G1 Day 1    Year 2  2018-03-28 18  # Matches CSV filename in JAC shared folder
# 22 W1G1 Day 2    Year 2  2018-03-28 15  # Matches CSV filename in JAC shared folder
# 23 W1G2 Day 1    Year 2  2018-04-30 16  # Matches CSV filename in JAC shared folder
# 24 W1G2 Day 2    Year 2  2018-05-01 28  # Matches CSV filename in JAC shared folder
# 25 W2G1 Day 1    Year 2  2018-08-14 16  # Matches CSV filename in JAC shared folder
# 26 W2G1 Day 2    Year 2  2018-08-15 13  # Matches CSV filename in JAC shared folder
# 27 W2G2 Day 1    Year 2  2018-09-26 18  # Matches CSV filename in JAC shared folder
# 28 W2G2 Day 2    Year 2  2018-09-26 24  # Matches CSV filename in JAC shared folder
# 29 W3G1 Day 1    Year 2  2018-12-17 16  # Matches CSV filename in JAC shared folder
# 30 W3G1 Day 2    Year 2  2018-12-17 17  # Matches CSV filename in JAC shared folder
# 31 W3G2 Day 1    Year 2  2019-01-08 16  # Matches CSV filename in JAC shared folder
# 32 W3G2 Day 2    Year 2  2019-01-08 28  # Matches CSV filename in JAC shared folder
# 33 W4G1 Day 1    Year 2  2019-04-16 15  # Matches CSV filename in JAC shared folder
# 34 W4G1 Day 2    Year 2  2019-04-16 16  # Matches CSV filename in JAC shared folder
# 35 W4G2 Day 1    Year 2  2019-05-21 16  # Matches CSV filename in JAC shared folder
# 36 W4G2 Day 2    Year 2  2019-05-21 22  # Matches CSV filename in JAC shared folder
# 37 W5G1 Day 1    Year 2  2019-08-19 15  # Matches CSV filename in JAC shared folder
# 38 W5G1 Day 2    Year 2  2019-08-20 17  # Matches CSV filename in JAC shared folder
# 39 W5G2 Day 1    Year 2  2019-10-01 15  # Matches CSV filename in JAC shared folder
# 40 W5G2 Day 2    Year 2  2019-10-02 24  # Matches CSV filename in JAC shared folder

# 41 W1G2 Day 2    Year 3  2019-05-06  1  # The collection date is correct, this mouse (OR13067-5807) was tested with the 35 month timepoint of the G23W1 DO mice in the longitudinal DO diet project with Calico  
# 42 W2G2 Day 2    Year 3  2019-10-01  1  # The collection date is correct, this mouse (OR13067-5821) was tested with the W5G2 Day 1 mice


REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    Shift = ifelse(
      DateCollect %in% as.Date(c('2018-01-16', '2018-01-17'), '%Y-%m-%d'),
      -1, 0
    ),
    DateCollect = DateCollect + days(Shift)
  ) %>% 
  select(-c(
    Shift
  )) %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, DOB
      ),
    by = 'MouseID'
  ) %>% 
  mutate(
    AgeInDays = as.numeric(difftime(DateCollect, DOB, units = 'days')),
    Timepoint = ifelse(
      AgeInDays/30.4 <= 13, 'Year 1',
      ifelse(
        AgeInDays/30.4 <= 25, 'Year 2',
        ifelse(
          AgeInDays/30.4 < 37, 'Year 3',
          'ERROR!'
        )
      )
    )
  ) %>% 
  select(-c(
    DOB
  )) %>% 
  select(
    OutputRowID:MouseID,
    Timepoint, AgeInDays,
    everything()
  )

REVISED_DATA %>% 
  group_by(Timepoint) %>% 
  summarise(
    Mean = mean(AgeInDays/30.4),
    Median = median(AgeInDays/30.4),
    Min = min(AgeInDays/30.4),
    Max = max(AgeInDays/30.4)
  ) %>% 
  data.frame()
#   Timepoint     Mean   Median       Min      Max
# 1    Year 1 11.37490 11.41447  9.210526 12.13816
# 2    Year 2 23.30804 23.35526 21.414474 24.21053
# 3    Year 3 34.70395 34.70395 33.585526 35.82237

#####


################################################################################
# Create days since fast column ####

# The 2-day fasting mice both have their food hoppers removed at noon on 
# Wednesday and returned at noon on Friday. Phenotyping generally happens in 
# the morning. Thus, if a IF mouse is phenotyped on a Wednesday it is not 
# in the fasting period, but if it is phenotyped on a Thursday or Friday it is.
# Thus, the "days since fast" for the IF mice for Monday through Saturday will
# be:
#   Monday  Tuesday  Wednesday  Thursday  Friday  Saturday  Sunday
#   3       4        5          0         0       1         2
# The days since fast for the ad libitum mice will be "NA", as will the days
# since fast for any measurements recorded before 6 months when the diet
# intervention started


REVISED_DATA$DateCollect %>% weekdays() %>% tab()
# Monday  Thursday   Tuesday Wednesday 
#    268       102       450       251 

WEEKDAYS <- c(
  'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'
)
IF2D <- c(
  3, 4, 5, 0, 0, 1, 2
)
names(IF2D) <- WEEKDAYS

REVISED_DATA <- REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, Diet
      ),
    by = 'MouseID'
  ) %>% 
  mutate(
    DaysSinceFast = ifelse(
      Diet == 'AL' | AgeInDays < 6*30.4, NA,
      ifelse(
        Diet == 'IF', IF2D[weekdays(DateCollect)],
        -999
      )
    )
  ) %>% 
  select(-c(
    Diet
  )) %>% 
  select(
    OutputRowID:DateCollect, 
    DaysSinceFast,
    everything()
  )

REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(MouseID, Diet),
    by = 'MouseID'
  ) %>% 
  select(Diet, DaysSinceFast) %>% 
  tab()
#     DaysSinceFast
# Diet   0   3   4   5 <NA>
#   AL   0   0   0   0  518
#   IF  51 138 233 131    0

rm(WEEKDAYS, IF2D)

#####


################################################################################
# Get weekly bodyweights ####

REVISED_DATA <- REVISED_DATA %>% 
  mutate(BW_Weekly = get_bw(MouseID, DateCollect)) %>% 
  select(OutputRowID:BW_Test, BW_Weekly, everything())
# No preceeding weekly BWs within 1 month of 2018-04-17 assay for IL16188-5056  # This mouse has a gap of missing (not lost due to cleaning, they are missing in the uncleaned weekly BW data) weekly bodyweights from mid february 2018 through mid may 2018, but it doesn't look like there was much of a change in BW during that time
# No following weekly BWs within 1 month of 2018-08-14 assay for IL16513-5324   # Mouse died shortly after CBC assay (actually, its death date matches the CBC date)
# No following weekly BWs within 1 month of 2018-09-24 assay for IL16750-5771   # Mouse died shortly after CBC assay
# No weekly BWs within 1 month of 2017-09-28 assay for OR3609-5926              # This mouse is missing (not lost due to cleaning, they are missing in the uncleaned weekly BW data) all of its weekly bodyweights before february 2018
# No following weekly BWs within 1 month of 2018-05-24 assay for OR3609-5952    # Mouse died shortly after CBC assay

ANIMAL_DATA %>% 
  filter(
    MouseID %in% 
      c(
        'IL16188-5056', 
        'IL16513-5324', 
        'IL16750-5771', 
        'OR3609-5926', 
        'OR3609-5952'
      )
  ) %>% 
  select(MouseID, Cohort, HID, DOB, DOE, COE) %>% 
  data.frame()
#        MouseID Cohort  HID        DOB        DOE             COE
# 1 IL16750-5771   W5G2 6498 2017-10-13 2018-09-26 Found Dead (FD)
# 2  OR3609-5952   W4G2 6143 2017-06-28 2018-05-25 Found Dead (FD)
# 3 IL16188-5056   W4G1 5974 2017-05-04 2018-09-18 Found Dead (FD)
# 4  OR3609-5926   W2G2 5130 2016-10-25 2019-03-25 Found Dead (FD)
# 5 IL16513-5324   W2G1 4875 2016-09-08 2018-08-14 Found Dead (FD)

#####


################################################################################
# Visualizeing swaps ####

REVISED_DATA %>% 
  select(
    BW_Test, BW_Weekly
  ) %>% 
  summary()
#      BW_Test        BW_Weekly    
# Min.   :15.19   Min.   :15.29  
# 1st Qu.:25.43   1st Qu.:23.99  
# Median :29.16   Median :27.89  
# Mean   :30.24   Mean   :29.15  
# 3rd Qu.:33.60   3rd Qu.:32.75  
# Max.   :53.75   Max.   :53.62  
# NA's   :1       NA's   :1    

DATA <- ANIMAL_DATA %>% 
  select(
    MouseID, JobGroup, HID
  ) %>% 
  right_join(
    REVISED_DATA %>% 
      select(
        MouseID, Timepoint, DateCollect, BW_Weekly, BW_Test
      ),
    by = 'MouseID'
  )

for(TP in sort(unique(DATA$Timepoint))){
  YEAR_DATA <- DATA %>% 
    filter(
      Timepoint == TP
    )
  pdf(
    paste0(
      'figures/bw_sample_swap_plots/',
      str_replace(TP, ' ', ''),
      '_',
      'bw_comparison_plots',
      '.pdf'
    ),
    width = 6.5, height = 6.0
  )
  
  for(JG in sort(unique(YEAR_DATA$JobGroup))){
    JOB_DATA <- YEAR_DATA %>% 
      filter(
        JobGroup == JG
      )
    for(DATE in sort(unique(as.character(JOB_DATA$DateCollect)))){
      DATE_DATA <- JOB_DATA %>% 
        filter(
          DateCollect == DATE
        ) %>% 
        mutate(
          HID = as.character(HID)
        )
      
      PLOT <- DATE_DATA %>% 
        ggplot() +
        theme_minimal() +
        geom_abline(
          slope = 1, intercept = 0,
          linetype = 3,
          color = 'gray50'
        ) +
        geom_point(
          aes(x = BW_Weekly, y = BW_Test, color = HID)
        ) +
        scale_color_manual(
          values = COLORS
        ) +
        scale_x_continuous(
          limits = c(15, 56),
          breaks = seq(0, 100, 6),
          minor_breaks = seq(0, 100, 2)
        ) +
        scale_y_continuous(
          limits = c(15, 56),
          breaks = seq(0, 100, 6),
          minor_breaks = seq(0, 100, 2)
        ) +
        labs(
          title = paste0(
            JG, ': ', DATE
          ),
          x = 'Weekly bodyweight (grams)',
          y = 'Bodyweight from CBC assay (grams)'
        )
      
      plot(PLOT)
      
      rm(DATE_DATA, PLOT)
    }
    rm(JOB_DATA, DATE)
  }
  
  dev.off()
  
  rm(YEAR_DATA, JG)
}
rm(TP)

rm(DATA)

#####


################################################################################
## Fixing swaps in year 1 data ###########################################

# Of the two mice that are missing either their test or weekly bodyweight, 
# neither have any housemates whose test and weekly bodyweights don't match.
ANIMAL_DATA %>% 
  select(MouseID, JobGroup, HID) %>% 
  right_join(
    REVISED_DATA %>% 
      filter(
        is.na(BW_Test) | is.na(BW_Weekly)
      ) %>% 
      select(MouseID, Timepoint, DateCollect, BW_Weekly, BW_Test)
  ) %>% 
  arrange(JobGroup) %>% 
  data.frame()
#        MouseID   JobGroup  HID Timepoint DateCollect BW_Weekly BW_Test
# 1  OR3609-5926 W2G2 Day 2 5130    Year 1  2017-09-28        NA   35.16
# 2 IL16188-5064 W5G1 Day 2 6409    Year 1  2018-09-05  33.52183      NA

REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, HID, EarNotch, Coat, DOE
      )
  ) %>% 
  filter(
    DateCollect == '2017-09-28',
    HID == 5129
  ) %>% 
  select(
    OutputRowID, MouseID, HID, EarNotch, Coat, BW_Test, BW_Weekly
  ) %>% 
  arrange(
    MouseID
  ) %>% 
  data.frame()
#   OutputRowID     MouseID  HID EarNotch  Coat BW_Test BW_Weekly
# 1       24774 OR3609-5916 5129        N other   30.80  30.13129
# 2       24776 OR3609-5917 5129        R other   27.90  18.30995 # Swap
# 3       24777 OR3609-5918 5129        L other   18.67  27.60307 # Swap
# 4       24778 OR3609-5919 5129        B other   26.93  25.55500


REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, HID, EarNotch, Coat, DOE
      )
  ) %>% 
  filter(
    DateCollect == '2018-09-24',
    HID == 6499
  ) %>% 
  select(
    OutputRowID, MouseID, HID, EarNotch, Coat, BW_Test, BW_Weekly
  ) %>% 
  arrange(
    MouseID
  ) %>% 
  data.frame()
#   OutputRowID      MouseID  HID EarNotch Coat BW_Test BW_Weekly
# 1      693483 IL16750-5777 6499        R <NA>   23.59  33.20347 # Swap
# 2      693484 IL16750-5778 6499        L <NA>   33.77  23.50459 # Swap
# 3      693485 IL16750-5779 6499        B <NA>   20.78  26.53676

REVISED_DATA %>% 
  filter(
    OutputRowID %in% c(24776, 24777, 693483, 693484)
  ) %>% 
  write_csv(
    'issues/sample_swaps.csv'
  )

REVISED_DATA$MouseID[REVISED_DATA$OutputRowID == '24776'] <- 'OR3609-5918'
REVISED_DATA$MouseID[REVISED_DATA$OutputRowID == '24777'] <- 'OR3609-5917'
REVISED_DATA$MouseID[REVISED_DATA$OutputRowID == '693483'] <- 'IL16750-5778'
REVISED_DATA$MouseID[REVISED_DATA$OutputRowID == '693484'] <- 'IL16750-5777'

#####


################################################################################
## Fixing swaps in year 2 data ###########################################

# No swaps identified


#####


################################################################################
## Fixing swaps in year 3 data ###########################################

# No swaps identified


#####


################################################################################
## Re-do ages, timepoints, and weekly bodyweights after fixing swaps ##########

REVISED_DATA <- REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, DOB
      ),
    by = 'MouseID'
  ) %>% 
  mutate(
    AgeInDays = as.numeric(difftime(DateCollect, DOB, units = 'days')),
    Timepoint = ifelse(
      AgeInDays/30.4 <= 13, 'Year 1',
      ifelse(
        AgeInDays/30.4 <= 25, 'Year 2',
        ifelse(
          AgeInDays/30.4 < 37, 'Year 3',
          'ERROR!'
        )
      )
    )
  ) %>% 
  select(-c(
    DOB
  )) %>% 
  select(
    OutputRowID:MouseID,
    Timepoint, AgeInDays,
    everything()
  )

REVISED_DATA <- REVISED_DATA %>% 
  mutate(BW_Weekly = get_bw(MouseID, DateCollect)) %>% 
  select(OutputRowID:BW_Test, BW_Weekly, everything())
# No preceeding weekly BWs within 1 month of 2018-04-17 assay for IL16188-5056  # This mouse has a gap of missing (not lost due to cleaning, they are missing in the uncleaned weekly BW data) weekly bodyweights from mid february 2018 through mid may 2018, but it doesn't look like there was much of a change in BW during that time
# No following weekly BWs within 1 month of 2018-08-14 assay for IL16513-5324   # Mouse died shortly after CBC assay (actually, its death date matches the CBC date)
# No following weekly BWs within 1 month of 2018-09-24 assay for IL16750-5771   # Mouse died shortly after CBC assay
# No weekly BWs within 1 month of 2017-09-28 assay for OR3609-5926              # This mouse is missing (not lost due to cleaning, they are missing in the uncleaned weekly BW data) all of its weekly bodyweights before february 2018
# No following weekly BWs within 1 month of 2018-05-24 assay for OR3609-5952    # Mouse died shortly after CBC assay


# Check that swaps are fixed:

DATA <- ANIMAL_DATA %>% 
  select(
    MouseID, JobGroup, HID
  ) %>% 
  right_join(
    REVISED_DATA %>% 
      select(
        MouseID, Timepoint, DateCollect, BW_Weekly, BW_Test
      ),
    by = 'MouseID'
  )

for(TP in sort(unique(DATA$Timepoint))){
  YEAR_DATA <- DATA %>% 
    filter(
      Timepoint == TP
    )
  pdf(
    paste0(
      'figures/bw_sample_swap_plots/',
      str_replace(TP, ' ', ''),
      '_',
      'bw_comparison_plots',
      '_fixed.pdf'
    ),
    width = 6.5, height = 6.0
  )
  
  for(JG in sort(unique(YEAR_DATA$JobGroup))){
    JOB_DATA <- YEAR_DATA %>% 
      filter(
        JobGroup == JG
      )
    for(DATE in sort(unique(as.character(JOB_DATA$DateCollect)))){
      DATE_DATA <- JOB_DATA %>% 
        filter(
          DateCollect == DATE
        ) %>% 
        mutate(
          HID = as.character(HID)
        )
      
      PLOT <- DATE_DATA %>% 
        ggplot() +
        theme_minimal() +
        geom_abline(
          slope = 1, intercept = 0,
          linetype = 3,
          color = 'gray50'
        ) +
        geom_point(
          aes(x = BW_Weekly, y = BW_Test, color = HID)
        ) +
        scale_color_manual(
          values = COLORS
        ) +
        scale_x_continuous(
          limits = c(15, 56),
          breaks = seq(0, 100, 6),
          minor_breaks = seq(0, 100, 2)
        ) +
        scale_y_continuous(
          limits = c(15, 56),
          breaks = seq(0, 100, 6),
          minor_breaks = seq(0, 100, 2)
        ) +
        labs(
          title = paste0(
            JG, ': ', DATE
          ),
          x = 'Weekly bodyweight (grams)',
          y = 'Bodyweight from CBC assay (grams)'
        )
      
      plot(PLOT)
      
      rm(DATE_DATA, PLOT)
    }
    rm(JOB_DATA, DATE)
  }
  
  dev.off()
  
  rm(YEAR_DATA, JG)
}
rm(TP)

rm(DATA)


#####


################################################################################
##### Plot histograms of phenotypes #####

PHENOTYPES <- names(REVISED_DATA)[16:48]

pdf(
  'figures/histograms_of_phenotypes_and_transformations.pdf',
  width = 7, height = 4 + (length(PHENOTYPES) - 1)*1.5
)
par(
  mfrow = c(length(PHENOTYPES), 2)
)
for(PHENO in PHENOTYPES){
  X <- REVISED_DATA[[PHENO]]
  XTF <- REVISED_DATA[[PHENO]]
  
  TRANSFORM <- 'none'
  if(tolower(substr(PHENO, 1, 4)) == 'perc'){
    TRANSFORM <- 'logit'
    XTF <- logit_of_perc(XTF)
  } else if(skewness(X, na.rm = TRUE) > 1){
    TRANSFORM <- 'log'
    XTF <- log_tf(XTF)
  }
  
  hist(
    X,
    main = paste0(
      PHENO, 
      ', skewness = ', 
      signif(skewness(X, na.rm = TRUE), 3), 
      ', kurtosis = ', 
      signif(skewness(X, na.rm = TRUE), 3)
    ),
    xlab = NULL,
    breaks = 100
  )
  
  
  
  hist(
    XTF,
    main = paste0(
      PHENO, ', ', 
      ifelse(
        TRANSFORM == 'none', 'no transform',
        paste0(TRANSFORM, ' transform')
      )
    ),
    xlab = NULL,
    breaks = 100
  )
  
  rm(X, XTF, TRANSFORM)
}
dev.off()

rm(PHENO, PHENOTYPES)

#####


################################################################################
## calculate derived measures ###########################################

REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    NumRetic = NumRetic/1000,                   # Convert NumRetic (10^3 cells / uL) to the same units as RBC (10^6 cells / uL)
    RDWsd = MCV*RDWcv/100,
    HDWcv = HDWsd/CHCM*100,
    NLR = NumNeut/NumLymph,
    PDWsd = MPV*PDWcv/100
  ) %>% 
  select(
    OutputRowID:Anesthesia,
    NumClumps,
    NumRBC, NumRetic, PercRetic, Hct, Hgb, CalcHgb, 
    MCV, CHCM, MCHC, CH, MCH, CHm, CHr, RDWcv, RDWsd, HDWcv, HDWsd,
    NumWBC, NumLymph, NumNeut, NumMono, NumEos, NumBaso, NumLUC,
    NLR, PercLymph, PercNeut, PercMono, PercEos, PercBaso, PercLUC,
    NumPlt, MPV, MPM, PDWcv, PDWsd, 
    everything()
  )

#####


################################################################################
# Save data ####

REVISED_DATA %>% 
  select(-c(TaskID, HID_Test, Coat_Test, EN_Test
  )) %>% 
  select(
    MouseID, OutputRowID,
    everything(),
  ) %>% 
  arrange(
    substr(MouseID, nchar(MouseID) - 3, 999),
    DateCollect
  ) %>% 
  write_csv('data/Cleaned_CBC_20210719.csv')
# # A tibble: 1,071 x 48
#    MouseID OutputRowID Timepoint AgeInDays DateCollect DaysSinceFast Tech  Comments BW_Test BW_Weekly Anesthesia NumClumps NumRBC NumRetic PercRetic   Hct   Hgb CalcHgb   MCV  CHCM  MCHC    CH   MCH   CHm   CHr RDWcv RDWsd HDWcv HDWsd NumWBC NumLymph NumNeut
#    <chr>         <int> <chr>         <dbl> <date>              <dbl> <chr> <chr>      <dbl>     <dbl> <chr>          <dbl>  <dbl>    <dbl>     <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>   <dbl>
#  1 IL1618…     1252746 Year 1          338 2017-03-28             NA John  Batch c…    40.0      40.0 proparaca…      1241   9.31    0.404      4.34  44.9  14.9    13.7  48.3  30.5  33    14.7  16    NA    16.4  14.6  7.05  6.75  2.06   3.15     1.9     0.62
#  2 IL1618…     1251082 Year 2          703 2018-03-28             NA Rach… Batch c…    44.4      42.0 proparaca…        60   4.3     0.861     20.0   26.7   8.1     7.6  62.1  28.3  30.1  17.5  18.7  17.9  18.1  19.7 12.2   9.89  2.8   12.6      2.88    8.97
#  3 IL1618…     1252747 Year 1          338 2017-03-28             NA John  Batch c…    31.5      30.4 proparaca…      1516   9.77    0.481      4.92  46.6  15.3    14    47.7  30.1  32.9  14.3  15.7  NA    16.4  15.2  7.25  6.78  2.04   3.72     1.87    0.86
#  4 IL1618…     1252748 Year 1          338 2017-03-28             NA John  Batch c…    26.7      28.3 proparaca…       419   9.91    0.379      3.83  47.6  15.5    14.2  48    29.9  32.6  14.3  15.7  NA    16.1  14    6.72  6.66  1.99   1.67     1.02    0.41
#  5 IL1618…     1251083 Year 2          703 2018-03-28             NA Rach… Batch c…    32.2      32.2 proparaca…        88   7.91    0.461      5.82  39.4  12.3    11.8  49.8  29.8  31.1  14.8  15.5  15.2  17.4  19.5  9.71  7.82  2.33   8.72     1.65    6.54
#  6 IL1618…     1252749 Year 1          337 2017-03-28             NA John  Batch c…    43.5      45.9 proparaca…      1289  10.2     0.177      1.73  48.7  16.1    14.8  47.6  30.3  32.9  14.4  15.7  NA    15.5  13.8  6.57  6.57  1.99   2.26     0.8     0.63
#  7 IL1618…     1252750 Year 1          337 2017-03-28              4 John  Batch c…    24.3      24.4 proparaca…      1045   8.98    0.555      6.18  44.6  14.4    13.6  49.7  30.4  32.2  15    16    NA    16.5  17.5  8.70  7.37  2.24   3.53     2.03    0.97
#  8 IL1618…     1252751 Year 1          337 2017-03-28              4 John  Batch c…    23.4      21.6 proparaca…       608   9.54    0.359      3.77  45.8  15.1    14    48    30.5  32.9  14.6  15.8  NA    17.4  16.2  7.78  6.52  1.99   1.99     0.96    0.57
#  9 IL1618…     1252752 Year 1          337 2017-03-28              4 John  Batch c…    31.4      29.6 proparaca…      1459   9.57    0.410      4.29  47    15.1    13.7  49.2  29.2  32.1  14.3  15.8  NA    17.1  16.4  8.07  6.68  1.95   2.04     1.04    0.52
# 10 IL1618…     1251084 Year 2          702 2018-03-28              5 Rach… Batch c…    30.0      28.6 proparaca…      1809   9.26    0.278      3     43    14.4    12.9  46.4  30    33.6  13.9  15.6  14.3  15.8  12.9  5.99  6.23  1.87   2.37     0.54    0.89
# # … with 1,061 more rows, and 16 more variables: NumMono <dbl>, NumEos <dbl>, NumBaso <dbl>, NumLUC <dbl>, NLR <dbl>, PercLymph <dbl>, PercNeut <dbl>, PercMono <dbl>, PercEos <dbl>, PercBaso <dbl>, PercLUC <dbl>, NumPlt <dbl>, MPV <dbl>, MPM <dbl>,
# #   PDWcv <dbl>, PDWsd <dbl>

#####


################################################################################
## clear workspace ###########################################

rm(list = ls())
pacman::p_unload('all')
graphics.off()

#####
