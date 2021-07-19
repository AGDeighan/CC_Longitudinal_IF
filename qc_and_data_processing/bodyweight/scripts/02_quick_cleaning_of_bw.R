
setwd('/Users/deigha/Documents/Projects/CC_Longitudinal_IF/qc_and_data_processing/bodyweight/')

################################################################################
# Load libraries etc ####

options(stringsAsFactors = FALSE)
library(lubridate)
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
  'data/FORMATTED_BW_20210719.csv',
  col_types = 'iccccDDDn'
)
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

ANIMAL_DATA <- read_csv(
  'data/animal_data_processed_20210719.csv'
) %>% 
  mutate(
    Sex = factor(Sex, levels = c('Female', 'Male')),
    Diet = factor(Diet, levels = c('AL', 'IF'))
  )
# # A tibble: 800 x 19
#    MouseID      Diet  Strain        StrainAKA Sex    Generation Cohort JobGroup   BWDay       HID  LHID EarNotch Coat  AnimalComments                                            DOB        DOE        COE             SurvDays Died 
#    <chr>        <fct> <chr>         <chr>     <fct>  <chr>      <chr>  <chr>      <chr>     <dbl> <dbl> <chr>    <chr> <chr>                                                     <date>     <date>     <chr>              <dbl> <lgl>
#  1 OR3609-5979  IF    CC018/UncJ    OR3609    Male   F06        W5G2   W5G2 Day 2 Wednesday  6598  6598 NA       NA    "REN"                                                     2017-11-08 2019-03-11 Found Dead (FD)      488 TRUE 
#  2 OR3609-5978  IF    CC018/UncJ    OR3609    Male   F06        W5G2   W5G2 Day 2 Wednesday  6598  6598 NA       NA    "found dead during wheel running testing 9/22/19\n"       2017-11-08 2019-09-22 Found Dead (FD)      683 TRUE 
#  3 IL16557-5478 IF    CC040/TauUncJ IL16557   Male   P01        W5G1   W5G1 Day 1 Wednesday  6573  6573 L        Other "no right eye"                                            2017-09-26 2020-02-01 Found Dead (FD)      858 TRUE 
#  4 IL16557-5479 IF    CC040/TauUncJ IL16557   Male   P01        W5G1   W5G1 Day 1 Wednesday  6573  6573 B        Other  NA                                                       2017-09-26 2019-12-18 Found Dead (FD)      813 TRUE 
#  5 IL16557-5471 IF    CC040/TauUncJ IL16557   Female P01        W5G1   W5G1 Day 1 Wednesday  6569  6569 B        Other "Abdomen slighty distended 8/29/18"                       2017-09-26 2018-08-31 Found Dead (FD)      339 TRUE 
#  6 IL16557-5472 AL    CC040/TauUncJ IL16557   Male   P01        W5G1   W5G1 Day 1 Wednesday  6570  6570 N        Other "4/17/19 mouse dropped 7 grams reweighed to double check" 2017-09-11 2019-04-22 Found Dead (FD)      588 TRUE 
#  7 IL16557-5473 AL    CC040/TauUncJ IL16557   Male   P01        W5G1   W5G1 Day 1 Wednesday  6570  6570 R        Other  NA                                                       2017-09-11 2019-01-14 Found Dead (FD)      490 TRUE 
#  8 IL16557-5474 AL    CC040/TauUncJ IL16557   Male   P01        W5G1   W5G1 Day 1 Wednesday  6570  6570 L        Other  NA                                                       2017-09-11 2019-04-22 Found Dead (FD)      588 TRUE 
#  9 IL16557-5475 AL    CC040/TauUncJ IL16557   Male   P01        W5G1   W5G1 Day 1 Wednesday  6571  6571 N        Other "Left Eye Damage 4/25/18"                                 2017-09-26 2019-01-21 Found Dead (FD)      482 TRUE 
# 10 IL16557-5476 IF    CC040/TauUncJ IL16557   Male   P01        W5G1   W5G1 Day 1 Wednesday  6573  6573 N        Other  NA                                                       2017-09-26 2019-04-29 Found Dead (FD)      580 TRUE 
# # … with 790 more rows

#####


################################################################################
# Create BW plots with uncleaned data #### 

DATA <- ANIMAL_DATA %>% 
  select(
    MouseID, Diet, Strain, Sex, Cohort, BWDay, HID, DOB, DOE, COE,
  ) %>% 
  right_join(
    REVISED_DATA %>% 
      select(
        MouseID, DateCollect, BW
      ),
    by = 'MouseID'
  )

for(COHORT in sort(unique(DATA$Cohort))){
  COHORT_DATA <- DATA %>% 
    filter(
      Cohort == COHORT
    )
  
  pdf(
    paste0(
      'figures/uncleaned_bw_plots/',
      COHORT, 
      '_uncleaned_weekly_bw_by_cage.pdf'
    ),
    width = 14,
    height = 6
  )
  for(H in sort(unique(COHORT_DATA$HID))){
    PLOT_DATA <- COHORT_DATA %>% 
      filter(
        HID == H
      )
    
    PLOT <- PLOT_DATA %>% 
      ggplot() +
      theme_minimal(
        base_size = 9
      ) +
      geom_line(
        aes(x = DateCollect, y = BW, color = MouseID, group = MouseID),
        alpha = 0.8
      ) +
      geom_point(
        aes(x = DateCollect, y = BW, color = MouseID),
        alpha = 0.5
      ) +
      geom_point(
        data = PLOT_DATA %>% 
          group_by(
            MouseID, DOB
          ) %>% 
          summarise(
            BW = 0
          ),
        aes(x = DOB, y = BW, color = MouseID),
        alpha = 0.5,
        shape = 15
      ) +
      geom_text(
        data = PLOT_DATA %>% 
          arrange(
            MouseID, DateCollect
          ) %>% 
          group_by(
            MouseID, DOE, COE
          ) %>% 
          summarise(
            BW = BW[n()]
          ),
        aes(x = DOE, y = BW, color = MouseID, label = COE),
        alpha = 1,
        size = 9 * 5/14,
        hjust = 0
      ) +
      scale_x_date(
        date_breaks = 'months',
        date_minor_breaks = 'weeks',
        date_labels = '%y %b',
        limits = c(
          min(PLOT_DATA$DOB, na.rm = TRUE) - weeks(1),
          max(c(PLOT_DATA$DOE, PLOT_DATA$DateCollect), na.rm = TRUE) + months(2)
        ),
        exp = c(0,0)
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, 12),
        minor_breaks = seq(0, 100, 3),
        limits = c(0, 70),
        exp = c(0,0)
      ) +
      scale_color_manual(
        values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
      ) +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(
          angle = 45, 
          vjust = 1, 
          hjust = 1
        )
      ) +
      labs(
        title = paste0('Uncleaned weekly bodyweights for pen ', H),
        subtitle = paste0(
          unique(PLOT_DATA$Diet), ', ',
          COHORT, ', ', 
          unique(PLOT_DATA$Strain), ', ',
          unique(PLOT_DATA$Sex), ', ',
          unique(PLOT_DATA$BWDay), ' bodyweights'
        ),
        x = 'Collection date',
        y = 'Weight (grams)',
        color = 'Mouse',
        caption = 'The small rectangle at lower left indicates the birthdate for each mouse'
      )
    
    plot(PLOT)
    
    rm(PLOT_DATA, PLOT)
  }
  rm(H)
  dev.off()
}

rm(COHORT)

rm(DATA)
#####


################################################################################
# Missing dates ####

# No records are missing either their due date or collection date. Most records
# are missing their completed by date (I don't know what this field is, it is
# new). There are some differences of more than a week between the completion
# date and collection date, we'll have to look into this.

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
# [1] 46071

REVISED_DATA %>% 
  filter(
    !is.na(DateComplete)
  ) %>% 
  nrow()
# [1] 22583


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
  filter(
    !is.na(DateComplete)
  ) %>% 
  mutate(
    DateDiff = as.numeric(difftime(DateCollect, DateComplete, units = 'days'))
  ) %>% 
  select(BWDay_Test, DateDiff) %>% tab()
#         DateDiff
# BWDay_Test   -12   -7   -4   -3   -2   -1    0    2
#   Friday       0    0   43    0    0    0 7937    0
#   Thursday     0    0    0   43    0    8 1109    0
#   Tuesday      0   86    0    0    0  215 5252    0
#   Wednesday  138    0    0    0   51  504 7165   32

#####


################################################################################
# Remove records missing both mouse ID and BW ####

REVISED_DATA %>% 
  filter(
    is.na(MouseID),
    is.na(BW)
  ) %>% 
  nrow()
# [1] 143

REVISED_DATA %>% 
  filter(
    is.na(MouseID),
    is.na(BW)
  ) %>% 
  write_csv(
    'issues/records_missing_mouseid_and_bw.csv'
  )

REVISED_DATA <- REVISED_DATA %>% 
  filter(
    !is.na(MouseID) | !is.na(BW)
  )

#####


################################################################################
# Records missing mouse ID only ####

REVISED_DATA %>% 
  filter(
    is.na(MouseID)
  ) %>% 
  nrow()
# [1] 44

REVISED_DATA %>% 
  filter(
    is.na(MouseID)
  ) %>% 
  write_csv(
    'issues/records_missing_mouseid_not_bw.csv'
  )

REVISED_DATA <- REVISED_DATA %>% 
  filter(
    !is.na(MouseID)
  )

#####


################################################################################
# Records missing BW only ####

REVISED_DATA %>% 
  filter(
    is.na(BW)
  ) %>% 
  nrow()
# [1] 19

REVISED_DATA %>% 
  filter(
    is.na(BW)
  ) %>% 
  write_csv(
    'issues/records_missing_bw_not_mouseid.csv'
  )

REVISED_DATA <- REVISED_DATA %>% 
  filter(
    !is.na(BW)
  )

#####


################################################################################
# Remove complete duplications ####

# There are eight copies of the record with output row ID 1121173, we only want
# to keep one of these copies

REVISED_DATA[
  (
    duplicated(
      REVISED_DATA[,c('MouseID', 'DateCollect', 'BW')]
    ) |
      duplicated(
        REVISED_DATA[,c('MouseID', 'DateCollect', 'BW')],
        fromLast = TRUE
      )
  ),
]
# # A tibble: 8 x 9
#    OutputRowID MouseID      BWDay_Test Tech   BatchComments DateDue    DateCollect DateComplete    BW
#          <int> <chr>        <chr>      <chr>  <chr>         <date>     <date>      <date>       <dbl>
#  1     1121173 IL16513-5341 Friday     Amanda NA            2019-06-21 2019-06-21  2019-06-21    21.5
#  2     1121173 IL16513-5341 Friday     Amanda NA            2019-06-21 2019-06-21  2019-06-21    21.5
#  3     1121173 IL16513-5341 Friday     Amanda NA            2019-06-21 2019-06-21  2019-06-21    21.5
#  4     1121173 IL16513-5341 Friday     Amanda NA            2019-06-21 2019-06-21  2019-06-21    21.5
#  5     1121173 IL16513-5341 Friday     Amanda NA            2019-06-21 2019-06-21  2019-06-21    21.5
#  6     1121173 IL16513-5341 Friday     Amanda NA            2019-06-21 2019-06-21  2019-06-21    21.5
#  7     1121173 IL16513-5341 Friday     Amanda NA            2019-06-21 2019-06-21  2019-06-21    21.5
#  8     1121173 IL16513-5341 Friday     Amanda NA            2019-06-21 2019-06-21  2019-06-21    21.5

REVISED_DATA[
  (
    duplicated(
      REVISED_DATA[,c('MouseID', 'DateCollect', 'BW')]
    ) |
      duplicated(
        REVISED_DATA[,c('MouseID', 'DateCollect', 'BW')],
        fromLast = TRUE
      )
  ),
] %>% 
  write_csv(
    'issues/full_duplicates.csv'
  )


which(REVISED_DATA$OutputRowID == '1121173')
# [1] 68154 68155 68156 68157 68158 68159 68160 68161

REVISED_DATA <- REVISED_DATA[
  -which(REVISED_DATA$OutputRowID == '1121173')[-1],
]

#####


################################################################################
# Multiple records for one date ####

# There are 178 records (89 duplications, all are pairs, no triplicates or 
# higher level duplications) involved in duplications in which the collection 
# date and mouse ID are identical, but the body weights are not. For many of 
# these, the bodyweights are very similar (perhaps the mice were weighed 
# twice?), but for some they are quite different. The due dates are also 
# identical and for all of these duplicates that have completion dates, the 
# completion dates are also identical, so that doesn't help. They don't all 
# come from one date, though there is one date (2018-11-14) that has a large
# amount of duplicates (12 recods, 6 duplicates). I'm not sure what to do here.
# It seems reasonable that the pairs with two bodyweights that are not very 
# different represent mice that were weighed twice and for those we could
# simply take the mean. However, there are numerous pairs for which the 
# bodyweights differ. Furthermore, how do we define what pairs are have
# bodyweights that are "close" enough? Often, different mice within the same
# cage have similar bodyweights. Until we decide what to do, I am dropping all
# these records involved in duplications.

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
  arrange(
    MouseID, DateCollect, DateDue, DateComplete
  ) %>% 
  data.frame()
#     OutputRowID      MouseID BWDay_Test      Tech                                                                                                                 BatchComments    DateDue DateCollect DateComplete    BW
# 1        231646  AU8002-5513   Thursday Mackenzie                                                                                                                          <NA> 2018-04-19  2018-04-19         <NA> 29.44
# 2        231650  AU8002-5513   Thursday Mackenzie                                                                                                                          <NA> 2018-04-19  2018-04-19         <NA> 29.45
# 3        493224  AU8002-5529     Friday Mackenzie                                                                                                                          <NA> 2018-05-04  2018-05-04         <NA> 30.61
# 4        493227  AU8002-5529     Friday Mackenzie                                                                                                                          <NA> 2018-05-04  2018-05-04         <NA> 33.61
# 5        688837  AU8002-5545     Friday Mackenzie                                                                                                                          <NA> 2018-09-21  2018-09-21         <NA> 24.77
# 6        688838  AU8002-5545     Friday Mackenzie                                                                                                                          <NA> 2018-09-21  2018-09-21         <NA> 24.82
# 7        886641  AU8002-5546     Friday    Farrar                                                                                                                          <NA> 2019-01-25  2019-01-25   2019-01-25 24.50
# 8        886586  AU8002-5546     Friday Mackenzie                                                                                                                          <NA> 2019-01-25  2019-01-25   2019-01-25 25.70
# 9        688839  AU8002-5547     Friday Mackenzie                                                                                                                          <NA> 2018-09-21  2018-09-21         <NA> 27.52
# 10       688843  AU8002-5547     Friday Mackenzie                                                                                                                          <NA> 2018-09-21  2018-09-21         <NA> 27.63
# 11       232712  AU8002-5552    Tuesday    Amanda                                                                                                                          <NA> 2018-04-24  2018-04-24         <NA> 29.51
# 12       232716  AU8002-5552    Tuesday    Amanda                                                                                                                          <NA> 2018-04-24  2018-04-24         <NA> 29.23
# 13       232715  AU8002-5553    Tuesday    Amanda                                                                                                                          <NA> 2018-04-24  2018-04-24         <NA> 23.11
# 14       232711  AU8002-5553    Tuesday    Amanda                                                                                                                          <NA> 2018-04-24  2018-04-24         <NA> 23.01
# 15       232714  AU8002-5554    Tuesday    Amanda                                                                                                                          <NA> 2018-04-24  2018-04-24         <NA> 30.03
# 16       232710  AU8002-5554    Tuesday    Amanda                                                                                                                          <NA> 2018-04-24  2018-04-24         <NA> 30.12
# 17       232709  AU8002-5555    Tuesday    Amanda                                                                                                                          <NA> 2018-04-24  2018-04-24         <NA> 28.46
# 18       232713  AU8002-5555    Tuesday    Amanda                                                                                                                          <NA> 2018-04-24  2018-04-24         <NA> 28.45
# 19       686356  AU8002-5571  Wednesday Mackenzie                                                                                                                          <NA> 2018-09-19  2018-09-19   2018-09-19 32.81
# 20       686354  AU8002-5571  Wednesday Mackenzie                                                                                                                          <NA> 2018-09-19  2018-09-19   2018-09-19 30.41
# 21       700923  AU8002-5571  Wednesday Mackenzie                                                                                                                          <NA> 2018-10-24  2018-10-24   2018-10-24 31.60
# 22       700902  AU8002-5571  Wednesday Mackenzie                                                                                                                          <NA> 2018-10-24  2018-10-24   2018-10-24 26.54
# 23       707232  AU8002-5571  Wednesday    Farrar                                                                                                                          <NA> 2018-11-21  2018-11-23   2018-11-23 31.80
# 24       707230  AU8002-5571  Wednesday    Farrar                                                                                                                          <NA> 2018-11-21  2018-11-23   2018-11-23 29.56
# 25       708208  AU8002-5571  Wednesday    Farrar                                                                                                                          <NA> 2018-11-28  2018-11-28   2018-11-28 30.31
# 26       708216  AU8002-5571  Wednesday    Farrar                                                                                                                          <NA> 2018-11-28  2018-11-28   2018-11-28 32.82
# 27       141995  AU8048-5612   Thursday Mackenzie                                                                                                                          <NA> 2018-03-08  2018-03-09         <NA> 27.28
# 28       141996  AU8048-5612   Thursday Mackenzie                                                                                                                          <NA> 2018-03-08  2018-03-09         <NA> 25.57
# 29       501165  AU8048-5617     Friday Mackenzie                                                                                                                          <NA> 2018-06-01  2018-06-01   2018-06-01 23.72
# 30       501175  AU8048-5617     Friday Mackenzie                                                                                                                          <NA> 2018-06-01  2018-06-01   2018-06-01 23.69
# 31       499909  AU8048-5639     Friday Mackenzie                                                                                                                          <NA> 2018-05-25  2018-05-25         <NA> 20.22
# 32       499918  AU8048-5639     Friday Mackenzie                                                                                                                          <NA> 2018-05-25  2018-05-25         <NA> 20.20
# 33       708986  AU8048-5642     Friday Mackenzie                                                                                                                          <NA> 2018-11-30  2018-11-30   2018-11-30 27.82
# 34       708987  AU8048-5642     Friday Mackenzie                                                                                                                          <NA> 2018-11-30  2018-11-30   2018-11-30 27.40
# 35       596950  AU8048-5665  Wednesday    Farrar                                                                                                                          <NA> 2018-08-15  2018-08-15   2018-08-15 24.13
# 36       596951  AU8048-5665  Wednesday    Farrar                                                                                                                          <NA> 2018-08-15  2018-08-15   2018-08-15 24.11
# 37       492796  AU8048-5670  Wednesday Mackenzie                                                                                                                          <NA> 2018-05-02  2018-05-02         <NA> 25.78
# 38       492797  AU8048-5670  Wednesday Mackenzie                                                                                                                          <NA> 2018-05-02  2018-05-02         <NA> 19.09
# 39       492799  AU8048-5672  Wednesday Mackenzie                                                                                                                          <NA> 2018-05-02  2018-05-02         <NA> 31.46
# 40       492852  AU8048-5672  Wednesday Mackenzie                                                                                                                          <NA> 2018-05-02  2018-05-02         <NA> 33.22
# 41       502452 IL16188-5030     Friday Mackenzie                                                                                                                          <NA> 2018-06-08  2018-06-08         <NA> 31.34
# 42       502453 IL16188-5030     Friday Mackenzie                                                                                                                          <NA> 2018-06-08  2018-06-08         <NA> 36.82
# 43       502476 IL16188-5037     Friday Mackenzie                                                                                                                          <NA> 2018-06-08  2018-06-08         <NA> 32.20
# 44       502479 IL16188-5037     Friday Mackenzie                                                                                                                          <NA> 2018-06-08  2018-06-08         <NA> 32.16
# 45       558360 IL16188-5038     Friday    Farrar                                                                                                                          <NA> 2018-07-27  2018-07-27   2018-07-27 29.04
# 46       558357 IL16188-5038     Friday    Farrar                                                                                                                          <NA> 2018-07-27  2018-07-27   2018-07-27 42.55
# 47       922702 IL16188-5038     Friday    Farrar                                                                                                                          <NA> 2019-03-01  2019-03-01   2019-03-01 27.93
# 48       922703 IL16188-5038     Friday    Farrar                                                                                                                          <NA> 2019-03-01  2019-03-01   2019-03-01 34.13
# 49       600318 IL16188-5040     Friday Mackenzie                                                                                                                          <NA> 2018-08-24  2018-08-24         <NA> 39.73
# 50       600459 IL16188-5040     Friday Mackenzie                                                                                                                          <NA> 2018-08-24  2018-08-24         <NA> 39.61
# 51       600460 IL16188-5043     Friday Mackenzie                                                                                                                          <NA> 2018-08-24  2018-08-24         <NA> 49.51
# 52       600317 IL16188-5043     Friday Mackenzie                                                                                                                          <NA> 2018-08-24  2018-08-24         <NA> 49.68
# 53       600320 IL16188-5045     Friday Mackenzie                                                                                                                          <NA> 2018-08-24  2018-08-24         <NA> 35.61
# 54       600461 IL16188-5045     Friday Mackenzie                                                                                                                          <NA> 2018-08-24  2018-08-24         <NA> 36.57
# 55       600319 IL16188-5047     Friday Mackenzie                                                                                                                          <NA> 2018-08-24  2018-08-24         <NA> 37.43
# 56       600462 IL16188-5047     Friday Mackenzie                                                                                                                          <NA> 2018-08-24  2018-08-24         <NA> 38.60
# 57       231036 IL16188-5049    Tuesday    Farrar                                                                                                                          <NA> 2018-04-17  2018-04-17         <NA> 34.93
# 58       231028 IL16188-5049    Tuesday    Farrar                                                                                                                          <NA> 2018-04-17  2018-04-17         <NA> 35.20
# 59       691948 IL16188-5049    Tuesday     Gaven                                                                                                        weighed in the morning 2018-10-02  2018-10-02   2018-10-02 27.67
# 60       691943 IL16188-5049    Tuesday     Gaven                                                                                                        weighed in the morning 2018-10-02  2018-10-02   2018-10-02 42.63
# 61       691946 IL16188-5051    Tuesday     Gaven                                                                                                        weighed in the morning 2018-10-02  2018-10-02   2018-10-02 45.02
# 62       692052 IL16188-5051    Tuesday     Gaven                                                                                                        weighed in the morning 2018-10-02  2018-10-02   2018-10-02 44.70
# 63       698094 IL16188-5055    Tuesday Mackenzie                                                                                                                          <NA> 2018-10-16  2018-10-16   2018-10-16 39.03
# 64       698045 IL16188-5055    Tuesday Mackenzie                                                                                                                          <NA> 2018-10-16  2018-10-16   2018-10-16 20.97
# 65       226943 IL16188-5064  Wednesday     Gaven                                                                                                                          <NA> 2018-03-28  2018-03-28   2018-03-28 37.16
# 66       226979 IL16188-5064  Wednesday     Gaven                                                                                                                          <NA> 2018-03-28  2018-03-28   2018-03-28 25.21
# 67       705374 IL16188-5064  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 35.98
# 68       705466 IL16188-5064  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 35.65
# 69       707054 IL16188-5064  Wednesday    Farrar                                                                                                                          <NA> 2018-11-21  2018-11-23   2018-11-23 37.03
# 70       707196 IL16188-5064  Wednesday    Farrar                                                                                                                          <NA> 2018-11-21  2018-11-23   2018-11-23 26.41
# 71       705375 IL16188-5065  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 23.14
# 72       705480 IL16188-5065  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 23.05
# 73       705481 IL16188-5067  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 29.09
# 74       705376 IL16188-5067  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 29.49
# 75       705379 IL16188-5068  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 24.65
# 76       705482 IL16188-5068  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 24.56
# 77       705488 IL16188-5069  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 29.29
# 78       705377 IL16188-5069  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 29.33
# 79       705378 IL16188-5070  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 36.03
# 80       705489 IL16188-5070  Wednesday Mackenzie                                                                                                                          <NA> 2018-11-14  2018-11-14         <NA> 35.55
# 81       582942 IL16188-5074  Wednesday    Amanda                                                                                                                          <NA> 2018-08-08  2018-08-08   2018-08-08 45.59
# 82       582943 IL16188-5074  Wednesday    Amanda                                                                                                                          <NA> 2018-08-08  2018-08-08   2018-08-08 47.80
# 83       231961 IL16211-5121     Friday    Amanda                                                                                                                          <NA> 2018-04-20  2018-04-20         <NA> 23.14
# 84       231868 IL16211-5121     Friday    Amanda                                                                                                                          <NA> 2018-04-20  2018-04-20         <NA> 20.27
# 85       493651 IL16211-5159    Tuesday Mackenzie                                                                                                                          <NA> 2018-05-08  2018-05-08         <NA> 27.58
# 86       493643 IL16211-5159    Tuesday Mackenzie                                                                                                                          <NA> 2018-05-08  2018-05-08         <NA> 26.18
# 87       854320 IL16211-5166  Wednesday    Farrar                                                                                                                          <NA> 2019-01-09  2019-01-09   2019-01-09 24.99
# 88       854316 IL16211-5166  Wednesday    Farrar                                                                                                                          <NA> 2019-01-09  2019-01-09   2019-01-09 22.53
# 89       599151 IL16211-5167  Wednesday Mackenzie The weight values for IL16441-5268 is out of usual range, double checked and it is correct. Mouse does appear sickly. 8/23/18 2018-08-22  2018-08-22   2018-08-22 23.78
# 90       599149 IL16211-5167  Wednesday Mackenzie The weight values for IL16441-5268 is out of usual range, double checked and it is correct. Mouse does appear sickly. 8/23/18 2018-08-22  2018-08-22   2018-08-22 21.63
# 91       707079 IL16211-5172  Wednesday    Farrar                                                                                                                          <NA> 2018-11-21  2018-11-23   2018-11-23 26.65
# 92       707085 IL16211-5172  Wednesday    Farrar                                                                                                                          <NA> 2018-11-21  2018-11-23   2018-11-23 26.70
# 93       707086 IL16211-5173  Wednesday    Farrar                                                                                                                          <NA> 2018-11-21  2018-11-23   2018-11-23 27.01
# 94       707075 IL16211-5173  Wednesday    Farrar                                                                                                                          <NA> 2018-11-21  2018-11-23   2018-11-23 27.02
# 95        16253 IL16211-5177  Wednesday Mackenzie                                                                                                                          <NA> 2018-03-07  2018-03-07         <NA> 27.20
# 96        16254 IL16211-5177  Wednesday Mackenzie                                                                                                                          <NA> 2018-03-07  2018-03-07         <NA> 28.47
# 97       502383 IL16441-5239     Friday Mackenzie                                                                                                                          <NA> 2018-06-08  2018-06-08         <NA> 23.03
# 98       502384 IL16441-5239     Friday Mackenzie                                                                                                                          <NA> 2018-06-08  2018-06-08         <NA> 24.29
# 99        15172 IL16441-5247     Friday    Farrar                                                                                                                          <NA> 2018-03-02  2018-03-02   2018-03-02 23.40
# 100       15175 IL16441-5247     Friday    Farrar                                                                                                                          <NA> 2018-03-02  2018-03-02   2018-03-02 36.15
# 101      231013 IL16441-5263    Tuesday    Farrar                                                                                                                          <NA> 2018-04-17  2018-04-17         <NA> 37.55
# 102      231003 IL16441-5263    Tuesday    Farrar                                                                                                                          <NA> 2018-04-17  2018-04-17         <NA> 37.63
# 103      225594 IL16441-5266  Wednesday Mackenzie                                                                                                                          <NA> 2018-03-21  2018-03-21         <NA> 29.73
# 104      225595 IL16441-5266  Wednesday Mackenzie                                                                                                                          <NA> 2018-03-21  2018-03-21         <NA> 28.99
# 105      553133 IL16441-5266  Wednesday    Farrar                                                                                                                          <NA> 2018-07-11  2018-07-11   2018-07-11 35.46
# 106      553131 IL16441-5266  Wednesday    Farrar                                                                                                                          <NA> 2018-07-11  2018-07-11   2018-07-11 32.12
# 107      574123 IL16513-5300   Thursday    Amanda                                                                                                                          <NA> 2018-08-02  2018-08-02   2018-08-02 28.03
# 108      574167 IL16513-5300   Thursday    Amanda                                                                                                                          <NA> 2018-08-02  2018-08-02   2018-08-02 23.31
# 109      233188 IL16513-5302   Thursday    Farrar                                                                                                                          <NA> 2018-04-26  2018-04-26         <NA> 24.96
# 110      233201 IL16513-5302   Thursday    Farrar                                                                                                                          <NA> 2018-04-26  2018-04-26         <NA> 23.73
# 111      499776 IL16513-5318     Friday    Farrar                                                                                                                          <NA> 2018-05-25  2018-05-25         <NA> 19.98
# 112      499770 IL16513-5318     Friday    Farrar                                                                                                                          <NA> 2018-05-25  2018-05-25         <NA> 20.00
# 113      230940 IL16513-5349    Tuesday    Farrar                                                                                                                          <NA> 2018-04-17  2018-04-17         <NA> 23.89
# 114      230944 IL16513-5349    Tuesday    Farrar                                                                                                                          <NA> 2018-04-17  2018-04-17         <NA> 20.38
# 115      539191 IL16513-5354    Tuesday     Gaven                                                                                                                          <NA> 2018-07-03  2018-07-03   2018-07-03 20.41
# 116      539193 IL16513-5354    Tuesday     Gaven                                                                                                                          <NA> 2018-07-03  2018-07-03   2018-07-03 22.81
# 117      707671 IL16513-5362    Tuesday Mackenzie                                                                                                                          <NA> 2018-11-27  2018-11-27   2018-11-27 28.29
# 118      707672 IL16513-5362    Tuesday Mackenzie                                                                                                                          <NA> 2018-11-27  2018-11-27   2018-11-27 23.16
# 119      921998 IL16513-5365  Wednesday Mackenzie                                                                                                                          <NA> 2019-02-27  2019-02-27         <NA> 25.02
# 120      922015 IL16513-5365  Wednesday Mackenzie                                                                                                                          <NA> 2019-02-27  2019-02-27         <NA> 25.88
# 121      710032 IL16513-5366  Wednesday Mackenzie                                                                                                                          <NA> 2018-12-05  2018-12-06   2018-12-06 24.77
# 122      710034 IL16513-5366  Wednesday Mackenzie                                                                                                                          <NA> 2018-12-05  2018-12-06   2018-12-06 26.95
# 123      922002 IL16513-5370  Wednesday Mackenzie                                                                                                                          <NA> 2019-02-27  2019-02-27         <NA> 25.18
# 124      922099 IL16513-5370  Wednesday Mackenzie                                                                                                                          <NA> 2019-02-27  2019-02-27         <NA> 20.41
# 125      500522 IL16513-5371  Wednesday Mackenzie                                                                                                                          <NA> 2018-05-30  2018-05-31         <NA> 22.64
# 126      500520 IL16513-5371  Wednesday Mackenzie                                                                                                                          <NA> 2018-05-30  2018-05-31         <NA> 20.30
# 127      710045 IL16513-5372  Wednesday Mackenzie                                                                                                                          <NA> 2018-12-05  2018-12-06   2018-12-06 24.51
# 128      710047 IL16513-5372  Wednesday Mackenzie                                                                                                                          <NA> 2018-12-05  2018-12-06   2018-12-06 24.58
# 129      854768 IL16513-5374  Wednesday Mackenzie                                                                                                                          <NA> 2019-01-09  2019-01-11   2019-01-09 25.19
# 130      854769 IL16513-5374  Wednesday Mackenzie                                                                                                                          <NA> 2019-01-09  2019-01-11   2019-01-09 24.79
# 131       15272 IL16557-5421     Friday Mackenzie                                                                                                                          <NA> 2018-03-02  2018-03-02   2018-03-02 31.80
# 132       15278 IL16557-5421     Friday Mackenzie                                                                                                                          <NA> 2018-03-02  2018-03-02   2018-03-02 31.81
# 133      887571 IL16557-5462    Tuesday     Gaven                                                                                                                          <NA> 2019-01-29  2019-01-29         <NA> 32.82
# 134      887572 IL16557-5462    Tuesday     Gaven                                                                                                                          <NA> 2019-01-29  2019-01-29         <NA> 41.51
# 135      854780 IL16557-5464  Wednesday Mackenzie                                                                                                                          <NA> 2019-01-09  2019-01-11   2019-01-09 43.57
# 136      854776 IL16557-5464  Wednesday Mackenzie                                                                                                                          <NA> 2019-01-09  2019-01-11   2019-01-09 36.01
# 137      921961 IL16557-5464  Wednesday Mackenzie                                                                                                                          <NA> 2019-02-27  2019-02-27         <NA> 35.55
# 138      922016 IL16557-5464  Wednesday Mackenzie                                                                                                                          <NA> 2019-02-27  2019-02-27         <NA> 29.63
# 139      854777 IL16557-5465  Wednesday Mackenzie                                                                                                                          <NA> 2019-01-09  2019-01-11   2019-01-09 43.79
# 140      854781 IL16557-5465  Wednesday Mackenzie                                                                                                                          <NA> 2019-01-09  2019-01-11   2019-01-09 43.16
# 141      708078 IL16557-5477  Wednesday    Farrar                                                                                                                          <NA> 2018-11-28  2018-11-28   2018-11-28 39.71
# 142      708070 IL16557-5477  Wednesday    Farrar                                                                                                                          <NA> 2018-11-28  2018-11-28   2018-11-28 48.42
# 143      836140 IL16750-5773  Wednesday    Amanda                                                                                                                          <NA> 2019-01-02  2019-01-03   2019-01-03 31.74
# 144      836052 IL16750-5773  Wednesday    Amanda                                                                                                                          <NA> 2019-01-02  2019-01-03   2019-01-03 31.81
# 145      836047 IL16750-5774  Wednesday    Amanda                                                                                                                          <NA> 2019-01-02  2019-01-03   2019-01-03 22.26
# 146      836137 IL16750-5774  Wednesday    Amanda                                                                                                                          <NA> 2019-01-02  2019-01-03   2019-01-03 22.27
# 147      836133 IL16750-5777  Wednesday    Amanda                                                                                                                          <NA> 2019-01-02  2019-01-03   2019-01-03 31.39
# 148      836043 IL16750-5777  Wednesday    Amanda                                                                                                                          <NA> 2019-01-02  2019-01-03   2019-01-03 31.41
# 149      836008 IL16750-5778  Wednesday    Amanda                                                                                                                          <NA> 2019-01-02  2019-01-03   2019-01-03 21.53
# 150      836077 IL16750-5778  Wednesday    Amanda                                                                                                                          <NA> 2019-01-02  2019-01-03   2019-01-03 21.30
# 151      227148 OR13067-5808   Thursday Mackenzie                                                                                                                          <NA> 2018-03-29  2018-03-29         <NA> 33.65
# 152      227152 OR13067-5808   Thursday Mackenzie                                                                                                                          <NA> 2018-03-29  2018-03-29         <NA> 33.61
# 153      494287 OR13067-5809   Thursday Mackenzie                                                                                                                          <NA> 2018-05-10  2018-05-10         <NA> 32.90
# 154      494277 OR13067-5809   Thursday Mackenzie                                                                                                                          <NA> 2018-05-10  2018-05-10         <NA> 38.89
# 155      227151 OR13067-5811   Thursday Mackenzie                                                                                                                          <NA> 2018-03-29  2018-03-29         <NA> 41.96
# 156      227149 OR13067-5811   Thursday Mackenzie                                                                                                                          <NA> 2018-03-29  2018-03-29         <NA> 41.97
# 157       15459 OR13067-5816     Friday Mackenzie                                                                                                                          <NA> 2018-03-02  2018-03-02   2018-03-02 36.15
# 158       15461 OR13067-5816     Friday Mackenzie                                                                                                                          <NA> 2018-03-02  2018-03-02   2018-03-02 37.80
# 159      230551 OR13067-5830     Friday Mackenzie                                                                                                                          <NA> 2018-04-13  2018-04-13         <NA> 28.25
# 160      230553 OR13067-5830     Friday Mackenzie                                                                                                                          <NA> 2018-04-13  2018-04-13         <NA> 25.14
# 161      702982 OR13067-5840     Friday Mackenzie                                                                                                                          <NA> 2018-11-02  2018-11-02   2018-11-02 34.86
# 162      702975 OR13067-5840     Friday Mackenzie                                                                                                                          <NA> 2018-11-02  2018-11-02   2018-11-02 38.98
# 163      609393 OR13067-5857    Tuesday    Farrar                                                                                                                          <NA> 2018-09-04  2018-09-04   2018-09-04 37.18
# 164      609397 OR13067-5857    Tuesday    Farrar                                                                                                                          <NA> 2018-09-04  2018-09-04   2018-09-04 37.95
# 165      698281 OR13067-5857    Tuesday Mackenzie                                                                                                                          <NA> 2018-10-16  2018-10-16   2018-10-16 38.50
# 166      698283 OR13067-5857    Tuesday Mackenzie                                                                                                                          <NA> 2018-10-16  2018-10-16   2018-10-16 36.58
# 167      797373 OR13067-5866  Wednesday Mackenzie                                                                                                                          <NA> 2018-12-19  2018-12-19         <NA> 32.23
# 168      797370 OR13067-5866  Wednesday Mackenzie                                                                                                                          <NA> 2018-12-19  2018-12-19         <NA> 34.41
# 169      554383 OR13067-5867  Wednesday    Farrar                                                                                                                          <NA> 2018-07-18  2018-07-18   2018-07-18 29.82
# 170      554381 OR13067-5867  Wednesday    Farrar                                                                                                                          <NA> 2018-07-18  2018-07-18   2018-07-18 30.65
# 171      581065  OR3609-5916     Friday Mackenzie                                                                                                                          <NA> 2018-08-03  2018-08-03   2018-08-03 25.24
# 172      581075  OR3609-5916     Friday Mackenzie                                                                                                                          <NA> 2018-08-03  2018-08-03   2018-08-03 31.45
# 173      495882  OR3609-5932     Friday     Gaven                                                                                                                          <NA> 2018-05-18  2018-05-18   2018-05-18 40.33
# 174      495885  OR3609-5932     Friday     Gaven                                                                                                                          <NA> 2018-05-18  2018-05-18   2018-05-18 24.51
# 175      495884  OR3609-5935     Friday     Gaven                                                                                                                          <NA> 2018-05-18  2018-05-18   2018-05-18 28.66
# 176      495879  OR3609-5935     Friday     Gaven                                                                                                                          <NA> 2018-05-18  2018-05-18   2018-05-18 32.89
# 177      493263  OR3609-5941     Friday Mackenzie                                                                                                                          <NA> 2018-05-04  2018-05-04         <NA> 33.63
# 178      493264  OR3609-5941     Friday Mackenzie                                                                                                                          <NA> 2018-05-04  2018-05-04         <NA> 33.04

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
  arrange(
    MouseID, DateCollect, DateDue, DateComplete
  ) %>% 
  group_by(
    MouseID, DateCollect,
  ) %>% 
  summarise(
    N = n(),
    DateDue = paste0(unique(DateDue), collapse = ', '),
    DateComplete = paste0(unique(DateComplete), collapse = ', '),
    BWs = paste0(unique(BW), collapse = ', '),
    Range = max(BW, na.rm = TRUE) - min(BW, na.rm = TRUE),
    Mean = mean(BW, na.rm = TRUE)
  ) %>% 
  data.frame()
#         MouseID DateCollect N    DateDue DateComplete          BWs Range   Mean
# 1   AU8002-5513  2018-04-19 2 2018-04-19           NA 29.44, 29.45  0.01 29.445
# 2   AU8002-5529  2018-05-04 2 2018-05-04           NA 30.61, 33.61  3.00 32.110
# 3   AU8002-5545  2018-09-21 2 2018-09-21           NA 24.77, 24.82  0.05 24.795
# 4   AU8002-5546  2019-01-25 2 2019-01-25   2019-01-25   24.5, 25.7  1.20 25.100
# 5   AU8002-5547  2018-09-21 2 2018-09-21           NA 27.52, 27.63  0.11 27.575
# 6   AU8002-5552  2018-04-24 2 2018-04-24           NA 29.51, 29.23  0.28 29.370
# 7   AU8002-5553  2018-04-24 2 2018-04-24           NA 23.11, 23.01  0.10 23.060
# 8   AU8002-5554  2018-04-24 2 2018-04-24           NA 30.03, 30.12  0.09 30.075
# 9   AU8002-5555  2018-04-24 2 2018-04-24           NA 28.46, 28.45  0.01 28.455
# 10  AU8002-5571  2018-09-19 2 2018-09-19   2018-09-19 32.81, 30.41  2.40 31.610
# 11  AU8002-5571  2018-10-24 2 2018-10-24   2018-10-24  31.6, 26.54  5.06 29.070
# 12  AU8002-5571  2018-11-23 2 2018-11-21   2018-11-23  31.8, 29.56  2.24 30.680
# 13  AU8002-5571  2018-11-28 2 2018-11-28   2018-11-28 30.31, 32.82  2.51 31.565
# 14  AU8048-5612  2018-03-09 2 2018-03-08           NA 27.28, 25.57  1.71 26.425
# 15  AU8048-5617  2018-06-01 2 2018-06-01   2018-06-01 23.72, 23.69  0.03 23.705
# 16  AU8048-5639  2018-05-25 2 2018-05-25           NA  20.22, 20.2  0.02 20.210
# 17  AU8048-5642  2018-11-30 2 2018-11-30   2018-11-30  27.82, 27.4  0.42 27.610
# 18  AU8048-5665  2018-08-15 2 2018-08-15   2018-08-15 24.13, 24.11  0.02 24.120
# 19  AU8048-5670  2018-05-02 2 2018-05-02           NA 25.78, 19.09  6.69 22.435
# 20  AU8048-5672  2018-05-02 2 2018-05-02           NA 31.46, 33.22  1.76 32.340
# 21 IL16188-5030  2018-06-08 2 2018-06-08           NA 31.34, 36.82  5.48 34.080
# 22 IL16188-5037  2018-06-08 2 2018-06-08           NA  32.2, 32.16  0.04 32.180
# 23 IL16188-5038  2018-07-27 2 2018-07-27   2018-07-27 29.04, 42.55 13.51 35.795
# 24 IL16188-5038  2019-03-01 2 2019-03-01   2019-03-01 27.93, 34.13  6.20 31.030
# 25 IL16188-5040  2018-08-24 2 2018-08-24           NA 39.73, 39.61  0.12 39.670
# 26 IL16188-5043  2018-08-24 2 2018-08-24           NA 49.51, 49.68  0.17 49.595
# 27 IL16188-5045  2018-08-24 2 2018-08-24           NA 35.61, 36.57  0.96 36.090
# 28 IL16188-5047  2018-08-24 2 2018-08-24           NA  37.43, 38.6  1.17 38.015
# 29 IL16188-5049  2018-04-17 2 2018-04-17           NA  34.93, 35.2  0.27 35.065
# 30 IL16188-5049  2018-10-02 2 2018-10-02   2018-10-02 27.67, 42.63 14.96 35.150
# 31 IL16188-5051  2018-10-02 2 2018-10-02   2018-10-02  45.02, 44.7  0.32 44.860
# 32 IL16188-5055  2018-10-16 2 2018-10-16   2018-10-16 39.03, 20.97 18.06 30.000
# 33 IL16188-5064  2018-03-28 2 2018-03-28   2018-03-28 37.16, 25.21 11.95 31.185
# 34 IL16188-5064  2018-11-14 2 2018-11-14           NA 35.98, 35.65  0.33 35.815
# 35 IL16188-5064  2018-11-23 2 2018-11-21   2018-11-23 37.03, 26.41 10.62 31.720
# 36 IL16188-5065  2018-11-14 2 2018-11-14           NA 23.14, 23.05  0.09 23.095
# 37 IL16188-5067  2018-11-14 2 2018-11-14           NA 29.09, 29.49  0.40 29.290
# 38 IL16188-5068  2018-11-14 2 2018-11-14           NA 24.65, 24.56  0.09 24.605
# 39 IL16188-5069  2018-11-14 2 2018-11-14           NA 29.29, 29.33  0.04 29.310
# 40 IL16188-5070  2018-11-14 2 2018-11-14           NA 36.03, 35.55  0.48 35.790
# 41 IL16188-5074  2018-08-08 2 2018-08-08   2018-08-08  45.59, 47.8  2.21 46.695
# 42 IL16211-5121  2018-04-20 2 2018-04-20           NA 23.14, 20.27  2.87 21.705
# 43 IL16211-5159  2018-05-08 2 2018-05-08           NA 27.58, 26.18  1.40 26.880
# 44 IL16211-5166  2019-01-09 2 2019-01-09   2019-01-09 24.99, 22.53  2.46 23.760
# 45 IL16211-5167  2018-08-22 2 2018-08-22   2018-08-22 23.78, 21.63  2.15 22.705
# 46 IL16211-5172  2018-11-23 2 2018-11-21   2018-11-23  26.65, 26.7  0.05 26.675
# 47 IL16211-5173  2018-11-23 2 2018-11-21   2018-11-23 27.01, 27.02  0.01 27.015
# 48 IL16211-5177  2018-03-07 2 2018-03-07           NA  27.2, 28.47  1.27 27.835
# 49 IL16441-5239  2018-06-08 2 2018-06-08           NA 23.03, 24.29  1.26 23.660
# 50 IL16441-5247  2018-03-02 2 2018-03-02   2018-03-02  23.4, 36.15 12.75 29.775
# 51 IL16441-5263  2018-04-17 2 2018-04-17           NA 37.55, 37.63  0.08 37.590
# 52 IL16441-5266  2018-03-21 2 2018-03-21           NA 29.73, 28.99  0.74 29.360
# 53 IL16441-5266  2018-07-11 2 2018-07-11   2018-07-11 35.46, 32.12  3.34 33.790
# 54 IL16513-5300  2018-08-02 2 2018-08-02   2018-08-02 28.03, 23.31  4.72 25.670
# 55 IL16513-5302  2018-04-26 2 2018-04-26           NA 24.96, 23.73  1.23 24.345
# 56 IL16513-5318  2018-05-25 2 2018-05-25           NA    19.98, 20  0.02 19.990
# 57 IL16513-5349  2018-04-17 2 2018-04-17           NA 23.89, 20.38  3.51 22.135
# 58 IL16513-5354  2018-07-03 2 2018-07-03   2018-07-03 20.41, 22.81  2.40 21.610
# 59 IL16513-5362  2018-11-27 2 2018-11-27   2018-11-27 28.29, 23.16  5.13 25.725
# 60 IL16513-5365  2019-02-27 2 2019-02-27           NA 25.02, 25.88  0.86 25.450
# 61 IL16513-5366  2018-12-06 2 2018-12-05   2018-12-06 24.77, 26.95  2.18 25.860
# 62 IL16513-5370  2019-02-27 2 2019-02-27           NA 25.18, 20.41  4.77 22.795
# 63 IL16513-5371  2018-05-31 2 2018-05-30           NA  22.64, 20.3  2.34 21.470
# 64 IL16513-5372  2018-12-06 2 2018-12-05   2018-12-06 24.51, 24.58  0.07 24.545
# 65 IL16513-5374  2019-01-11 2 2019-01-09   2019-01-09 25.19, 24.79  0.40 24.990
# 66 IL16557-5421  2018-03-02 2 2018-03-02   2018-03-02  31.8, 31.81  0.01 31.805
# 67 IL16557-5462  2019-01-29 2 2019-01-29           NA 32.82, 41.51  8.69 37.165
# 68 IL16557-5464  2019-01-11 2 2019-01-09   2019-01-09 43.57, 36.01  7.56 39.790
# 69 IL16557-5464  2019-02-27 2 2019-02-27           NA 35.55, 29.63  5.92 32.590
# 70 IL16557-5465  2019-01-11 2 2019-01-09   2019-01-09 43.79, 43.16  0.63 43.475
# 71 IL16557-5477  2018-11-28 2 2018-11-28   2018-11-28 39.71, 48.42  8.71 44.065
# 72 IL16750-5773  2019-01-03 2 2019-01-02   2019-01-03 31.74, 31.81  0.07 31.775
# 73 IL16750-5774  2019-01-03 2 2019-01-02   2019-01-03 22.26, 22.27  0.01 22.265
# 74 IL16750-5777  2019-01-03 2 2019-01-02   2019-01-03 31.39, 31.41  0.02 31.400
# 75 IL16750-5778  2019-01-03 2 2019-01-02   2019-01-03  21.53, 21.3  0.23 21.415
# 76 OR13067-5808  2018-03-29 2 2018-03-29           NA 33.65, 33.61  0.04 33.630
# 77 OR13067-5809  2018-05-10 2 2018-05-10           NA  32.9, 38.89  5.99 35.895
# 78 OR13067-5811  2018-03-29 2 2018-03-29           NA 41.96, 41.97  0.01 41.965
# 79 OR13067-5816  2018-03-02 2 2018-03-02   2018-03-02  36.15, 37.8  1.65 36.975
# 80 OR13067-5830  2018-04-13 2 2018-04-13           NA 28.25, 25.14  3.11 26.695
# 81 OR13067-5840  2018-11-02 2 2018-11-02   2018-11-02 34.86, 38.98  4.12 36.920
# 82 OR13067-5857  2018-09-04 2 2018-09-04   2018-09-04 37.18, 37.95  0.77 37.565
# 83 OR13067-5857  2018-10-16 2 2018-10-16   2018-10-16  38.5, 36.58  1.92 37.540
# 84 OR13067-5866  2018-12-19 2 2018-12-19           NA 32.23, 34.41  2.18 33.320
# 85 OR13067-5867  2018-07-18 2 2018-07-18   2018-07-18 29.82, 30.65  0.83 30.235
# 86  OR3609-5916  2018-08-03 2 2018-08-03   2018-08-03 25.24, 31.45  6.21 28.345
# 87  OR3609-5932  2018-05-18 2 2018-05-18   2018-05-18 40.33, 24.51 15.82 32.420
# 88  OR3609-5935  2018-05-18 2 2018-05-18   2018-05-18 28.66, 32.89  4.23 30.775
# 89  OR3609-5941  2018-05-04 2 2018-05-04           NA 33.63, 33.04  0.59 33.335

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
  group_by(
    DateCollect,
  ) %>% 
  summarise(
    N = n(),
  ) %>% 
  arrange(desc(N)) %>% 
  data.frame() 
#    DateCollect  N
# 1   2018-11-14 12
# 2   2018-04-24  8
# 3   2018-08-24  8
# 4   2018-11-23  8
# 5   2019-01-03  8
# 6   2018-03-02  6
# 7   2018-04-17  6
# 8   2018-06-08  6
# 9   2019-01-11  6
# 10  2019-02-27  6
# 11  2018-03-29  4
# 12  2018-05-02  4
# 13  2018-05-04  4
# 14  2018-05-18  4
# 15  2018-05-25  4
# 16  2018-09-21  4
# 17  2018-10-02  4
# 18  2018-10-16  4
# 19  2018-11-28  4
# 20  2018-12-06  4
# 21  2018-03-07  2
# 22  2018-03-09  2
# 23  2018-03-21  2
# 24  2018-03-28  2
# 25  2018-04-13  2
# 26  2018-04-19  2
# 27  2018-04-20  2
# 28  2018-04-26  2
# 29  2018-05-08  2
# 30  2018-05-10  2
# 31  2018-05-31  2
# 32  2018-06-01  2
# 33  2018-07-03  2
# 34  2018-07-11  2
# 35  2018-07-18  2
# 36  2018-07-27  2
# 37  2018-08-02  2
# 38  2018-08-03  2
# 39  2018-08-08  2
# 40  2018-08-15  2
# 41  2018-08-22  2
# 42  2018-09-04  2
# 43  2018-09-19  2
# 44  2018-10-24  2
# 45  2018-11-02  2
# 46  2018-11-27  2
# 47  2018-11-30  2
# 48  2018-12-19  2
# 49  2019-01-09  2
# 50  2019-01-25  2
# 51  2019-01-29  2
# 52  2019-03-01  2



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
  write_csv(
    'issues/same_mouse_same_date_different_bw.csv'
  )


X <- REVISED_DATA[
  (
    duplicated(
      REVISED_DATA[,c('MouseID', 'DateCollect')]
    ) |
      duplicated(
        REVISED_DATA[,c('MouseID', 'DateCollect')],
        fromLast = TRUE
      )
  ),
]

REVISED_DATA <- REVISED_DATA %>% 
  filter(
    !OutputRowID %in% X$OutputRowID
  )
  
rm(X)
  
#####


################################################################################
# Other duplicated output row IDs ####

# The output row ID should be unique to each record. However there are 17
# duplications (34 records in total) of output row IDs (after removing the
# duplicates mentioned above). For these records, the BWs and collection dates
# appear to be identical, but no the mouse ID. We need to figure out what 
# caused these duplications (row IDs should be unique), and, if possible, 
# determine to which mice the data correctly belong (though if we just drop
# them we'll probably be okay since it is only 34 records)

REVISED_DATA[
  (
    duplicated(REVISED_DATA$OutputRowID) | 
      duplicated(REVISED_DATA$OutputRowID, fromLast = TRUE)
  ),
] %>% 
  arrange(OutputRowID) %>% 
  data.frame()
#    OutputRowID      MouseID BWDay_Test      Tech BatchComments    DateDue DateCollect DateComplete    BW
# 1         4516 IL16557-5421     Friday    Farrar          <NA> 2018-02-16  2018-02-16         <NA> 30.63
# 2         4516  AU8048-5646     Friday    Farrar          <NA> 2018-02-16  2018-02-16         <NA> 30.63
# 3        13771 IL16557-5417     Friday    Farrar          <NA> 2018-02-23  2018-02-23   2018-02-23 40.66
# 4        13771  AU8002-5535     Friday    Farrar          <NA> 2018-02-23  2018-02-23   2018-02-23 40.66
# 5        13772 IL16557-5416     Friday Mackenzie          <NA> 2018-02-23  2018-02-23   2018-02-23 27.98
# 6        13772  AU8002-5533     Friday Mackenzie          <NA> 2018-02-23  2018-02-23   2018-02-23 27.98
# 7        13773 IL16557-5419     Friday Mackenzie          <NA> 2018-02-23  2018-02-23   2018-02-23 31.02
# 8        13773  AU8002-5534     Friday Mackenzie          <NA> 2018-02-23  2018-02-23   2018-02-23 31.02
# 9       223864 IL16557-5445     Friday    Hannah          <NA> 2018-03-16  2018-03-16   2018-03-16 34.22
# 10      223864 IL16188-5022     Friday    Hannah          <NA> 2018-03-16  2018-03-16   2018-03-16 34.22
# 11      223865 IL16557-5444     Friday    Hannah          <NA> 2018-03-16  2018-03-16   2018-03-16 37.74
# 12      223865 IL16188-5021     Friday    Hannah          <NA> 2018-03-16  2018-03-16   2018-03-16 37.74
# 13      223866 IL16557-5447     Friday    Hannah          <NA> 2018-03-16  2018-03-16   2018-03-16 37.89
# 14      223866 IL16188-5020     Friday    Hannah          <NA> 2018-03-16  2018-03-16   2018-03-16 37.89
# 15      499759 IL16557-5433     Friday Mackenzie          <NA> 2018-05-25  2018-05-25         <NA> 39.11
# 16      499759 IL16557-5416     Friday Mackenzie          <NA> 2018-05-25  2018-05-25         <NA> 39.11
# 17      502325 OR13067-5823     Friday Mackenzie          <NA> 2018-06-08  2018-06-08         <NA> 30.69
# 18      502325 IL16557-5428     Friday Mackenzie          <NA> 2018-06-08  2018-06-08         <NA> 30.69
# 19      503779  AU8048-5638     Friday Mackenzie          <NA> 2018-06-15  2018-06-15         <NA> 19.99
# 20      503779 OR13067-5819     Friday Mackenzie          <NA> 2018-06-15  2018-06-15         <NA> 19.99
# 21      581046 IL16188-5038     Friday Mackenzie          <NA> 2018-08-03  2018-08-03   2018-08-03 29.98
# 22      581046  AU8002-5517     Friday Mackenzie          <NA> 2018-08-03  2018-08-03   2018-08-03 29.98
# 23      702997 OR13067-5835     Friday    Farrar          <NA> 2018-11-02  2018-11-02   2018-11-02 31.75
# 24      702997 IL16513-5342     Friday    Farrar          <NA> 2018-11-02  2018-11-02   2018-11-02 31.75
# 25      854259 OR13067-5875  Wednesday    Farrar          <NA> 2019-01-09  2019-01-09   2019-01-09 26.43
# 26      854259  OR3609-5971  Wednesday    Farrar          <NA> 2019-01-09  2019-01-09   2019-01-09 26.43
# 27     1107099 IL16557-5446     Friday     Gaven          <NA> 2019-06-07  2019-06-07   2019-06-07 20.34
# 28     1107099  AU8048-5629     Friday     Gaven          <NA> 2019-06-07  2019-06-07   2019-06-07 20.34
# 29     1121201 OR13067-5842     Friday    Hannah          <NA> 2019-06-21  2019-06-21   2019-06-21 34.67
# 30     1121201  AU8048-5643     Friday    Hannah          <NA> 2019-06-21  2019-06-21   2019-06-21 34.67
# 31     1121202 OR13067-5840     Friday    Hannah          <NA> 2019-06-21  2019-06-21   2019-06-21 33.68
# 32     1121202  AU8048-5642     Friday    Hannah          <NA> 2019-06-21  2019-06-21   2019-06-21 33.68
# 33     1138301 OR13067-5875  Wednesday    Amanda          <NA> 2019-12-04  2019-12-04   2019-12-04 34.95
# 34     1138301 IL16513-5365  Wednesday    Amanda          <NA> 2019-12-04  2019-12-04   2019-12-04 34.95

REVISED_DATA[
  (
    duplicated(REVISED_DATA$OutputRowID) | 
      duplicated(REVISED_DATA$OutputRowID, fromLast = TRUE)
  ),
] %>% 
  arrange(OutputRowID) %>% 
  data.frame() %>% 
  write_csv(
    'issues/duplicated_outputrowid.csv'
  )

X <- REVISED_DATA[
  (
    duplicated(REVISED_DATA$OutputRowID) | 
      duplicated(REVISED_DATA$OutputRowID, fromLast = TRUE)
  ),
] %>% 
  arrange(OutputRowID)

REVISED_DATA <- REVISED_DATA %>% 
  filter(
    !OutputRowID %in% X$OutputRowID
  )

rm(X)

#####


################################################################################
# Records after death ####

# There are fifty records with collection dates after the death date. Some
# of these may represent mice for which the death date is wrong, or for which
# there is a typo in the collection date. Others may be swaps. Although it is 
# only a small number of records, we should try and identify what is going on 
# for each of these records in case it is a result of some larger underlying
# issue.

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
  mutate(
    DateDiff = as.numeric(difftime(DOE, DateCollect, units = 'days'))
  ) %>% 
  arrange(
    MouseID, DateCollect
  ) %>% 
  data.frame()
#    OutputRowID      MouseID BWDay_Test      Tech                                      BatchComments    DateDue DateCollect DateComplete    BW        DOE DateDiff
# 1       698925  AU8002-5513   Thursday     Gaven                                               <NA> 2018-10-18  2018-10-18   2018-10-18 24.08 2018-10-15       -3
# 2       701231  AU8002-5513   Thursday    Amanda                                               <NA> 2018-10-25  2018-10-25   2018-10-25 22.00 2018-10-15      -10
# 3       690843  AU8002-5519     Friday Mackenzie                                               <NA> 2018-09-28  2018-09-28   2018-09-28 25.62 2018-09-03      -25
# 4        12550  AU8002-5522     Friday    Hannah                                               <NA> 2018-02-09  2018-02-09         <NA> 22.33 2018-01-12      -28
# 5      1129455  AU8002-5531     Friday Mackenzie                                               <NA> 2019-09-13  2019-09-13   2019-09-13 23.44 2019-05-03     -133
# 6       886587  AU8002-5547     Friday Mackenzie                                               <NA> 2019-01-25  2019-01-25   2019-01-25 34.25 2018-11-12      -74
# 7       686352  AU8002-5564  Wednesday Mackenzie                                               <NA> 2018-09-19  2018-09-19   2018-09-19 27.50 2018-09-17       -2
# 8      1347439  AU8048-5619     Friday      <NA>                                               <NA> 2018-01-05  2018-01-05         <NA> 21.80 2018-01-03       -2
# 9      1348040  AU8048-5633     Friday      <NA>                                               <NA> 2017-07-07  2017-07-07         <NA> 19.75 2017-03-27     -102
# 10      890485  AU8048-5637     Friday    Farrar                                               <NA> 2019-02-08  2019-02-08   2019-02-08 18.11 2019-02-01       -7
# 11      891993  AU8048-5637     Friday Mackenzie                                               <NA> 2019-02-15  2019-02-15   2019-02-15 21.20 2019-02-01      -14
# 12      921465  AU8048-5637     Friday    Amanda                                               <NA> 2019-02-22  2019-02-22   2019-02-22 17.44 2019-02-01      -21
# 13      892001  AU8048-5640     Friday Mackenzie                                               <NA> 2019-02-15  2019-02-15   2019-02-15 34.13 2018-11-26      -81
# 14      979124  AU8048-5651    Tuesday    Amanda                                               <NA> 2019-04-09  2019-04-09   2019-04-09 35.31 2018-11-18     -142
# 15      600324 IL16188-5036     Friday Mackenzie                                               <NA> 2018-08-24  2018-08-24         <NA> 30.05 2018-08-14      -10
# 16      607833 IL16188-5036     Friday    Farrar                                               <NA> 2018-08-31  2018-08-31   2018-08-31 31.42 2018-08-14      -17
# 17      611733 IL16188-5036     Friday Mackenzie                                               <NA> 2018-09-07  2018-09-07   2018-09-07 30.67 2018-08-14      -24
# 18      683261 IL16188-5036     Friday     Gaven     Half off CCW2G2 are in wheel running 9/14/2018 2018-09-14  2018-09-14   2018-09-14 30.05 2018-08-14      -31
# 19      688918 IL16188-5036     Friday Mackenzie                                               <NA> 2018-09-21  2018-09-21         <NA> 28.37 2018-08-14      -38
# 20      690699 IL16188-5036     Friday    Hannah                                               <NA> 2018-09-28  2018-09-28   2018-09-28 28.99 2018-08-14      -45
# 21      693974 IL16188-5036     Friday Mackenzie                                               <NA> 2018-10-05  2018-10-05   2018-10-05 25.03 2018-08-14      -52
# 22      697271 IL16188-5036     Friday Mackenzie                                               <NA> 2018-10-12  2018-10-12   2018-10-12 21.57 2018-08-14      -59
# 23      922067 IL16188-5065  Wednesday Mackenzie                                               <NA> 2019-02-27  2019-02-27         <NA> 24.67 2019-01-16      -42
# 24     1247796 IL16188-5072  Wednesday     Gaven                                               <NA> 2020-04-08  2020-04-08   2020-04-08 23.47 2019-07-02     -281
# 25      985668 IL16188-5073  Wednesday    Hannah                                               <NA> 2019-05-08  2019-05-08   2019-05-08 39.40 2019-03-18      -51
# 26     1115113 IL16188-5073  Wednesday    Farrar                                               <NA> 2019-06-12  2019-06-12   2019-06-12 20.41 2019-03-18      -86
# 27        4587 IL16211-5124     Friday     Daria                                               <NA> 2018-02-16  2018-02-16         <NA> 23.80 2017-10-30     -109
# 28      502404 IL16211-5124     Friday    Hannah                                               <NA> 2018-06-08  2018-06-08         <NA> 23.96 2017-10-30     -221
# 29      558347 IL16211-5124     Friday    Farrar                                               <NA> 2018-07-27  2018-07-27   2018-07-27 34.42 2017-10-30     -270
# 30      597441 IL16211-5124     Friday    Farrar                                               <NA> 2018-08-17  2018-08-17   2018-08-17 21.20 2017-10-30     -291
# 31      683434 IL16211-5124     Friday    Farrar     Half off CCW2G2 are in wheel running 9/14/2018 2018-09-14  2018-09-14   2018-09-14 21.90 2017-10-30     -319
# 32      703028 IL16211-5124     Friday Mackenzie                                               <NA> 2018-11-02  2018-11-02   2018-11-02 25.87 2017-10-30     -368
# 33      710286 IL16211-5124     Friday Mackenzie The other group in W3 are in wheel running 12/7/18 2018-12-07  2018-12-07   2018-12-11 34.38 2017-10-30     -403
# 34      708925 IL16211-5127     Friday Mackenzie                                               <NA> 2018-11-30  2018-11-30   2018-11-30 35.99 2018-11-28       -2
# 35      977999 IL16211-5159    Tuesday    Farrar                                               <NA> 2019-04-02  2019-04-02   2019-04-02 24.86 2019-03-22      -11
# 36     1138324 IL16211-5166  Wednesday    Amanda                                               <NA> 2019-12-04  2019-12-04   2019-12-04 24.23 2019-12-02       -2
# 37     1115058 IL16211-5176  Wednesday    Farrar                                               <NA> 2019-06-12  2019-06-12   2019-06-12 23.99 2019-06-10       -2
# 38     1356844 IL16441-5203   Thursday      <NA>                                               <NA> 2017-10-19  2017-10-19         <NA> 24.00 2017-10-18       -1
# 39      553600 IL16441-5225     Friday     Gaven                                               <NA> 2018-07-13  2018-07-13   2018-07-13 23.07 2018-07-11       -2
# 40      695269 IL16441-5257    Tuesday     Gaven                                               <NA> 2018-10-09  2018-10-09   2018-10-10 41.13 2018-10-03       -6
# 41      698076 IL16441-5257    Tuesday Mackenzie                                               <NA> 2018-10-16  2018-10-16   2018-10-16 37.37 2018-10-03      -13
# 42      700418 IL16441-5257    Tuesday    Amanda                                               <NA> 2018-10-23  2018-10-23   2018-10-23 35.03 2018-10-03      -20
# 43      701959 IL16441-5257    Tuesday    Amanda                                               <NA> 2018-10-30  2018-10-30         <NA> 30.79 2018-10-03      -27
# 44      475527 IL16513-5315   Thursday      <NA>                                               <NA> 2017-12-07  2018-02-14         <NA>  2.00 2017-07-21     -208
# 45       15151 IL16513-5333     Friday    Farrar                                               <NA> 2018-03-02  2018-03-02   2018-03-02 21.94 2018-02-09      -21
# 46      882245 IL16513-5345     Friday Mackenzie                                               <NA> 2019-01-18  2019-01-18   2019-01-18 17.44 2018-12-10      -39
# 47     1124386 IL16513-5378  Wednesday     Gaven                                               <NA> 2019-07-24  2019-07-24         <NA> 29.22 2019-07-22       -2
# 48     1127723 OR13067-5816     Friday    Hannah                                               <NA> 2019-08-23  2019-08-23   2019-08-23 33.92 2018-05-30     -450
# 49      228244 OR13067-5852    Tuesday    Hannah                                               <NA> 2018-04-03  2018-04-04         <NA> 34.47 2018-01-16      -78
# 50     1140202 OR13067-5864  Wednesday    Hannah                                               <NA> 2020-01-01  2020-01-02   2020-01-02 19.14 2019-12-04      -29


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
  write_csv(
    'issues/bw_recorded_after_death.csv'
  )

X <- REVISED_DATA %>% 
  left_join(
    ANIMAL_DATA %>% 
      select(
        MouseID, DOE
      ),
    by = 'MouseID'
  ) %>% 
  filter(
    DateCollect > DOE
  )


REVISED_DATA <- REVISED_DATA %>% 
  filter(
    !OutputRowID %in% X$OutputRowID
  )

rm(X)

#####


################################################################################
# Impossibly high bodyweights ####

# There are some impossibly high bodyweights that must be typos. After 
# excluding bodyweights over 200 grams, the maximum bodyweight is ~ 65 grams,
# which seems reasonable.

REVISED_DATA$BW %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.37   23.20   27.01   28.57   32.33 4732.00 

REVISED_DATA %>% 
  filter(BW < 200) %>% 
  select(BW) %>% 
  unlist() %>% 
  hist(
    breaks = 300
  )

REVISED_DATA %>% 
  filter(BW < 200) %>% 
  select(BW) %>% 
  summary()
#       BW       
# Min.   : 0.37  
# 1st Qu.:23.20  
# Median :27.01  
# Mean   :28.36  
# 3rd Qu.:32.33  
# Max.   :64.91 

(
  HIGH_BW <- REVISED_DATA %>% 
    filter(
      BW > 100
    )
)
# A tibble: 6 x 9
#    OutputRowID MouseID      BWDay_Test Tech   BatchComments DateDue    DateCollect DateComplete    BW
#          <int> <chr>        <chr>      <chr>  <chr>         <date>     <date>      <date>       <dbl>
#  1     1366845 IL16557-5449 Tuesday    NA     NA            2017-07-11 2017-07-11  NA           4732    * Weird typo, should be 32.47
#  2      690211 IL16513-5379 Wednesday  Hannah NA            2018-09-26 2018-09-26  2018-09-26   2800    * Clear decimal typo, divide by 1000 
#  3     1364821 IL16557-5411 Thursday   NA     NA            2017-11-23 2017-11-23  NA           4517    * Clear decimal typo, divide by 1000  
#  4     1363160 IL16513-5345 Friday     NA     NA            2017-06-02 2017-06-02  NA            223.   * Typo, should be 23.47
#  5     1363161 IL16513-5345 Friday     NA     NA            2017-06-09 2017-06-09  NA            223.   * Typo, should be 23.42
#  6     1362078 IL16513-5325 Friday     NA     NA            2018-01-12 2018-01-12  NA           2197    * Clear decimal typo, divide by 1000  

HIGH_BW %>% 
  write_csv(
    'issues/impossibly_high_bw.csv'
  )

I <- 1
REVISED_DATA %>% 
  filter(
    MouseID %in% HIGH_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, HIGH_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test Tech BatchComments    DateDue DateCollect DateComplete      BW
# 1     1366842 IL16557-5449    Tuesday <NA>          <NA> 2017-06-20  2017-06-20         <NA>   28.02
# 2     1366843 IL16557-5449    Tuesday <NA>          <NA> 2017-06-27  2017-06-27         <NA>   30.25
# 3     1366844 IL16557-5449    Tuesday <NA>          <NA> 2017-07-04  2017-07-04         <NA>   32.07
# 4     1366845 IL16557-5449    Tuesday <NA>          <NA> 2017-07-11  2017-07-11         <NA> 4732.00
# 5     1366846 IL16557-5449    Tuesday <NA>          <NA> 2017-07-18  2017-07-18         <NA>   33.12
# 6     1366847 IL16557-5449    Tuesday <NA>          <NA> 2017-07-25  2017-07-25         <NA>   32.98
# 7     1366848 IL16557-5449    Tuesday <NA>          <NA> 2017-08-01  2017-08-01         <NA>   32.86

I <- 2
REVISED_DATA %>% 
  filter(
    MouseID %in% HIGH_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, HIGH_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test Tech BatchComments    DateDue DateCollect DateComplete      BW
# 1     1364818 IL16557-5411   Thursday <NA>          <NA> 2017-11-02  2017-11-02         <NA>   47.19
# 2     1364819 IL16557-5411   Thursday <NA>          <NA> 2017-11-09  2017-11-09         <NA>   46.97
# 3     1364820 IL16557-5411   Thursday <NA>          <NA> 2017-11-16  2017-11-16         <NA>   45.89
# 4     1364821 IL16557-5411   Thursday <NA>          <NA> 2017-11-23  2017-11-23         <NA> 4517.00
# 5     1364822 IL16557-5411   Thursday <NA>          <NA> 2017-11-30  2017-11-30         <NA>   43.75
# 6     1364823 IL16557-5411   Thursday <NA>          <NA> 2017-12-07  2017-12-07         <NA>   43.18
# 7     1364824 IL16557-5411   Thursday <NA>          <NA> 2017-12-14  2017-12-14         <NA>   43.86

I <- 3
REVISED_DATA %>% 
  filter(
    MouseID %in% HIGH_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, HIGH_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test Tech BatchComments    DateDue DateCollect DateComplete      BW
# 1     1364818 IL16557-5411   Thursday <NA>          <NA> 2017-11-02  2017-11-02         <NA>   47.19
# 2     1364819 IL16557-5411   Thursday <NA>          <NA> 2017-11-09  2017-11-09         <NA>   46.97
# 3     1364820 IL16557-5411   Thursday <NA>          <NA> 2017-11-16  2017-11-16         <NA>   45.89
# 4     1364821 IL16557-5411   Thursday <NA>          <NA> 2017-11-23  2017-11-23         <NA> 4517.00
# 5     1364822 IL16557-5411   Thursday <NA>          <NA> 2017-11-30  2017-11-30         <NA>   43.75
# 6     1364823 IL16557-5411   Thursday <NA>          <NA> 2017-12-07  2017-12-07         <NA>   43.18
# 7     1364824 IL16557-5411   Thursday <NA>          <NA> 2017-12-14  2017-12-14         <NA>   43.86

I <- 4
REVISED_DATA %>% 
  filter(
    MouseID %in% HIGH_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, HIGH_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test Tech BatchComments    DateDue DateCollect DateComplete     BW
# 1     1363157 IL16513-5345     Friday <NA>          <NA> 2017-05-12  2017-05-12         <NA>  22.42
# 2     1363158 IL16513-5345     Friday <NA>          <NA> 2017-05-19  2017-05-19         <NA>  22.43
# 3     1363159 IL16513-5345     Friday <NA>          <NA> 2017-05-26  2017-05-26         <NA>  23.12
# 4     1363160 IL16513-5345     Friday <NA>          <NA> 2017-06-02  2017-06-02         <NA> 223.47
# 5     1363161 IL16513-5345     Friday <NA>          <NA> 2017-06-09  2017-06-09         <NA> 223.42
# 6     1363162 IL16513-5345     Friday <NA>          <NA> 2017-06-16  2017-06-16         <NA>  23.48
# 7     1363163 IL16513-5345     Friday <NA>          <NA> 2017-06-23  2017-06-23         <NA>  23.48

I <- 6
REVISED_DATA %>% 
  filter(
    MouseID %in% HIGH_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, HIGH_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test  Tech BatchComments    DateDue DateCollect DateComplete      BW
# 1     1362075 IL16513-5325     Friday  <NA>          <NA> 2017-12-22  2017-12-22         <NA>   22.58
# 2     1362076 IL16513-5325     Friday  <NA>          <NA> 2017-12-29  2017-12-29         <NA>   22.70
# 3     1362077 IL16513-5325     Friday  <NA>          <NA> 2018-01-05  2018-01-05         <NA>   22.18
# 4     1362078 IL16513-5325     Friday  <NA>          <NA> 2018-01-12  2018-01-12         <NA> 2197.00
# 5     1362079 IL16513-5325     Friday  <NA>          <NA> 2018-01-19  2018-01-19         <NA>   22.09
# 6     1362080 IL16513-5325     Friday  <NA>          <NA> 2018-01-26  2018-01-26         <NA>   22.07
# 7       11209 IL16513-5325     Friday Daria          <NA> 2018-02-02  2018-02-01         <NA>   22.19

REVISED_DATA$BW[REVISED_DATA$OutputRowID == HIGH_BW$OutputRowID[1]] <- 32.47
REVISED_DATA$BW[REVISED_DATA$OutputRowID == HIGH_BW$OutputRowID[2]] <- 28.00
REVISED_DATA$BW[REVISED_DATA$OutputRowID == HIGH_BW$OutputRowID[3]] <- 45.17
REVISED_DATA$BW[REVISED_DATA$OutputRowID == HIGH_BW$OutputRowID[4]] <- 23.47
REVISED_DATA$BW[REVISED_DATA$OutputRowID == HIGH_BW$OutputRowID[5]] <- 23.42
REVISED_DATA$BW[REVISED_DATA$OutputRowID == HIGH_BW$OutputRowID[6]] <- 21.97

hist(
  REVISED_DATA$BW,
  breaks = 300
)

rm(HIGH_BW, I)

#####


################################################################################
# Impossibly low bodyweights ####

# There are some impossibly low bodyweights that must be typos.

REVISED_DATA$BW %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.37   23.20   27.01   28.36   32.33   64.91 

(
  LOW_BW <- REVISED_DATA %>% 
    filter(
      BW < 5
    )
) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test  Tech BatchComments    DateDue DateCollect DateComplete   BW
# 1       11626 IL16557-5454    Tuesday Gaven          <NA> 2018-02-06  2018-02-07         <NA> 3.21    * Typo, multiply by 10
# 2      475524 IL16513-5312   Thursday  <NA>          <NA> 2017-12-07  2018-02-14         <NA> 2.00    * Typo, multiply by 10
# 3      475525 IL16513-5313   Thursday  <NA>          <NA> 2017-12-07  2018-02-14         <NA> 2.00    * Typo, multiply by 10
# 4      475526 IL16513-5314   Thursday  <NA>          <NA> 2017-12-07  2018-02-14         <NA> 2.00    * Typo, multiply by 10
# 5     1355414 IL16211-5137     Friday  <NA>          <NA> 2017-09-15  2017-09-15         <NA> 0.37    * Typo, but not sure what the correct value should be

LOW_BW %>% 
  write_csv(
    'issues/impossibly_low_bw.csv'
  )

I <- 1
REVISED_DATA %>% 
  filter(
    MouseID %in% LOW_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, LOW_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test   Tech BatchComments    DateDue DateCollect DateComplete    BW
# 1     1367041 IL16557-5454    Tuesday   <NA>          <NA> 2018-01-23  2018-01-23         <NA> 32.21
# 2        9530 IL16557-5454    Tuesday  Gaven          <NA> 2018-01-30  2018-01-30         <NA> 33.54
# 3       11626 IL16557-5454    Tuesday  Gaven          <NA> 2018-02-06  2018-02-07         <NA>  3.21
# 4        3592 IL16557-5454    Tuesday Hannah          <NA> 2018-02-13  2018-02-14         <NA> 33.42
# 5       12927 IL16557-5454    Tuesday Farrar          <NA> 2018-02-20  2018-02-20   2018-02-20 33.18
# 6       14572 IL16557-5454    Tuesday Farrar          <NA> 2018-02-27  2018-02-28   2018-02-28 33.55

I <- 2
REVISED_DATA %>% 
  filter(
    MouseID %in% LOW_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, LOW_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test   Tech BatchComments    DateDue DateCollect DateComplete    BW
# 1      475524 IL16513-5312   Thursday   <NA>          <NA> 2017-12-07  2018-02-14         <NA>  2.00
# 2     1361197 IL16513-5312   Thursday   <NA>          <NA> 2018-01-25  2018-01-25         <NA> 22.63
# 3       10626 IL16513-5312   Thursday Farrar          <NA> 2018-02-01  2018-02-01         <NA> 21.44
# 4        4345 IL16513-5312   Thursday Farrar          <NA> 2018-02-15  2018-02-16         <NA> 19.81
# 5       13687 IL16513-5312   Thursday Farrar          <NA> 2018-02-22  2018-02-22   2018-02-22 20.43
# 6       15342 IL16513-5312   Thursday Farrar          <NA> 2018-03-01  2018-03-02   2018-03-02 19.14

I <- 3
REVISED_DATA %>% 
  filter(
    MouseID %in% LOW_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, LOW_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test   Tech BatchComments    DateDue DateCollect DateComplete    BW
# 1      475525 IL16513-5313   Thursday   <NA>          <NA> 2017-12-07  2018-02-14         <NA>  2.00
# 2     1361281 IL16513-5313   Thursday   <NA>          <NA> 2018-01-25  2018-01-25         <NA> 23.08
# 3       10624 IL16513-5313   Thursday Farrar          <NA> 2018-02-01  2018-02-01         <NA> 23.33
# 4        4346 IL16513-5313   Thursday Farrar          <NA> 2018-02-15  2018-02-16         <NA> 21.66
# 5       13686 IL16513-5313   Thursday Farrar          <NA> 2018-02-22  2018-02-22   2018-02-22 22.32
# 6       15344 IL16513-5313   Thursday Farrar          <NA> 2018-03-01  2018-03-02   2018-03-02 21.82

I <- 4
REVISED_DATA %>% 
  filter(
    MouseID %in% LOW_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, LOW_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test   Tech BatchComments    DateDue DateCollect DateComplete    BW
# 1      475526 IL16513-5314   Thursday   <NA>          <NA> 2017-12-07  2018-02-14         <NA>  2.00
# 2     1361365 IL16513-5314   Thursday   <NA>          <NA> 2018-01-25  2018-01-25         <NA> 24.08
# 3       10625 IL16513-5314   Thursday Farrar          <NA> 2018-02-01  2018-02-01         <NA> 23.46
# 4        4344 IL16513-5314   Thursday Farrar          <NA> 2018-02-15  2018-02-16         <NA> 22.07
# 5       13688 IL16513-5314   Thursday Farrar          <NA> 2018-02-22  2018-02-22   2018-02-22 23.46
# 6       15343 IL16513-5314   Thursday Farrar          <NA> 2018-03-01  2018-03-02   2018-03-02 21.97

I <- 5
REVISED_DATA %>% 
  filter(
    MouseID %in% LOW_BW$MouseID[I],
    abs(as.numeric(difftime(DateCollect, LOW_BW$DateCollect[I], units = 'days'))) <= 21
  ) %>% 
  data.frame()
#   OutputRowID      MouseID BWDay_Test Tech BatchComments    DateDue DateCollect DateComplete    BW
# 1     1355411 IL16211-5137     Friday <NA>          <NA> 2017-08-25  2017-08-25         <NA> 14.66
# 2     1355412 IL16211-5137     Friday <NA>          <NA> 2017-09-01  2017-09-01         <NA> 23.87
# 3     1355413 IL16211-5137     Friday <NA>          <NA> 2017-09-08  2017-09-08         <NA> 18.20
# 4     1355414 IL16211-5137     Friday <NA>          <NA> 2017-09-15  2017-09-15         <NA>  0.37
# 5     1355415 IL16211-5137     Friday <NA>          <NA> 2017-09-22  2017-09-22         <NA> 14.89
# 6     1355416 IL16211-5137     Friday <NA>          <NA> 2017-09-29  2017-09-29         <NA> 20.32
# 7     1355417 IL16211-5137     Friday <NA>          <NA> 2017-10-06  2017-10-06         <NA> 20.00




REVISED_DATA$BW[REVISED_DATA$OutputRowID == LOW_BW$OutputRowID[1]] <- 32.10
REVISED_DATA$BW[REVISED_DATA$OutputRowID == LOW_BW$OutputRowID[2]] <- 20.00
REVISED_DATA$BW[REVISED_DATA$OutputRowID == LOW_BW$OutputRowID[3]] <- 20.00
REVISED_DATA$BW[REVISED_DATA$OutputRowID == LOW_BW$OutputRowID[4]] <- 20.00
REVISED_DATA$BW[REVISED_DATA$OutputRowID == LOW_BW$OutputRowID[5]] <- as.numeric(NA)


hist(
  REVISED_DATA$BW,
  breaks = 300
)

rm(LOW_BW, I)

#####


################################################################################
# Create BW plots with semi-cleaned data #### 

DATA <- ANIMAL_DATA %>% 
  select(
    MouseID, Diet, Strain, Sex, Cohort, BWDay, HID, DOB, DOE, COE,
  ) %>% 
  right_join(
    REVISED_DATA %>% 
      select(
        MouseID, DateCollect, BW
      ),
    by = 'MouseID'
  )

for(COHORT in sort(unique(DATA$Cohort))){
  COHORT_DATA <- DATA %>% 
    filter(
      Cohort == COHORT
    )
  
  pdf(
    paste0(
      'figures/semicleaned_bw_plots/',
      COHORT, 
      '_semicleaned_weekly_bw_by_cage.pdf'
    ),
    width = 14,
    height = 6
  )
  for(H in sort(unique(COHORT_DATA$HID))){
    PLOT_DATA <- COHORT_DATA %>% 
      filter(
        HID == H
      )
    
    PLOT <- PLOT_DATA %>% 
      ggplot() +
      theme_minimal(
        base_size = 9
      ) +
      geom_line(
        aes(x = DateCollect, y = BW, color = MouseID, group = MouseID),
        alpha = 0.8
      ) +
      geom_point(
        aes(x = DateCollect, y = BW, color = MouseID),
        alpha = 0.5
      ) +
      geom_point(
        data = PLOT_DATA %>% 
          group_by(
            MouseID, DOB
          ) %>% 
          summarise(
            BW = 0
          ),
        aes(x = DOB, y = BW, color = MouseID),
        alpha = 0.5,
        shape = 15
      ) +
      geom_text(
        data = PLOT_DATA %>% 
          arrange(
            MouseID, DateCollect
          ) %>% 
          group_by(
            MouseID, DOE, COE
          ) %>% 
          summarise(
            BW = BW[n()]
          ),
        aes(x = DOE, y = BW, color = MouseID, label = COE),
        alpha = 1,
        size = 9 * 5/14,
        hjust = 0
      ) +
      scale_x_date(
        date_breaks = 'months',
        date_minor_breaks = 'weeks',
        date_labels = '%y %b',
        limits = c(
          min(PLOT_DATA$DOB) - weeks(1),
          max(PLOT_DATA$DOE) + months(2)
        ),
        exp = c(0,0)
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, 12),
        minor_breaks = seq(0, 100, 3),
        limits = c(0, 70),
        exp = c(0,0)
      ) +
      scale_color_manual(
        values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
      ) +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(
          angle = 45, 
          vjust = 1, 
          hjust = 1
        )
      ) +
      labs(
        title = paste0('Semi-cleaned (duplicates, impossible weights, etc removed) weekly bodyweights for pen ', H),
        subtitle = paste0(
          unique(PLOT_DATA$Diet), ', ',
          COHORT, ', ', 
          unique(PLOT_DATA$Strain), ', ',
          unique(PLOT_DATA$Sex), ', ',
          unique(PLOT_DATA$BWDay), ' bodyweights'
        ),
        x = 'Collection date',
        y = 'Weight (grams)',
        color = 'Mouse',
        caption = 'The small rectangle at lower left indicates the birthdate for each mouse'
      )
    
    plot(PLOT)
    
    rm(PLOT_DATA, PLOT)
  }
  rm(H)
  dev.off()
}

#####


################################################################################
# Flag and remove records with large relative changes in BW ####

# Remove records that differ from adjacent BW values by more than 10% in the
# same direction, for instance if a mouse has the following three conse BWs:
#     41, 47, 42  or   41, 35, 42
# then the middle (second) BW would be removed. This results in the removal of
# 523 records out of 102,100 total records, or ~0.5%

MARG <- 0.10

flagged_bws = 1
FLAGGED_BW_TABLE <- REVISED_DATA %>% 
  filter(BW > 100000)
while(flagged_bws > 0){
  REVISED_DATA <- REVISED_DATA %>%
    arrange(MouseID, DateCollect) %>%
    group_by(MouseID) %>% 
    mutate(
      Lag = lag(BW, 1),
      Lead = lead(BW, 1),
      Lag_Diff = (Lag - BW)/Lag,
      Lead_Diff = (Lead - BW)/Lead,
      BW_Flag = (abs(Lag_Diff) > MARG & abs(Lead_Diff) > MARG) & (sign(Lag_Diff) == sign(Lead_Diff)),
      BW_Flag = ifelse(is.na(BW_Flag), FALSE, BW_Flag)
    ) %>%
    ungroup() %>% 
    select(-c(Lag, Lead, Lag_Diff, Lead_Diff))
  
  flagged_bws <- sum(REVISED_DATA$BW_Flag, na.rm = TRUE) 
  
  FLAGGED_BW_TABLE <- rbind(
    FLAGGED_BW_TABLE,
    REVISED_DATA %>% 
      filter(BW_Flag) %>% 
      select(-BW_Flag)
  )
  
  REVISED_DATA %>% 
    filter(BW_Flag) %>% 
    print()
  
  REVISED_DATA <- REVISED_DATA %>% 
    filter(!BW_Flag) %>% 
    select(-BW_Flag)
}
# # # A tibble: 1,470 x 10
# #    OutputRowID MouseID     BWDay_Test Tech   BatchComments DateDue    DateCollect DateComplete    BW BW_Flag
# #          <int> <chr>       <chr>      <chr>  <chr>         <date>     <date>      <date>       <dbl> <lgl>  
# #  1     1342728 AU8002-5500 Thursday   NA     NA            2017-02-16 2017-02-16  NA            29.4 TRUE   
# #  2     1342745 AU8002-5500 Thursday   NA     NA            2017-06-15 2017-06-15  NA            33.4 TRUE   
# #  3     1342752 AU8002-5500 Thursday   NA     NA            2017-08-03 2017-08-03  NA            34.7 TRUE   
# #  4     1342774 AU8002-5500 Thursday   NA     NA            2018-01-04 2018-01-04  NA            27.0 TRUE   
# #  5     1342805 AU8002-5501 Thursday   NA     NA            2017-01-12 2017-01-12  NA            25.6 TRUE   
# #  6     1342810 AU8002-5501 Thursday   NA     NA            2017-02-16 2017-02-16  NA            28.3 TRUE   
# #  7     1342834 AU8002-5501 Thursday   NA     NA            2017-08-03 2017-08-03  NA            31.6 TRUE   
# #  8     1342854 AU8002-5501 Thursday   NA     NA            2017-12-21 2017-12-21  NA            21.1 TRUE   
# #  9     1342856 AU8002-5501 Thursday   NA     NA            2018-01-04 2018-01-04  NA            35.5 TRUE   
# # 10       13623 AU8002-5501 Thursday   Farrar NA            2018-02-22 2018-02-22  2018-02-22    31.3 TRUE   
# # # … with 1,460 more rows
# 
# # # A tibble: 6 x 10
# #    OutputRowID MouseID      BWDay_Test Tech      BatchComments DateDue    DateCollect DateComplete    BW BW_Flag
# #          <int> <chr>        <chr>      <chr>     <chr>         <date>     <date>      <date>       <dbl> <lgl>  
# #  1     1349765 IL16188-5005 Thursday   NA        NA            2017-06-08 2017-06-08  NA            20.4 TRUE   
# #  2       13342 IL16188-5067 Wednesday  Mackenzie NA            2018-02-21 2018-02-21  NA            25.3 TRUE   
# #  3      554816 IL16513-5324 Friday     Gaven     NA            2018-07-20 2018-07-20  2018-07-20    28.6 TRUE   
# #  4      142522 IL16513-5338 Friday     Mackenzie NA            2018-03-09 2018-03-09  2018-03-09    24.7 TRUE   
# #  5      544454 OR13067-5826 Friday     Gaven     NA            2018-07-06 2018-07-06  NA            29.2 TRUE   
# #  6     1374874 OR3609-5916  Friday     NA        NA            2017-06-16 2017-06-16  NA            27.5 TRUE   
# # # A tibble: 0 x 10
# 
# # # … with 10 variables: OutputRowID <int>, MouseID <chr>, BWDay_Test <chr>, Tech <chr>, BatchComments <chr>, DateDue <date>, DateCollect <date>, DateComplete <date>, BW <dbl>, BW_Flag <lgl>

FLAGGED_BW_TABLE %>% 
  write_csv(
    'issues/flagged_bw.csv'
  )

rm(flagged_bws, MARG)

#####


################################################################################
# Create BW plots with speedcleaned data #### 

DATA <- ANIMAL_DATA %>% 
  select(
    MouseID, Diet, Strain, Sex, Cohort, BWDay, HID, DOB, DOE, COE,
  ) %>% 
  right_join(
    REVISED_DATA %>% 
      select(
        MouseID, DateCollect, BW
      ),
    by = 'MouseID'
  )

for(COHORT in sort(unique(DATA$Cohort))){
  COHORT_DATA <- DATA %>% 
    filter(
      Cohort == COHORT
    )
  
  pdf(
    paste0(
      'figures/speedcleaned_bw_plots/',
      COHORT, 
      '_speedcleaned_weekly_bw_by_cage.pdf'
    ),
    width = 14,
    height = 6
  )
  for(H in sort(unique(COHORT_DATA$HID))){
    PLOT_DATA <- COHORT_DATA %>% 
      filter(
        HID == H
      )
    
    PLOT <- PLOT_DATA %>% 
      ggplot() +
      theme_minimal(
        base_size = 9
      ) +
      geom_line(
        aes(x = DateCollect, y = BW, color = MouseID, group = MouseID),
        alpha = 0.8
      ) +
      geom_point(
        aes(x = DateCollect, y = BW, color = MouseID),
        alpha = 0.5
      ) +
      geom_point(
        data = PLOT_DATA %>% 
          group_by(
            MouseID, DOB
          ) %>% 
          summarise(
            BW = 0
          ),
        aes(x = DOB, y = BW, color = MouseID),
        alpha = 0.5,
        shape = 15
      ) +
      geom_text(
        data = PLOT_DATA %>% 
          arrange(
            MouseID, DateCollect
          ) %>% 
          group_by(
            MouseID, DOE, COE
          ) %>% 
          summarise(
            BW = BW[n()]
          ),
        aes(x = DOE, y = BW, color = MouseID, label = COE),
        alpha = 1,
        size = 9 * 5/14,
        hjust = 0
      ) +
      scale_x_date(
        date_breaks = 'months',
        date_minor_breaks = 'weeks',
        date_labels = '%y %b',
        limits = c(
          min(PLOT_DATA$DOB) - weeks(1),
          max(PLOT_DATA$DOE) + months(2)
        ),
        exp = c(0,0)
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, 12),
        minor_breaks = seq(0, 100, 3),
        limits = c(0, 70),
        exp = c(0,0)
      ) +
      scale_color_manual(
        values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
      ) +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(
          angle = 45, 
          vjust = 1, 
          hjust = 1
        )
      ) +
      labs(
        title = paste0('Cleaned (removed flagged bodyweights) weekly bodyweights for pen ', H),
        subtitle = paste0(
          unique(PLOT_DATA$Diet), ', ',
          COHORT, ', ', 
          unique(PLOT_DATA$Strain), ', ',
          unique(PLOT_DATA$Sex), ', ',
          unique(PLOT_DATA$BWDay), ' bodyweights'
        ),
        x = 'Collection date',
        y = 'Weight (grams)',
        color = 'Mouse',
        caption = 'The small rectangle at lower left indicates the birthdate for each mouse'
      )
    
    plot(PLOT)
    
    rm(PLOT_DATA, PLOT)
  }
  rm(H)
  dev.off()
}

#####


################################################################################
# Save data ####

REVISED_DATA %>% 
  write_csv('data/SpeedCleaned_BW_20210719.csv')
# # A tibble: 66,909 x 9
#    OutputRowID MouseID     BWDay_Test Tech  BatchComments DateDue    DateCollect DateComplete    BW
#          <int> <chr>       <chr>      <chr> <chr>         <date>     <date>      <date>       <dbl>
#  1     1342696 AU8002-5500 Thursday   NA    NA            2016-07-07 2016-07-07  NA            16.0
#  2     1342697 AU8002-5500 Thursday   NA    NA            2016-07-14 2016-07-14  NA            16.9
#  3     1342698 AU8002-5500 Thursday   NA    NA            2016-07-21 2016-07-21  NA            17.4
#  4     1342699 AU8002-5500 Thursday   NA    NA            2016-07-28 2016-07-28  NA            18.9
#  5     1342700 AU8002-5500 Thursday   NA    NA            2016-08-04 2016-08-04  NA            19.4
#  6     1342701 AU8002-5500 Thursday   NA    NA            2016-08-11 2016-08-11  NA            20.9
#  7     1342702 AU8002-5500 Thursday   NA    NA            2016-08-18 2016-08-18  NA            20.2
#  8     1342703 AU8002-5500 Thursday   NA    NA            2016-08-25 2016-08-25  NA            21.1
#  9     1342704 AU8002-5500 Thursday   NA    NA            2016-09-01 2016-09-01  NA            22.5
# 10     1342705 AU8002-5500 Thursday   NA    NA            2016-09-08 2016-09-08  NA            22.8
# # … with 66,899 more rows

#####


################################################################################
## clear workspace ###########################################

rm(list = ls())
pacman::p_unload('all')
graphics.off()

#####
