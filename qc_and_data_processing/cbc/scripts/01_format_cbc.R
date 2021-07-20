
setwd('/Users/deigha/Documents/Projects/CC_Longitudinal_IF/qc_and_data_processing/cbc/')

################################################################################
# Load libraries etc ####

options(max.print = 5000)
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

convert_to_na <- function(x, na_val = -999){
  
  if(length(x) > 1){
    x <- lapply(x, convert_to_na)
    x <- unlist(x)
    return(x)
  }
  
  if(!is.na(x)){
    if(near(x, na_val)){
      x <- as.numeric(NA)
    }
  }
  return(x)
}

#####


################################################################################
# Load data ####

ORIGINAL_DATA <- REVISED_DATA <- read_csv(
  'data/Blood3_20210719.csv',                           
  col_types = paste0(
    'iicccccnccccc',
    'ccccccccccnnccccciccncc',
    paste0(rep('n',33), collapse = ''),
    'cc',
    collapse = ''
  ),
  na = c('', 'NA', 'na', 'null', 'NULL', '-999')
)
# # A tibble: 1,074 x 71
#    TaskID OutputRowID AnimalName  LineName  Sex    AnimalStatus DateBorn AgeInWeeks DateExit ExitReason Marker HousingID Comments  DateDue JobID StudyName CollectionDateT… CollectedBy CompletedBy CompletedDate TaskName TaskNotes `Born Dates` `Age at Test`    BW Bleed Box   `Eye Drops` Cohort Sex_1 `Pen Number`
#     <int>       <int> <chr>       <chr>     <chr>  <chr>        <chr>         <dbl> <chr>    <chr>      <chr>  <chr>     <chr>     <chr>   <chr> <chr>     <chr>            <chr>       <chr>       <chr>         <chr>    <chr>     <chr>                <dbl> <dbl> <chr> <chr> <chr>       <chr>  <chr>        <int>
#  1   3224     1251077 CC IL16513… CC IL165… Male   Dead         2016-05…         97 2018-06… Missing (… N      NULL      "Eye dam… 2018-0… CC W… Collabor… 2018-03-28 04:0… Mackenzie   NULL        2018-10-23 1… Blood 3  NULL      5/18/2016               97  22.2 3/CBC NA    Proparacai… W1G1   M             3873
#  2   3224     1251078 CC IL16513… CC IL165… Male   Dead         2016-05…         97 2019-02… Found Dea… 2R     NULL      "NULL"    2018-0… CC W… Collabor… 2018-03-28 04:0… Mackenzie   NULL        2018-10-23 1… Blood 3  NULL      5/18/2016               97  24.6 3/CBC NA    Proparacai… W1G1   M             3873
#  3   3224     1251079 CC IL16513… CC IL165… Male   Dead         2016-05…         97 2018-08… Found Dea… L      NULL      "NULL"    2018-0… CC W… Collabor… 2018-03-28 04:0… Mackenzie   NULL        2018-10-23 1… Blood 3  NULL      5/18/2016               97  25.0 3/CBC NA    Proparacai… W1G1   M             3873
#  4   3224     1251069 CC IL16513… CC IL165… Female Dead         2016-04…        103 2018-09… Found Dea… L      NULL      "NULL"    2018-0… CC W… Collabor… 2018-03-28 04:0… Rachel      NULL        2018-10-23 1… Blood 3  NULL      4/3/2016               103  25.2 3/CBC NA    Proparacai… W1G1   F             3570
#  5   3224     1251074 CC IL16513… CC IL165… Male   Dead         2016-04…        103 2018-05… Found Dea… N      NULL      "Case 1-… 2018-0… CC W… Collabor… 2018-03-28 04:0… Mackenzie   NULL        2018-10-23 1… Blood 3  NULL      4/3/2016               103  22.3 3/CBC NA    Proparacai… W1G1   M             3571
#  6   3224     1251075 CC IL16513… CC IL165… Male   Dead         2016-04…        103 2018-07… Found Dea… R      NULL      "\"domed… 2018-0… CC W… Collabor… 2018-03-28 04:0… Mackenzie   NULL        2018-10-23 1… Blood 3  NULL      4/7/2016               103  26   3/CBC NA    Proparacai… W1G1   M             3571
#  7   3224     1251070 CC IL16513… CC IL165… Female Dead         2016-04…        102 2018-11… Found Dea… 2R     NULL      "distend… 2018-0… CC W… Collabor… 2018-03-28 04:0… Rachel      NULL        2018-10-23 1… Blood 3  NULL      4/11/2016              102  30.2 3/CBC NA    Proparacai… W1G1   F             3611
#  8   3224     1251071 CC IL16513… CC IL165… Female Dead         2016-04…        102 2018-05… Found Dea… 2L     NULL      "NULL"    2018-0… CC W… Collabor… 2018-03-28 04:0… Rachel      NULL        2018-10-23 1… Blood 3  NULL      4/11/2016              102  18.4 3/CBC NA    Proparacai… W1G1   F             3611
#  9   3224     1251072 CC IL16513… CC IL165… Female Dead         2016-04…        102 2018-04… Found Dea… 2R1L   NULL      "NULL"    2018-0… CC W… Collabor… 2018-03-28 04:0… Rachel      NULL        2018-10-23 1… Blood 3  NULL      4/11/2016              102  22.4 3/CBC NA    Proparacai… W1G1   F             3611
# 10   3224     1251076 CC IL16513… CC IL165… Male   Dead         2016-04…        102 2018-07… Found Dea… 2R1L   NULL      "exposed… 2018-0… CC W… Collabor… 2018-03-28 04:0… Mackenzie   NULL        2018-10-23 1… Blood 3  NULL      4/11/2016              102  25.9 3/CBC NA    Proparacai… W1G1   M             3571
# # … with 1,064 more rows, and 40 more variables: EN <chr>, Coat Color <chr>, GL <dbl>, Extra Plasma <chr>, Comments_1 <chr>, WBC x10^3/uL <dbl>, RBC x10^6/uL <dbl>, HGB g/dL <dbl>, HGBCell g/dL <dbl>, HCT % <dbl>, MCV fL <dbl>, MCH pg <dbl>, MCHC g/dL <dbl>, CH pg <dbl>, CHCM g/dL <dbl>, RDW % <dbl>,
# #   HDW g/dL <dbl>, PLT x10^3/uL <dbl>, PDW% % <dbl>, MPV fL <dbl>, MPM pg <dbl>, %NEUT <dbl>, %LYMPH <dbl>, %MONO <dbl>, %EOS <dbl>, %BASO <dbl>, %LUC <dbl>, %RETIC <dbl>, #RETIC 10^9cells/L <dbl>, CHR pg <dbl>, CHm pg <dbl>, #NEUT 10^3cells/ul <dbl>, #LYMPH 10^3cells/ul <dbl>, #MONO 10^3cells/ul <dbl>,
# #   #EOS 10^3cells/ul <dbl>, #BASO 10^3cells/ul <dbl>, #LUC 10^3cells/ul <dbl>, Clumps <dbl>, PLTCLM <chr>, SMPLCMNT <chr>

#####


################################################################################
# Convert -999 values to NA ####

REVISED_DATA <- REVISED_DATA %>% 
  mutate_if(is.numeric, convert_to_na)

#####


#####################################################################################
# Drop unnecessary columns ####

REVISED_DATA <- REVISED_DATA %>% 
  select(-c(
    LineName, Sex, AnimalStatus, DateBorn, AgeInWeeks, DateExit, ExitReason,
    Marker, HousingID, Comments,
    
    JobID, StudyName, CompletedBy, TaskName, `Born Dates`, `Age at Test`, Bleed,
    Box, Cohort, Sex_1, GL, `Extra Plasma`, PLTCLM, SMPLCMNT
  ))

#####


################################################################################
# Collapse comments columns ####

REVISED_DATA <- REVISED_DATA %>%
  mutate(
    TaskNotes = ifelse(
      is.na(TaskNotes), 'none',
      TaskNotes
    ),
    Comments_1 = ifelse(
      is.na(Comments_1), 'none',
      Comments_1
    ),
    Comments = paste0(
      paste0('Batch comments: ', TaskNotes),
      ' |  ',
      paste0('Sample comments: ', Comments_1)
    )
  ) %>%
  select(-c(
    TaskNotes, Comments_1
  ))

#####


################################################################################
# Rename columns ####

REVISED_DATA <- REVISED_DATA %>% 
  rename(
    MouseID = AnimalName,
    DateCollect = CollectionDateTime,
    Tech = CollectedBy,
    DateComplete = CompletedDate,
    HID_Test = `Pen Number`,
    BW_Test = BW,
    Coat_Test = `Coat Color`,
    EN_Test = EN,
    Anesthesia = `Eye Drops`
  ) %>% 
  rename(
    NumWBC = `WBC x10^3/uL`,
    NumRBC = `RBC x10^6/uL`,
    Hgb = `HGB g/dL`,
    CalcHgb = `HGBCell g/dL`,
    Hct = `HCT %`,
    MCV = `MCV fL`,
    MCH = `MCH pg`,
    MCHC = `MCHC g/dL`,
    CH = `CH pg`,
    CHCM = `CHCM g/dL`,
    RDWcv = `RDW %`,
    HDWsd = `HDW g/dL`,
    NumPlt = `PLT x10^3/uL`,
    PDWcv = `PDW% %`,
    MPV = `MPV fL`,
    MPM = `MPM pg`,
    PercNeut = `%NEUT`,
    PercLymph = `%LYMPH`,
    PercMono = `%MONO`,
    PercEos = `%EOS`,
    PercBaso = `%BASO`,
    PercLUC = `%LUC`,
    PercRetic = `%RETIC`,
    NumRetic = `#RETIC 10^9cells/L`,
    CHr = `CHR pg`,
    CHm = `CHm pg`,
    NumNeut = `#NEUT 10^3cells/ul`,
    NumLymph = `#LYMPH 10^3cells/ul`,
    NumMono = `#MONO 10^3cells/ul`,
    NumEos = `#EOS 10^3cells/ul`,
    NumBaso = `#BASO 10^3cells/ul`,
    NumLUC = `#LUC 10^3cells/ul`,
    NumClumps = `Clumps`
  ) %>% 
  select(
    OutputRowID, TaskID,
    MouseID, DateDue, DateCollect, DateComplete,
    Tech, Comments,
    HID_Test, Coat_Test, EN_Test, BW_Test,
    Anesthesia, NumClumps,
    NumRBC, NumRetic, PercRetic,
    Hct, Hgb, CalcHgb,
    MCV, CHCM, MCHC, CH, MCH, CHm, CHr, 
    RDWcv, HDWsd, 
    NumWBC, NumLymph, NumNeut, NumMono, NumEos, NumBaso, NumLUC,
    PercLymph, PercNeut, PercMono, PercEos, PercBaso, PercLUC,
    NumPlt, 
    MPV, MPM, 
    PDWcv,
    everything()
  )

#####


################################################################################
# Fix MouseID names #### 

REVISED_DATA <- REVISED_DATA %>% 
  mutate(MouseID = str_replace(MouseID, 'CC ', ''))

#####


################################################################################
# Convert dates to Date type ####

REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    DateDue = as.Date(DateDue, format = '%Y-%m-%d'),
    DateCollect = as.Date(DateCollect, format = '%Y-%m-%d'),
    DateComplete = as.Date(DateComplete, format = '%Y-%m-%d')
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
# Format coat color column ####

REVISED_DATA$Coat_Test %>% tolower() %>% tab()
# agouti     black  black hb     other wb agouti     white      <NA> 
#     23        96        26       787        12       129         1 

REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    Coat_Test = tolower(Coat_Test),
    Coat_Test = ifelse(
      is.na(Coat_Test), as.character(NA),
      ifelse(
        Coat_Test %in% c('agouti', 'other', 'wb agouti'), 'other',
        ifelse(
          Coat_Test %in% c('black', 'black hb'), 'black',
          ifelse(
            Coat_Test %in% c('white'), 'white',
            'ERROR!'
          )
        )
      )
    )
  )

REVISED_DATA$Coat_Test %>% tab()
# black other white  <NA> 
#   122   822   129     1 

#####


################################################################################
# Format ear notch column ####

REVISED_DATA$EN_Test %>% toupper() %>% tab()
#  2L 2L1R   2R 2R1L    B    L    N    R <NA> 
#   5    4   10   11  263  242  264  274    1 

REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    EN_Test = toupper(EN_Test)
  )

REVISED_DATA$EN_Test %>% tab()
#  2L 2L1R   2R 2R1L    B    L    N    R <NA> 
#   5    4   10   11  263  242  264  274    1 

#####


################################################################################
# Format anesthesia column ####

REVISED_DATA$Anesthesia %>% tolower() %>% tab()
#  prop        prop. proparacaine  proparcaine   tetracaine         <NA> 
#   250          153          441           35           40          155 

REVISED_DATA <- REVISED_DATA %>% 
  mutate(
    Anesthesia = tolower(Anesthesia),
    Anesthesia = ifelse(
      is.na(Anesthesia), NA,
      ifelse(
        substr(Anesthesia, 1, 1) == 'p', 'proparacaine',
        ifelse(
          substr(Anesthesia, 1, 1) == 't', 'tetracaine',
          'ERROR!'
        )
      )
    )
  )

REVISED_DATA$Anesthesia %>% tab()
# proparacaine   tetracaine         <NA> 
#          879           40          155 

#####


################################################################################
# arrange ####

REVISED_DATA <- REVISED_DATA %>% 
  arrange(
    substr(MouseID, nchar(MouseID) - 3, 999),
    DateCollect
  )

#####


################################################################################
# Save data ####

REVISED_DATA %>% 
  write_csv('data/FORMATTED_CBC_20210719.csv')
# # A tibble: 1,074 x 46
#    OutputRowID TaskID MouseID      DateDue    DateCollect DateComplete Tech   Comments                                                              HID_Test Coat_Test EN_Test BW_Test Anesthesia   NumClumps NumRBC NumRetic PercRetic   Hct   Hgb CalcHgb   MCV  CHCM  MCHC    CH   MCH   CHm   CHr RDWcv HDWsd NumWBC NumLymph NumNeut NumMono NumEos NumBaso NumLUC PercLymph PercNeut PercMono PercEos PercBaso PercLUC NumPlt   MPV   MPM PDWcv
#          <int>  <int> <chr>        <date>     <date>      <date>       <chr>  <chr>                                                                    <int> <chr>     <chr>     <dbl> <chr>            <dbl>  <dbl>    <dbl>     <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>   <dbl>   <dbl>  <dbl>   <dbl>  <dbl>     <dbl>    <dbl>    <dbl>   <dbl>    <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#  1     1252746   3307 IL16188-5000 2017-03-28 2017-03-28  NA           John   Batch comments: none |  Sample comments: CLUMPS_HIGH_EOS?                 3705 other     N          40.0 proparacaine      1241   9.31     404.      4.34  44.9  14.9    13.7  48.3  30.5  33    14.7  16    NA    16.4  14.6  2.06   3.15     1.9     0.62    0.09   0.51    0.02   0.02      60.4     19.8      2.7    16.2      0.5     0.5    601   6    1.29  47.4
#  2     1251082   3304 IL16188-5000 2018-03-27 2018-03-28  2018-10-23   Rachel Batch comments: none |  Sample comments: Tumors                           3705 other     N          44.4 proparacaine        60   4.3      861.     20.0   26.7   8.1     7.6  62.1  28.3  30.1  17.5  18.7  17.9  18.1  19.7  2.8   12.6      2.88    8.97    0.45   0.26    0.01   0.03      22.9     71.2      3.6     2        0.1     0.2    690   6.8  1.25  48.4
#  3     1252747   3307 IL16188-5001 2017-03-28 2017-03-28  NA           John   Batch comments: none |  Sample comments: CLUMPS_HIGH_EOS?                 3705 other     R          31.5 proparacaine      1516   9.77     481.      4.92  46.6  15.3    14    47.7  30.1  32.9  14.3  15.7  NA    16.4  15.2  2.04   3.72     1.87    0.86    0.14   0.82    0.05   0.04      50.2     23        3.7    22.1      1.3     1      561   6.2  1.33  50.4
#  4     1252748   3307 IL16188-5002 2017-03-28 2017-03-28  NA           John   Batch comments: none |  Sample comments: none                             3705 other     L          26.7 proparacaine       419   9.91     379.      3.83  47.6  15.5    14.2  48    29.9  32.6  14.3  15.7  NA    16.1  14    1.99   1.67     1.02    0.41    0.04   0.15    0.04   0.06      60.7     24.7      2.3     8.8      2.4     3.4    652   6    1.27  50.8
#  5     1251083   3304 IL16188-5002 2018-03-27 2018-03-28  2018-10-23   Rachel Batch comments: none |  Sample comments: Tumors                           3705 other     L          32.2 proparacaine        88   7.91     461.      5.82  39.4  12.3    11.8  49.8  29.8  31.1  14.8  15.5  15.2  17.4  19.5  2.33   8.72     1.65    6.54    0.36   0.14    0.02   0.02      18.9     75        4.1     1.6      0.3     0.2   1072   6.6  1.25  48.5
#  6     1252749   3307 IL16188-5003 2017-03-28 2017-03-28  NA           John   Batch comments: none |  Sample comments: CLUMPS_HIGH_EOS?                 3705 other     B          43.5 proparacaine      1289  10.2      177.      1.73  48.7  16.1    14.8  47.6  30.3  32.9  14.4  15.7  NA    15.5  13.8  1.99   2.26     0.8     0.63    0.08   0.73    0.01   0.01      35.6     27.9      3.5    32.5      0.6     0.4    583   6    1.3   45.3
#  7     1252750   3307 IL16188-5004 2017-03-28 2017-03-28  NA           John   Batch comments: none |  Sample comments: CLUMPS_HIGH_EOS?                 3708 other     N          24.3 proparacaine      1045   8.98     555.      6.18  44.6  14.4    13.6  49.7  30.4  32.2  15    16    NA    16.5  17.5  2.24   3.53     2.03    0.97    0.14   0.36    0.03   0.03      57.6     27.5      3.9    10.2      0.8     0.8    804   6.2  1.31  44.5
#  8     1252751   3307 IL16188-5005 2017-03-28 2017-03-28  NA           John   Batch comments: none |  Sample comments: No R Eye,CLUMPS_HIGH_EOS?        3708 other     R          23.4 proparacaine       608   9.54     359.      3.77  45.8  15.1    14    48    30.5  32.9  14.6  15.8  NA    17.4  16.2  1.99   1.99     0.96    0.57    0.06   0.38    0.01   0         48.4     28.8      2.8    19.1      0.7     0.2    679   5.9  1.36  42.3
#  9     1252752   3307 IL16188-5006 2017-03-28 2017-03-28  NA           John   Batch comments: none |  Sample comments: Small L Eye,CLUMPS_HIGH_EOS?     3708 other     L          31.4 proparacaine      1459   9.57     410.      4.29  47    15.1    13.7  49.2  29.2  32.1  14.3  15.8  NA    17.1  16.4  1.95   2.04     1.04    0.52    0.07   0.4     0.01   0.01      51.1     25.5      3.5    19.5      0.5     0.4    615   6.7  1.32  49.5
# 10     1251084   3304 IL16188-5006 2018-03-27 2018-03-28  2018-10-23   Rachel Batch comments: none |  Sample comments: R eye,CLUMPS_HIGH_EOS?           3708 other     L          30.0 proparacaine      1809   9.26     278.      3     43    14.4    12.9  46.4  30    33.6  13.9  15.6  14.3  15.8  12.9  1.87   2.37     0.54    0.89    0.08   0.85    0.01   0         22.9     37.3      3.5    35.7      0.5     0.1    862   6.4  1.34  47.1
# # … with 1,064 more rows

#####


################################################################################
## clear workspace ###########################################

rm(list = ls())
pacman::p_unload('all')
graphics.off()

#####

