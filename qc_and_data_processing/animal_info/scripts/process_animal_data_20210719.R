
setwd('/Users/deigha/Documents/Projects/CC_Longitudinal_IF/qc_and_data_processing/animal_info/')

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

ANIMAL <- read_csv(
  'data/animals_20210719.csv'
)
ANIMAL_BULK <- read_csv(
  'data/AnimalBulkExport_20210719.csv'
)

#####


################################################################################
# Drop unnecessary columns ####

ANIMAL_BULK <- ANIMAL_BULK %>% 
  select(-c(
    ID, `Study Name`, Cohorts, `Age (weeks)`,
  ))

#####


################################################################################
# Rename columns ####

ANIMAL <- ANIMAL %>% 
  rename(
    MouseID = Name,
    StrainAKA = Line,
    Strain = `Line (Short)`,
  )

ANIMAL_BULK <- ANIMAL_BULK %>% 
  rename(
    MouseID = `Animal Name`,
    EarNotch = Marker,
    AnimalComments = Comment,
    DOB = `Birth Date`,
    DOE = `Death/Exit Date`,
    COE = `Death/Exit Reason`,
    FHID = `First Housing ID`,
    LHID = `Last Housing ID`,
    Coat = `Coat Color`
  )

#####


################################################################################
# Format strain and mouse ID columns ####

ANIMAL <- ANIMAL %>% 
  mutate(
    MouseID = str_replace(MouseID, 'CC ', ''),
    StrainAKA = str_replace(StrainAKA, 'CC ', ''),
    Strain = str_replace(Strain, 'CC ', 'CC')
  )

ANIMAL_BULK <- ANIMAL_BULK %>% 
  mutate(
    MouseID = str_replace(MouseID, 'CC ', '')
  )

#####


################################################################################
# Format cohort, job group, and BW day columns  ####

ANIMAL <- ANIMAL %>% 
  mutate(
    Cohort = str_replace(
      Job,
      '^CC[[:print:]]* (W[[:digit:]]G[[:digit:]]) [[:print:]]+', 
      '\\1'
    ),
    JobGroup = paste0(
      Cohort,
      ' ',
      str_replace(
        Job, 
        '^CC[[:print:]]* (Day [[:print:]])[[:print:]]*', 
        '\\1'
      )
    ),
    BWDay = str_replace(
      Job,
      '^CC[[:print:]]* ([[:alpha:]]+) BW[[:print:]]*', 
      '\\1'
    )
  ) %>% 
  select(
    -Job
  )

#####


################################################################################
# Format diet names ####

ANIMAL <- ANIMAL %>% 
  mutate(
    Diet = ifelse(Diet == '6% Extruded AL', 'AL', 'IF')
  )

#####


################################################################################
# Fix ear notch column #### 

ANIMAL_BULK$EarNotch %>% toupper() %>% tab()
#  2L 2L1R   2R 2R1L    B    L    N    R <NA> 
#   5    3   15    6  181  190  200  197    3 

ANIMAL_BULK %>% filter(is.na(EarNotch)) %>% data.frame()
#        MouseID EarNotch  Sex Status Generation                                    AnimalComments        DOB        DOE             COE FHID LHID Coat
# 1 IL16188-5056     <NA> Male   Dead        F07                                              <NA> 05/04/2017 09/18/2018 Found Dead (FD) 5974 5974 <NA>
# 2  OR3609-5978     <NA> Male   Dead        F06 found dead during wheel running testing 9/22/19\n 11/08/2017 09/22/2019 Found Dead (FD) 6598 6598 <NA>
# 3  OR3609-5979     <NA> Male   Dead        F06                                               REN 11/08/2017 03/11/2019 Found Dead (FD) 6598 6598 <NA>

ANIMAL_BULK <- ANIMAL_BULK %>% 
  mutate(
    EarNotch = toupper(EarNotch),
    EarNotch = ifelse(
      MouseID == 'OR3609-5979', 'R', EarNotch
    )
  )

ANIMAL_BULK$EarNotch %>% tab()
# 2L 2L1R   2R 2R1L    B    L    N    R <NA> 
#  5    3   15    6  181  190  200  198    2 

#####


################################################################################
# Format coat color column #### 

ANIMAL_BULK$Coat %>% tolower() %>% tab()
# black other  <NA> 
#    43   287   470 

ANIMAL_BULK <- ANIMAL_BULK %>% 
  mutate(
    Coat = tolower(Coat)
  )

#####


################################################################################
# Get housing ID data from the bulk export  ####

ANIMAL <- ANIMAL %>% 
  full_join(
    ANIMAL_BULK %>% 
      select(
        MouseID, FHID, LHID
      ),
    by = 'MouseID'
  )

ANIMAL_BULK <- ANIMAL_BULK %>% 
  select(-c(
    FHID, LHID
  ))

# See Hannah's email on 2020-08-04. Pens with less than 4 mice are real, pens
# with more than 4 mice represent errors that were quickly fixed. Two ad 
# libitum mice were added to the intermittent fasting cage 6423 accidently, but
# then removed on the same day. Pens 6064 and 6143 had 5 mice in them originally
# but were split up and assigned diets when we started them on the study. 
# Therefore, the diets assigned for the study are accurate. For these three 
# pens, the issue can be fixed by using the "last" housing ID (LHID) as the
# housing ID.

(
  X <- ANIMAL %>%
    group_by(FHID) %>%
    summarise(N = n()) %>%
    ungroup() %>%
    filter(N > 4) %>%
    arrange(desc(N)) %>%
    data.frame()
)
#   FHID N
# 1 6423 6
# 2 6064 5
# 3 6143 5

ANIMAL %>% 
  filter(FHID %in% X$FHID) %>% 
  group_by(FHID, Strain, Diet) %>% 
  summarise(
    N = n(),
    LHID = paste(unique(LHID), collapse = '; ')
  ) %>% 
  arrange(FHID, desc(N)) %>% 
  data.frame()
#   FHID        Strain Diet N LHID
# 1 6064    CC018/UncJ   IF 3 6064
# 2 6064    CC018/UncJ   AL 2 6193
# 3 6143    CC018/UncJ   IF 3 6143
# 4 6143    CC018/UncJ   AL 2 6192
# 5 6423 CC005/TauUncJ   IF 4 6423
# 6 6423 CC005/TauUncJ   AL 2 6421

ANIMAL <- ANIMAL %>% 
  mutate(
    HID = ifelse(
      FHID %in% c(6423, 6064, 6143),
      LHID,
      FHID
    )
  ) %>% 
  select(
    -FHID
  )

(
  X <- ANIMAL %>%
    group_by(HID) %>%
    summarise(N = n()) %>%
    ungroup() %>%
    filter(N > 4) %>%
    arrange(desc(N)) %>%
    data.frame()
)
# <0 rows> 

rm(X)

#####


################################################################################
# Check obvious date errors ####

ANIMAL_BULK <- ANIMAL_BULK %>% 
  mutate(
    DOB = as.Date(DOB, format = '%m/%d/%Y'),
    DOE = as.Date(DOE, format = '%m/%d/%Y')
  )

# No mice are missing their birth date, exit date, or reason for exit
ANIMAL_BULK %>% 
  filter(
    is.na(DOB) | 
      is.na(DOE) |
      is.na(COE)
  ) %>% 
  nrow()
# [1] 0

# The birth date is not greater than the death date for any mice
ANIMAL_BULK %>% 
  filter(DOB > DOE) %>% 
  nrow()
# [1] 0

# The birth date is not greater than the current date for any mice
ANIMAL_BULK %>% 
  filter(DOB > Sys.Date()) %>% 
  nrow()
# [1] 0

# The eixt date is not greater than the current date for any mice (study done)
ANIMAL_BULK %>% 
  filter(DOE > Sys.Date()) %>% 
  nrow()
# [1] 0

#####


################################################################################
# Determine if mice died or were censored ####

# "Missing" means that the mouse was absent from the cage, the animal care
# technicians report that this generally means the mouse died and then its
# corpse was eaten by its housemates. Perhaps this explains why there are more
# missing mice in the intermittent fasting group (26) than the ad libitum group
# (15). Failed to recover means the mouse did not recover from anesthesia (some
# of the assays require the mouse to anesthesitzed). Mishandled means the mouse
# died accidentally due to handling by animal care technicians. For the purpose
# of survival analysis, I will treat all exit reasons except "Mishandled" as 
# deaths

ANIMAL_BULK$COE %>% tab()
# Euthanized Sick (ES)   Failed to Recover (FTR)           Found Dead (FD) 
#                  37                         1                       703 
#     Mishandled (MSH)             Missing (MSG) Requested Euthanasia (RE) 
#                   2                        41                        16 

ANIMAL %>% 
  select(
    MouseID, Diet
  ) %>% 
  full_join(
    ANIMAL_BULK %>% 
      select(
        MouseID, COE
      )
  ) %>% 
  group_by(
    Diet, COE
  ) %>% 
  summarise(
    N = n()
  ) %>%
  pivot_wider(
    names_from = 'Diet',
    values_from = 'N'
  ) %>% 
  data.frame()
#                         COE  AL  IF
# 1      Euthanized Sick (ES)  26  11
# 2           Found Dead (FD) 346 357
# 3             Missing (MSG)  15  26
# 4 Requested Euthanasia (RE)  13   3
# 5   Failed to Recover (FTR)  NA   1
# 6          Mishandled (MSH)  NA   2

ANIMAL_BULK <- ANIMAL_BULK %>% 
  mutate(
    SurvDays = as.numeric(difftime(DOE, DOB, units = 'days')),
    Died = !COE %in% c('Mishandled (MSH)')
  ) %>% 
  select(-Status)

#####


################################################################################
# Join data and order columns ####

FULL_ANIMAL_DATA <- ANIMAL %>% 
  full_join(
    ANIMAL_BULK,
    by = 'MouseID'
  ) %>% 
  select(
    MouseID, 
    Diet, Strain, StrainAKA, Sex,
    Generation,
    Cohort, JobGroup, BWDay, HID, LHID,
    EarNotch, Coat, AnimalComments,
    DOB, DOE, COE, SurvDays, Died
  )

#####


################################################################################
# Save data ####

FULL_ANIMAL_DATA %>% 
  write_csv(
    'data/animal_data_processed_20210719.csv'
  )
# # A tibble: 800 x 19
#    MouseID      Diet  Strain        StrainAKA Sex    Generation Cohort JobGroup   BWDay       HID  LHID EarNotch Coat  AnimalComments                                            DOB        DOE        COE             SurvDays Died 
#    <chr>        <chr> <chr>         <chr>     <chr>  <chr>      <chr>  <chr>      <chr>     <dbl> <dbl> <chr>    <chr> <chr>                                                     <date>     <date>     <chr>              <dbl> <lgl>
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
# some plots ####

median_confint <- function(x, ...){
  M <- median(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE, ...)
  N <- sum(!is.na(x))
  CI <- c(
    M - 1.58*iqr/sqrt(N),
    M + 1.58*iqr/sqrt(N)
  )
  return(CI)
}


EighteenDecentColors <- c(
  'darkolivegreen', 'darkolivegreen3', 'seagreen', 'mediumaquamarine', 
  'cadetblue2', 'lightskyblue', 'dodgerblue', 'royalblue2', 'royalblue4', 'navyblue', 
  'darkorchid4', 'orchid4', 
  'firebrick', 'tomato2', 'darksalmon', 
  'orange', 'darkorange', 'darkgoldenrod'
)

pdf(
  'figures/male_and_female_lifespan_by_diet.pdf',
  width = 6.5, height = 4.5
)
FULL_ANIMAL_DATA %>% 
  mutate(
    SurvDays = SurvDays/30.4
  ) %>% 
  group_by(
    Sex, Strain, Diet
  ) %>% 
  summarise(
    Median = median(SurvDays, na.rm = TRUE),
    MedLo = median_confint(SurvDays)[1],
    MedHi = median_confint(SurvDays)[2]
  ) %>% 
  ungroup() %>%
  pivot_longer(
    cols = Median:MedHi,
    names_to = 'K', 
    values_to = 'V'
  ) %>% 
  unite(K, K, Sex) %>% 
  pivot_wider(
    names_from = 'K', 
    values_from = 'V'
  ) %>% 
  ggplot(
    aes(x = Median_Female, y = Median_Male, color = Strain)
  ) +
  theme_minimal(
    base_size = 9
  ) +
  geom_abline(
    intercept = 0, slope = 1
  ) +
  geom_point(
    alpha = 0.5
  ) +
  geom_errorbarh(
    aes(xmin = MedLo_Female, xmax = MedHi_Female), 
    height = 0.25,
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(ymin = MedLo_Male, ymax = MedHi_Male), 
    width = 0.25,
    alpha = 0.5
  ) +
  scale_color_manual(
    values = EighteenDecentColors[c(1,2,3, 7,10, 11, 13, 15, 17, 18)]
  ) +
  scale_x_continuous(
    limits = c(10, 32),
    exp = c(0, 0),
    breaks = seq(0, 120, 6),
    minor_breaks = seq(0, 120, 2)
  ) +
  scale_y_continuous(
    limits = c(10, 32),
    exp = c(0, 0),
    breaks = seq(0, 120, 6),
    minor_breaks = seq(0, 120, 2)
  ) +
  facet_grid(
    . ~ Diet
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    title = 'Median lifespan by strain, sex, and diet',
    x = 'Median female lifespan (months)',
    y = 'Median male lifespan (months)',
    caption = 'Under the ad libitum diet there does not appear to be a sex effect, but under the intermittent fasting diet males appear to live longer than females'
  )
dev.off()


pdf(
  'figures/AL_and_IF_lifespan_by_sex.pdf',
  width = 6.5, height = 4.5
)
FULL_ANIMAL_DATA %>% 
  mutate(
    SurvDays = SurvDays/30.4
  ) %>% 
  group_by(
    Sex, Strain, Diet
  ) %>% 
  summarise(
    Median = median(SurvDays, na.rm = TRUE),
    MedLo = median_confint(SurvDays)[1],
    MedHi = median_confint(SurvDays)[2]
  ) %>% 
  ungroup() %>%
  pivot_longer(
    cols = Median:MedHi,
    names_to = 'K', 
    values_to = 'V'
  ) %>% 
  unite(K, K, Diet) %>% 
  pivot_wider(
    names_from = 'K', 
    values_from = 'V'
  ) %>% 
  ggplot(
    aes(x = Median_AL, y = Median_IF, color = Strain)
  ) +
  theme_minimal(
    base_size = 9
  ) +
  geom_abline(
    intercept = 0, slope = 1
  ) +
  geom_point(
    alpha = 0.5
  ) +
  geom_errorbarh(
    aes(xmin = MedLo_AL, xmax = MedHi_AL), 
    height = 2,
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(ymin = MedLo_IF, ymax = MedHi_IF), 
    width = 2,
    alpha = 0.5
  ) +
  scale_color_manual(
    values = EighteenDecentColors[c(1,2,3, 7,10, 11, 13, 15, 17, 18)]
  ) +
  scale_x_continuous(
    limits = c(10, 32),
    exp = c(0, 0),
    breaks = seq(0, 120, 6),
    minor_breaks = seq(0, 120, 2)
  ) +
  scale_y_continuous(
    limits = c(10, 32),
    exp = c(0, 0),
    breaks = seq(0, 120, 6),
    minor_breaks = seq(0, 120, 2)
  ) +
  facet_grid(
    . ~ Sex
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    title = 'Median lifespan by strain, sex, and diet',
    x = 'Median AL lifespan (months)',
    y = 'Median IF lifespan (months)',
    caption = 'The effect of intermittent fasting on lifespan appears to be more pronounces in males than females'
  )
dev.off()

#####


################################################################################
## clear workspace ###########################################

rm(list = ls())
pacman::p_unload('all')
graphics.off()

#####