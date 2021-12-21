

################################################################################
# Load libraries etc ####

options(stringsAsFactors = FALSE)
library(lubridate)
library(tidyverse)

STRAIN_COLORS <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ca9577','#b15928')
SEX_COLORS <- c('royalblue4', 'darkorange3')
DIET_COLORS <- c('gray50', 'gray10')
SEXDIET_COLORS <- c('royalblue1', 'navy', 'darkorange1', 'darkorange4')

#####


################################################################################
# Helper functions ####

tab <- function(...,
                useNA = 'ifany'){
  x <- table(... = ...,
             useNA = useNA)
  return(x)
}

visualize_colpal <- function(colors){
  image(1:length(colors), 1,
        as.matrix(1:length(colors)),
        col=colors,
        xlab="", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}



#####


################################################################################
# Load data ####

ANIMAL_DATA <- read_csv(
  'data/animal_data_processed_20210719.csv'
) %>% 
  mutate(
    Strain = factor(Strain, levels = sort(unique(Strain))),
    Sex = factor(Sex, levels = c('Female', 'Male')),
    Diet = factor(Diet, levels = c('AL', 'IF')),
    BWDay = factor(
      BWDay,
      levels = c('Tuesday', 'Wednesday', 'Thursday', 'Friday')
    )
  ) %>% 
  select(
    MouseID, Diet, Strain, StrainAKA, Sex, BWDay, HID, 
    DOB, DOE, COE, SurvDays, Died
  ) %>% 
  filter(SurvDays >= 6*30.4)
# # A tibble: 768 x 12
#    MouseID      Diet  Strain        StrainAKA Sex    BWDay       HID DOB        DOE        COE             SurvDays Died 
#    <chr>        <fct> <fct>         <chr>     <fct>  <fct>     <dbl> <date>     <date>     <chr>              <dbl> <lgl>
#  1 OR3609-5979  IF    CC018/UncJ    OR3609    Male   Wednesday  6598 2017-11-08 2019-03-11 Found Dead (FD)      488 TRUE 
#  2 OR3609-5978  IF    CC018/UncJ    OR3609    Male   Wednesday  6598 2017-11-08 2019-09-22 Found Dead (FD)      683 TRUE 
#  3 IL16557-5478 IF    CC040/TauUncJ IL16557   Male   Wednesday  6573 2017-09-26 2020-02-01 Found Dead (FD)      858 TRUE 
#  4 IL16557-5479 IF    CC040/TauUncJ IL16557   Male   Wednesday  6573 2017-09-26 2019-12-18 Found Dead (FD)      813 TRUE 
#  5 IL16557-5471 IF    CC040/TauUncJ IL16557   Female Wednesday  6569 2017-09-26 2018-08-31 Found Dead (FD)      339 TRUE 
#  6 IL16557-5472 AL    CC040/TauUncJ IL16557   Male   Wednesday  6570 2017-09-11 2019-04-22 Found Dead (FD)      588 TRUE 
#  7 IL16557-5473 AL    CC040/TauUncJ IL16557   Male   Wednesday  6570 2017-09-11 2019-01-14 Found Dead (FD)      490 TRUE 
#  8 IL16557-5474 AL    CC040/TauUncJ IL16557   Male   Wednesday  6570 2017-09-11 2019-04-22 Found Dead (FD)      588 TRUE 
#  9 IL16557-5475 AL    CC040/TauUncJ IL16557   Male   Wednesday  6571 2017-09-26 2019-01-21 Found Dead (FD)      482 TRUE 
# 10 IL16557-5476 IF    CC040/TauUncJ IL16557   Male   Wednesday  6573 2017-09-26 2019-04-29 Found Dead (FD)      580 TRUE 
# # … with 758 more rows

ANIMAL_DATA$Died %>% tab
# FALSE  TRUE 
#     1   767 

ANIMAL_DATA <- ANIMAL_DATA %>% 
  filter(Died)

BW_DATA <- read_csv(
  'data/SpeedCleaned_BW_20210719.csv',
  col_types = 'cicccDDDinn'
) %>% 
  filter(
    MouseID %in% ANIMAL_DATA$MouseID
  ) %>% 
  select(
    MouseID, DateCollect, AgeInDays, BW_LOESS
  )
# # A tibble: 66,567 x 4
#    MouseID      DateCollect AgeInDays BW_LOESS
#    <chr>        <date>          <int>    <dbl>
#  1 IL16188-5000 2016-06-02         39     20.8
#  2 IL16188-5000 2016-06-09         46     21.7
#  3 IL16188-5000 2016-06-16         53     22.6
#  4 IL16188-5000 2016-06-23         60     23.5
#  5 IL16188-5000 2016-06-30         67     24.3
#  6 IL16188-5000 2016-07-07         74     25.1
#  7 IL16188-5000 2016-07-14         81     25.9
#  8 IL16188-5000 2016-07-21         88     26.7
#  9 IL16188-5000 2016-07-28         95     27.5
# 10 IL16188-5000 2016-08-04        102     28.2
# # … with 66,557 more rows

BWANIMAL_DATA <- ANIMAL_DATA %>% 
  select(
    MouseID, Diet, Strain, Sex, HID, SurvDays, COE, Died
  ) %>% 
  left_join(
    BW_DATA %>% 
      select(
        MouseID, AgeInDays, BW_LOESS
      ),
    by = 'MouseID'
  ) %>% 
  mutate(
    PLL = AgeInDays / SurvDays * 100,
    AgeInMonths = AgeInDays / 30.4
  ) %>% 
  select(
    MouseID:AgeInDays, AgeInMonths, PLL, BW_LOESS
  )
# # A tibble: 66,567 x 12
#    MouseID     Diet  Strain     Sex     HID SurvDays COE             Died  AgeInDays AgeInMonths   PLL BW_LOESS
#    <chr>       <fct> <fct>      <fct> <dbl>    <dbl> <chr>           <lgl>     <int>       <dbl> <dbl>    <dbl>
#  1 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE         49        1.61  10.0     22.8
#  2 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE         70        2.30  14.3     24.0
#  3 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE         77        2.53  15.8     24.4
#  4 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE         84        2.76  17.2     24.8
#  5 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE         92        3.03  18.9     25.3
#  6 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE         98        3.22  20.1     25.6
#  7 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE        106        3.49  21.7     26.0
#  8 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE        119        3.91  24.4     26.6
#  9 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE        127        4.18  26.0     27.0
# 10 OR3609-5979 IF    CC018/UncJ Male   6598      488 Found Dead (FD) TRUE        133        4.38  27.3     27.4
# # … with 66,557 more rows
  

#####


################################################################################
# Diet summary plot ####

## By age in months

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(AgeInMonths)
  ) %>% 
  group_by(
    Diet, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Diet, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 5
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Diet, group = Diet),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Diet, group = Diet),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,60,6),
    minor_breaks = seq(0,60,2),
    limits = c(0,34.1),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(
    values = DIET_COLORS,
    labels = c('Ad libitum', '2-day fast')
  ) +
  scale_fill_manual(
    values = DIET_COLORS,
    labels = c('Ad libitum', '2-day fast')
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'Age (months)',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  )

pdf(
  'analysis/bodyweight/figures/BWbyAge_Summary_Diet.pdf',
  width = 7, height = 5
)
plot(PLOT)
dev.off()


## By percent of life lived

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(PLL/5)*5
  ) %>% 
  group_by(
    Diet, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Diet, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 5
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Diet, group = Diet),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Diet, group = Diet),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,100,10),
    minor_breaks = seq(0,100,5),
    limits = c(0,100),
    expand = c(0,0.5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(
    values = DIET_COLORS,
    labels = c('Ad libitum', '2-day fast')
  ) +
  scale_fill_manual(
    values = DIET_COLORS,
    labels = c('Ad libitum', '2-day fast')
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'Percent of life lived',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyPLL_Summary_Diet.pdf',
  width = 7, height = 5
)
plot(PLOT)
dev.off()


rm(PLOT_DATA, PLOT)

#####


################################################################################
# Sex summary plot ####

## By age in months

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(AgeInMonths)
  ) %>% 
  group_by(
    Sex, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Sex, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 5
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Sex, group = Sex),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Sex, group = Sex),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,60,6),
    minor_breaks = seq(0,60,2),
    limits = c(0,34.1),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(
    values = SEX_COLORS,
    labels = c('Female', 'Male')
  ) +
  scale_fill_manual(
    values = SEX_COLORS,
    labels = c('Female', 'Male')
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'Age (months)',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyAge_Summary_Sex.pdf',
  width = 7, height = 5
)
plot(PLOT)
dev.off()


## By percent of life lived

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(PLL/5)*5
  ) %>% 
  group_by(
    Sex, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Sex, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 5
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Sex, group = Sex),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Sex, group = Sex),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,100,10),
    minor_breaks = seq(0,100,5),
    limits = c(0,100),
    expand = c(0,0.5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(
    values = SEX_COLORS,
    labels = c('Female', 'Male')
  ) +
  scale_fill_manual(
    values = SEX_COLORS,
    labels = c('Female', 'Male')
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'Percent of life lived',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyPLL_Summary_Sex.pdf',
  width = 7, height = 5
)
plot(PLOT)
dev.off()


rm(PLOT_DATA, PLOT)

#####


################################################################################
# Diet-Sex summary plot ####

## By age in months

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    SexDiet = paste0(Sex, ':', Diet),
    SexDiet = factor(
      SexDiet,
      levels = c('Female:AL', 'Female:IF', 'Male:AL', 'Male:IF')
    )
  ) %>% 
  mutate(
    Age = round(AgeInMonths)
  ) %>% 
  group_by(
    SexDiet, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    SexDiet, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 5
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = SexDiet, group = SexDiet),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = SexDiet, group = SexDiet),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,60,6),
    minor_breaks = seq(0,60,2),
    limits = c(0,34.1),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(
    values = SEXDIET_COLORS,
    labels = c(
      'Female: Ad libitum', 'Female: 2-day fast',
      'Male: Ad libitum', 'Male: 2-day fast'
    )
  ) +
  scale_fill_manual(
    values = SEXDIET_COLORS,
    labels = c(
      'Female: Ad libitum', 'Female: 2-day fast',
      'Male: Ad libitum', 'Male: 2-day fast'
    )
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'Age (months)',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyAge_Summary_SexAndDiet.pdf',
  width = 7, height = 5
)
plot(PLOT)
dev.off()


## By percent of life lived

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    SexDiet = paste0(Sex, ':', Diet),
    SexDiet = factor(
      SexDiet,
      levels = c('Female:AL', 'Female:IF', 'Male:AL', 'Male:IF')
    )
  ) %>% 
  mutate(
    Age = round(PLL/5)*5
  ) %>% 
  group_by(
    SexDiet, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    SexDiet, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 5
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = SexDiet, group = SexDiet),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = SexDiet, group = SexDiet),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,100,10),
    minor_breaks = seq(0,100,5),
    limits = c(0,100),
    expand = c(0,0.5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(
    values = SEXDIET_COLORS,
    labels = c(
      'Female: Ad libitum', 'Female: 2-day fast',
      'Male: Ad libitum', 'Male: 2-day fast'
    )
  ) +
  scale_fill_manual(
    values = SEXDIET_COLORS,
    labels = c(
      'Female: Ad libitum', 'Female: 2-day fast',
      'Male: Ad libitum', 'Male: 2-day fast'
    )
  ) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'Percent of life lived',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyPLL_Summary_SexAndDiet.pdf',
  width = 7, height = 5
)
plot(PLOT)
dev.off()


rm(PLOT_DATA, PLOT)

#####


################################################################################
# Strain summary plot ####

## By age in months

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(AgeInMonths)
  ) %>% 
  group_by(
    Strain, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Strain, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 3
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Strain, group = Strain),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Strain, group = Strain),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,60,6),
    minor_breaks = seq(0,60,2),
    limits = c(0,34.1),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(values = STRAIN_COLORS) +
  scale_fill_manual(values = STRAIN_COLORS) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'Age (months)',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyAge_Summary_Strain.pdf',
  width = 7, height = 5
)
plot(PLOT)
dev.off()


## By percent of life lived

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(PLL/5)*5
  ) %>% 
  group_by(
    Strain, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Strain, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 3
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Strain, group = Strain),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Strain, group = Strain),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,100,10),
    minor_breaks = seq(0,100,5),
    limits = c(0,100),
    expand = c(0,0.5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(values = STRAIN_COLORS) +
  scale_fill_manual(values = STRAIN_COLORS) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'Percent of life lived',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyPLL_Summary_Strain.pdf',
  width = 7, height = 5
)
plot(PLOT)
dev.off()


rm(PLOT_DATA, PLOT)

#####


################################################################################
# Strain-diet summary plot ####

## By age in months

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(AgeInMonths),
    Diet = factor(
      ifelse(
        Diet == 'AL', 'Ad libitum',
        '2-day fast'
      ),
      levels = c('Ad libitum', '2-day fast')
    )
  ) %>% 
  group_by(
    Strain, Diet, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Strain, Diet, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 2
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Strain, group = Strain),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Strain, group = Strain),
    alpha = 0.5
  ) +
  facet_wrap(
    . ~ Diet,
    ncol = 2,
  ) +
  scale_x_continuous(
    breaks = seq(0,60,6),
    minor_breaks = seq(0,60,2),
    limits = c(0,34.1),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(values = STRAIN_COLORS) +
  scale_fill_manual(values = STRAIN_COLORS) +
  theme(
    legend.position = 'top',
    panel.spacing.x = unit(2, 'lines')
  ) +
  labs(
    x = 'Age (months)',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyAge_Summary_StrainAndDiet.pdf',
  width = 12, height = 5
)
plot(PLOT)
dev.off()


## By percent of life lived

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(PLL/5)*5,
    Diet = factor(
      ifelse(
        Diet == 'AL', 'Ad libitum',
        '2-day fast'
      ),
      levels = c('Ad libitum', '2-day fast')
    )
  ) %>% 
  group_by(
    Strain, Diet, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Strain, Diet, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 2
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Strain, group = Strain),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Strain, group = Strain),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,100,10),
    minor_breaks = seq(0,100,5),
    limits = c(0,100),
    expand = c(0,0.5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  facet_wrap(
    . ~ Diet,
    ncol = 2,
  ) +
  scale_color_manual(values = STRAIN_COLORS) +
  scale_fill_manual(values = STRAIN_COLORS) +
  theme(
    legend.position = 'top',
    panel.spacing.x = unit(2, 'lines')
  ) +
  labs(
    x = 'Percent of life lived',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyPLL_Summary_StrainAndDiet.pdf',
  width = 12, height = 5
)
plot(PLOT)
dev.off()


rm(PLOT_DATA, PLOT)

#####


################################################################################
# Strain-sex summary plot ####

## By age in months

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(AgeInMonths)
  ) %>% 
  group_by(
    Strain, Sex, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Strain, Sex, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 2
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Strain, group = Strain),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Strain, group = Strain),
    alpha = 0.5
  ) +
  facet_wrap(
    . ~ Sex,
    ncol = 2,
  ) +
  scale_x_continuous(
    breaks = seq(0,60,6),
    minor_breaks = seq(0,60,2),
    limits = c(0,34.1),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(values = STRAIN_COLORS) +
  scale_fill_manual(values = STRAIN_COLORS) +
  theme(
    legend.position = 'top',
    panel.spacing.x = unit(2, 'lines')
  ) +
  labs(
    x = 'Age (months)',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyAge_Summary_StrainAndSex.pdf',
  width = 12, height = 5
)
plot(PLOT)
dev.off()


## By percent of life lived

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(PLL/5)*5
  ) %>% 
  group_by(
    Strain, Sex, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Strain, Sex, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 2
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Strain, group = Strain),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Strain, group = Strain),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,100,10),
    minor_breaks = seq(0,100,5),
    limits = c(0,100),
    expand = c(0,0.5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  facet_wrap(
    . ~ Sex,
    ncol = 2,
  ) +
  scale_color_manual(values = STRAIN_COLORS) +
  scale_fill_manual(values = STRAIN_COLORS) +
  theme(
    legend.position = 'top',
    panel.spacing.x = unit(2, 'lines')
  ) +
  labs(
    x = 'Percent of life lived',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyPLL_Summary_StrainAndSex.pdf',
  width = 12, height = 5
)
plot(PLOT)
dev.off()


rm(PLOT_DATA, PLOT)

#####


################################################################################
# Strain-sex-diet summary plot ####

## By age in months

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(AgeInMonths),
    Diet = factor(
      ifelse(
        Diet == 'AL', 'Ad libitum',
        '2-day fast'
      ),
      levels = c('Ad libitum', '2-day fast')
    )
  ) %>% 
  group_by(
    Strain, Sex, Diet, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Strain, Sex, Diet, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 2
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Strain, group = Strain),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Strain, group = Strain),
    alpha = 0.5
  ) +
  facet_grid(
    Diet ~ Sex
  ) +
  scale_x_continuous(
    breaks = seq(0,60,6),
    minor_breaks = seq(0,60,2),
    limits = c(0,34.1),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  scale_color_manual(values = STRAIN_COLORS) +
  scale_fill_manual(values = STRAIN_COLORS) +
  theme(
    legend.position = 'top',
    panel.spacing = unit(2, 'lines')
  ) +
  labs(
    x = 'Age (months)',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyAge_Summary_StrainAndSexAndDiet.pdf',
  width = 12, height = 8
)
plot(PLOT)
dev.off()


## By percent of life lived

PLOT_DATA <- BWANIMAL_DATA %>% 
  mutate(
    Age = round(PLL/5)*5,
    Diet = factor(
      ifelse(
        Diet == 'AL', 'Ad libitum',
        '2-day fast'
      ),
      levels = c('Ad libitum', '2-day fast')
    )
  ) %>% 
  group_by(
    Strain, Sex, Diet, Age, MouseID
  ) %>% 
  summarise(
    BW_LOESS = mean(BW_LOESS)
  ) %>% 
  group_by(
    Strain, Sex, Diet, Age
  ) %>% 
  summarise(
    N = n(),
    Mean = mean(BW_LOESS),
    Var = var(BW_LOESS)
  ) %>% 
  ungroup() %>% 
  mutate(
    SE = sqrt(Var/N)
  ) %>% 
  filter(
    N > 2
  )

PLOT <- PLOT_DATA %>% 
  ggplot() +
  theme_minimal() +
  geom_line(
    aes(x = Age, y = Mean, color = Strain, group = Strain),
    alpha = 0.75
  ) +
  geom_ribbon(
    aes(x = Age, ymin = Mean - SE, ymax = Mean + SE, fill = Strain, group = Strain),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0,100,10),
    minor_breaks = seq(0,100,5),
    limits = c(0,100),
    expand = c(0,0.5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = seq(0,100,2),
    limits = c(0, 49),
    expand = c(0,0)
  ) +
  facet_grid(
    Diet ~ Sex
  ) +
  scale_color_manual(values = STRAIN_COLORS) +
  scale_fill_manual(values = STRAIN_COLORS) +
  theme(
    legend.position = 'top',
    panel.spacing = unit(2, 'lines')
  ) +
  labs(
    x = 'Percent of life lived',
    y = 'Bodyweight (grams)',
    color = NULL,
    fill = NULL
  ); PLOT

pdf(
  'analysis/bodyweight/figures/BWbyPLL_Summary_StrainAndSexAndDiet.pdf',
  width = 13, height = 8
)
plot(PLOT)
dev.off()


rm(PLOT_DATA, PLOT)

#####


################################################################################
#  ####


#####


################################################################################
# clear workspace  ####

rm(list = ls())
pacman::p_unload('all')
graphics.off()

#####