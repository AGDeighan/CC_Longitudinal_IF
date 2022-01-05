
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

quant_test <- function(response, group, covariates = NULL, quant, data, ...){
  if(length(covariates) > 0){
    NAMES <- c('Response', 'Group', paste0('COV', 1:length(covariates)))
  } else{
    NAMES <- c('Response', 'Group')
  }
  DATA <- data[,c(response, group, covariates)]
  DATA$Group <- data[[group]] 
  DATA$Response <- data[[response]] 
  
  DATA <- DATA %>% 
    group_by_at(
      covariates
    ) %>% 
    mutate(
      Q = quantile(Response, probs = quant),
      Above = Response > Q
    ) %>% 
    ungroup()
  
  TEST <- fisher.test(DATA$Above, DATA$Group, ...)
  
  return(TEST)
}

lm_fe_mean_test <- function(response, group, covariates = NULL, data, ...){
  if(length(covariates) > 0){
    RHS_FULL <- paste(group, paste(covariates, collapse = ' + '), sep = ' + ')
    RHS_NULL <- paste(covariates, collapse = ' + ')
  } else{
    RHS_FULL <- group
    RHS_NULL <- '1'
  }
  FORM_FULL <- formula(paste0(response, ' ~ ', RHS_FULL))
  FORM_NULL <- formula(paste0(response, ' ~ ', RHS_NULL))
  
  MODEL_FULL <- lm(FORM_FULL, data = data)
  MODEL_NULL <- lm(FORM_NULL, data = data)
  
  TEST <- anova(MODEL_FULL, MODEL_NULL, ...)
  
  return(TEST)
}

lm_me_mean_test <- function(response, group, fe_covariates = NULL, me_covariates = NULL, data, ...){
  if(length(fe_covariates) > 0){
    RHS_FULL <- paste(group, paste(fe_covariates, collapse = ' + '), sep = ' + ')
    RHS_NULL <- paste(fe_covariates, collapse = ' + ')
    if(length(me_covariates) > 0){
      RHS_FULL <- paste(RHS_FULL, paste(paste0('(1|', me_covariates, ')'), collapse = ' + '), sep = ' + ')
      RHS_NULL <- paste(RHS_NULL, paste(paste0('(1|', me_covariates, ')'), collapse = ' + '), sep = ' + ')
    }
  } else if(length(me_covariates) > 0){
    RHS_FULL <- paste(group, paste(paste0('(1|', me_covariates, ')'), collapse = ' + '), sep = ' + ')
    RHS_NULL <- paste(paste0('(1|', me_covariates, ')'), collapse = ' + ')
  } else{
    RHS_FULL <- group
    RHS_NULL <- '1'
  }
  FORM_FULL <- formula(paste0(response, ' ~ ', RHS_FULL))
  FORM_NULL <- formula(paste0(response, ' ~ ', RHS_NULL))
  
  MODEL_FULL <- lme4::lmer(FORM_FULL, REML = FALSE, data = data)
  MODEL_NULL <- lme4::lmer(FORM_NULL, REML = FALSE, data = data)
  
  TEST <- anova(MODEL_FULL, MODEL_NULL, ...)
  
  return(TEST)
}

#####


################################################################################
# load data ####

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

ANIMAL_DATA$Died %>% table(useNA = 'ifany')
# FALSE  TRUE 
#     1   767 

ANIMAL_DATA <- ANIMAL_DATA %>% 
  filter(Died) %>% 
  select(
    MouseID, Diet, Strain, Sex, SurvDays
  )
# # A tibble: 767 x 5
#    MouseID      Diet  Strain        Sex    SurvDays
#    <chr>        <fct> <fct>         <fct>     <dbl>
#  1 OR3609-5979  IF    CC018/UncJ    Male        488
#  2 OR3609-5978  IF    CC018/UncJ    Male        683
#  3 IL16557-5478 IF    CC040/TauUncJ Male        858
#  4 IL16557-5479 IF    CC040/TauUncJ Male        813
#  5 IL16557-5471 IF    CC040/TauUncJ Female      339
#  6 IL16557-5472 AL    CC040/TauUncJ Male        588
#  7 IL16557-5473 AL    CC040/TauUncJ Male        490
#  8 IL16557-5474 AL    CC040/TauUncJ Male        588
#  9 IL16557-5475 AL    CC040/TauUncJ Male        482
# 10 IL16557-5476 IF    CC040/TauUncJ Male        580
# # … with 757 more rows

#####


DIET_TEST_RESULTS <- data.frame(
  Test = vector(),
  TestFactor = vector(),
  CovariateFactors = vector(),
  Strain = vector(),
  Sex = vector(),
  PValue = vector()
)

################################################################################
# Diet effects: mean lifespan, fixed-effect model ####

## Mean lifespan: diet conditioned on sex and strain
TEST <- lm_fe_mean_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain', 'Sex'),
  data = ANIMAL_DATA
)
# Analysis of Variance Table
# 
# Model 1: SurvDays ~ Diet + Strain + Sex
# Model 2: SurvDays ~ Strain + Sex
#   Res.Df      RSS Df Sum of Sq      F   Pr(>F)   
# 1    755 21800738                                
# 2    756 22072323 -1   -271585 9.4055 0.002241 **
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Mean lifespan (lm)',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain and Sex',
    Strain = 'All',
    Sex = 'All',
    PValue = TEST$`Pr(>F)`[2]
  )
)


## Mean lifespan: diet for females conditioned on strain
TEST <- lm_fe_mean_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain'),
  data = ANIMAL_DATA %>% 
    filter(Sex == 'Female')
)
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Mean lifespan (lm)',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain',
    Strain = 'All',
    Sex = 'Female',
    PValue = TEST$`Pr(>F)`[2]
  )
)


## Mean lifespan: diet for males conditioned on strain
TEST <- lm_fe_mean_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain'),
  data = ANIMAL_DATA %>% 
    filter(Sex == 'Male')
)
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Mean lifespan (lm)',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain',
    Strain = 'All',
    Sex = 'Male',
    PValue = TEST$`Pr(>F)`[2]
  )
)


## Mean lifespan: strain-specific diet effects
for(STRAIN in levels(ANIMAL_DATA$Strain)){
  STRAIN_DATA <- ANIMAL_DATA %>% 
    filter(
      Strain == STRAIN
    )
  TEST <- lm_fe_mean_test(
    response = 'SurvDays',
    group = 'Diet',
    covariates = c('Sex'),
    data = STRAIN_DATA
  )
  
  DIET_TEST_RESULTS <- rbind(
    DIET_TEST_RESULTS,
    data.frame(
      Test = 'Mean lifespan (lm)',
      TestFactor = 'Diet',
      CovariateFactors = 'Sex',
      Strain = STRAIN,
      Sex = 'All',
      PValue = TEST$`Pr(>F)`[2]
    )
  )
  
  for(SEX in levels(STRAIN_DATA$Sex)){
    SEX_DATA <- STRAIN_DATA %>% 
      filter(Sex == SEX)
    
    TEST <- lm_fe_mean_test(
      response = 'SurvDays',
      group = 'Diet',
      covariates = NULL,
      data = SEX_DATA
    )
    
    DIET_TEST_RESULTS <- rbind(
      DIET_TEST_RESULTS,
      data.frame(
        Test = 'Mean lifespan (lm)',
        TestFactor = 'Diet',
        CovariateFactors = NA,
        Strain = STRAIN,
        Sex = SEX,
        PValue = TEST$`Pr(>F)`[2]
      )
    )
  }
  
  rm(STRAIN, STRAIN_DATA, TEST, SEX, SEX_DATA)
}

#####


################################################################################
# Diet effects: mean lifespan, mixed-effect model ####

## Mean lifespan: diet conditioned on sex and strain
TEST <- lm_me_mean_test(
  response = 'SurvDays',
  group = 'Diet',
  fe_covariates = c('Sex'),
  me_covariates = c('Strain'),
  data = ANIMAL_DATA
)
# Data: data
# Models:
# MODEL_NULL: SurvDays ~ Sex + (1 | Strain)
# MODEL_FULL: SurvDays ~ Diet + Sex + (1 | Strain)
#            npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# MODEL_NULL    4 10101 10120 -5046.6    10093                        
# MODEL_FULL    5 10094 10117 -5041.9    10084 9.3222  1   0.002264 **
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Mean lifespan (lmer)',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain and Sex',
    Strain = 'All',
    Sex = 'All',
    PValue = TEST$`Pr(>Chisq)`[2]
  )
)


## Mean lifespan: diet for females conditioned on strain
TEST <- lm_me_mean_test(
  response = 'SurvDays',
  group = 'Diet',
  me_covariates = c('Strain'),
  data = ANIMAL_DATA %>% 
    filter(Sex == 'Female')
)
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Mean lifespan (lmer)',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain',
    Strain = 'All',
    Sex = 'Female',
    PValue = TEST$`Pr(>Chisq)`[2]
  )
)


## Mean lifespan: diet for males conditioned on strain
TEST <- lm_me_mean_test(
  response = 'SurvDays',
  group = 'Diet',
  me_covariates = c('Strain'),
  data = ANIMAL_DATA %>% 
    filter(Sex == 'Male')
)
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Mean lifespan (lmer)',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain',
    Strain = 'All',
    Sex = 'Male',
    PValue = TEST$`Pr(>Chisq)`[2]
  )
)


# ### Don't do!! No random effects in these models (because they are strain-specific)
# ## Mean lifespan: strain-specific diet effects
# for(STRAIN in levels(ANIMAL_DATA$Strain)){
#   STRAIN_DATA <- ANIMAL_DATA %>% 
#     filter(
#       Strain == STRAIN
#     )
#   TEST <- lm_fe_mean_test(
#     response = 'SurvDays',
#     group = 'Diet',
#     covariates = c('Sex'),
#     data = STRAIN_DATA
#   )
#   
#   DIET_TEST_RESULTS <- rbind(
#     DIET_TEST_RESULTS,
#     data.frame(
#       Test = 'Mean lifespan (lmer)',
#       TestFactor = 'Diet',
#       CovariateFactors = 'Sex',
#       Strain = STRAIN,
#       Sex = 'All',
#       PValue = TEST$`Pr(>F)`[2]
#     )
#   )
#   
#   for(SEX in levels(STRAIN_DATA$Sex)){
#     SEX_DATA <- STRAIN_DATA %>% 
#       filter(Sex == SEX)
#     
#     TEST <- lm_fe_mean_test(
#       response = 'SurvDays',
#       group = 'Diet',
#       data = SEX_DATA
#     )
#     
#     DIET_TEST_RESULTS <- rbind(
#       DIET_TEST_RESULTS,
#       data.frame(
#         Test = 'Mean lifespan (lmer)',
#         TestFactor = 'Diet',
#         CovariateFactors = NA,
#         Strain = STRAIN,
#         Sex = SEX,
#         PValue = TEST$`Pr(>F)`[2]
#       )
#     )
#   }
#   
#   rm(STRAIN, STRAIN_DATA, TEST, SEX, SEX_DATA)
# }

#####


################################################################################
# Diet effects: median lifespan ####

## Median lifespan: diet conditioned on sex and strain
TEST <- quant_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain', 'Sex'),
  quant = 0.5,
  data = ANIMAL_DATA
)
# Fisher's Exact Test for Count Data
# 
# data:  DATA$Above and DATA$Group
# p-value = 0.007532
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  1.102990 1.988405
# sample estimates:
# odds ratio 
#   1.480083 
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Median lifespan',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain and Sex',
    Strain = 'All',
    Sex = 'All',
    PValue = TEST$p.value
  )
)


## Median lifespan: diet for females conditioned on strain
TEST <- quant_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain'),
  quant = 0.5,
  data = ANIMAL_DATA %>% 
    filter(Sex == 'Female')
)
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Median lifespan',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain',
    Strain = 'All',
    Sex = 'Female',
    PValue = TEST$p.value
  )
)


## Median lifespan: diet for males conditioned on strain
TEST <- quant_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain'),
  quant = 0.5,
  data = ANIMAL_DATA %>% 
    filter(Sex == 'Male')
)
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Median lifespan',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain',
    Strain = 'All',
    Sex = 'Male',
    PValue = TEST$p.value
  )
)


## Median lifespan: strain-specific diet effects
for(STRAIN in levels(ANIMAL_DATA$Strain)){
  STRAIN_DATA <- ANIMAL_DATA %>% 
    filter(
      Strain == STRAIN
    )
  TEST <- quant_test(
    response = 'SurvDays',
    group = 'Diet',
    covariates = c('Sex'),
    quant = 0.5,
    data = STRAIN_DATA
  )
  
  DIET_TEST_RESULTS <- rbind(
    DIET_TEST_RESULTS,
    data.frame(
      Test = 'Median lifespan',
      TestFactor = 'Diet',
      CovariateFactors = 'Sex',
      Strain = STRAIN,
      Sex = 'All',
      PValue = TEST$p.value
    )
  )
  
  for(SEX in levels(STRAIN_DATA$Sex)){
    SEX_DATA <- STRAIN_DATA %>% 
      filter(Sex == SEX)
    
    TEST <- quant_test(
      response = 'SurvDays',
      group = 'Diet',
      covariates = NULL,
      quant = 0.5,
      data = SEX_DATA
    )
    
    DIET_TEST_RESULTS <- rbind(
      DIET_TEST_RESULTS,
      data.frame(
        Test = 'Median lifespan',
        TestFactor = 'Diet',
        CovariateFactors = NA,
        Strain = STRAIN,
        Sex = SEX,
        PValue = TEST$p.value
      )
    )
  }
  
  rm(STRAIN, STRAIN_DATA, TEST, SEX, SEX_DATA)
}

#####


################################################################################
# Diet effects: maximum (90th percentile) lifespan ####

## Max lifespan: diet conditioned on sex and strain
TEST <- quant_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain', 'Sex'),
  quant = 0.9,
  data = ANIMAL_DATA
)
# Fisher's Exact Test for Count Data
# 
# data:  DATA$Above and DATA$Group
# p-value = 0.01277
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  1.124423 3.116837
# sample estimates:
# odds ratio 
#   1.857162 
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Maximum lifespan',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain and Sex',
    Strain = 'All',
    Sex = 'All',
    PValue = TEST$p.value
  )
)


## Max lifespan: diet for females conditioned on strain
TEST <- quant_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain'),
  quant = 0.9,
  data = ANIMAL_DATA %>% 
    filter(Sex == 'Female')
)
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Maximum lifespan',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain',
    Strain = 'All',
    Sex = 'Female',
    PValue = TEST$p.value
  )
)


## Max lifespan: diet for males conditioned on strain
TEST <- quant_test(
  response = 'SurvDays',
  group = 'Diet',
  covariates = c('Strain'),
  quant = 0.9,
  data = ANIMAL_DATA %>% 
    filter(Sex == 'Male')
)
DIET_TEST_RESULTS <- rbind(
  DIET_TEST_RESULTS,
  data.frame(
    Test = 'Maximum lifespan',
    TestFactor = 'Diet',
    CovariateFactors = 'Strain',
    Strain = 'All',
    Sex = 'Male',
    PValue = TEST$p.value
  )
)


## Max lifespan: strain-specific diet effects
for(STRAIN in levels(ANIMAL_DATA$Strain)){
  STRAIN_DATA <- ANIMAL_DATA %>% 
    filter(
      Strain == STRAIN
    )
  TEST <- quant_test(
    response = 'SurvDays',
    group = 'Diet',
    covariates = c('Sex'),
    quant = 0.9,
    data = STRAIN_DATA
  )
  
  DIET_TEST_RESULTS <- rbind(
    DIET_TEST_RESULTS,
    data.frame(
      Test = 'Maximum lifespan',
      TestFactor = 'Diet',
      CovariateFactors = 'Sex',
      Strain = STRAIN,
      Sex = 'All',
      PValue = TEST$p.value
    )
  )
  
  for(SEX in levels(STRAIN_DATA$Sex)){
    SEX_DATA <- STRAIN_DATA %>% 
      filter(Sex == SEX)
    
    TEST <- quant_test(
      response = 'SurvDays',
      group = 'Diet',
      covariates = NULL,
      quant = 0.9,
      data = SEX_DATA
    )
    
    DIET_TEST_RESULTS <- rbind(
      DIET_TEST_RESULTS,
      data.frame(
        Test = 'Maximum lifespan',
        TestFactor = 'Diet',
        CovariateFactors = NA,
        Strain = STRAIN,
        Sex = SEX,
        PValue = TEST$p.value
      )
    )
  }
  
  rm(STRAIN, STRAIN_DATA, TEST, SEX, SEX_DATA)
}

#####


DIET_TEST_RESULTS <- DIET_TEST_RESULTS %>% 
  arrange(
    Strain,
    factor(
      Test, 
      levels = c(
        'Mean lifespan (lmer)', 'Mean lifespan (lm)', 
        'Median lifespan', 'Maximum lifespan'
      )
    ),
    Sex
  )


################################################################################
#  ####

write_csv(
  DIET_TEST_RESULTS,
  'analysis/lifespan/results/diet_effect_pvalues.csv'
)

#####


################################################################################
#  ####

#####


################################################################################
## clear workspace ###########################################

rm(list = ls())
pacman::p_unload('all')
graphics.off()

#####
