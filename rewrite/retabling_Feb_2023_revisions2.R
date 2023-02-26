
# libraries ---------------------------------------------------------------

library(tidyverse)
library(dummies)
library(gt)
library(gtExtras)
library(flextable)
library(data.table)


# load data ---------------------------------------------------------------

LTM_cols = data.table::fread("C:/Users/theab/Dropbox/school/Berkeley/Capstone/Capstone_Data/Capstone_Data_MASTER/LTM_cols_M.csv")


# reorder some factors ----------------------------------------------------
## preg provider - OBGYN as reference
LTM_cols$preg_provider_4 <- factor(
  LTM_cols$preg_provider_3,
  levels = c(
    "OB or OBGYN",
    "Family Medicine Doctor",
    "Doctor Unsure Type",
    "CNM",
    "NP or PA"
  )
) 

## preg provider - CNM as reference
LTM_cols$preg_provider_5 <- factor(
  LTM_cols$preg_provider_3,
  levels = c(
    "CNM",
    "OB or OBGYN",
    "Family Medicine Doctor",
    "Doctor Unsure Type",
    "NP or PA"
  )
) 

## race
LTM_cols$racegrp_4 <- factor(
  LTM_cols$racegrp_3,
  levels = c(
    "White (non-Hispanic)",
    "Black (non-Hispanic)",
    "Hispanic/Latina",
    "Asian/Pacific Islander (non-Hispanic)",
    "Other/Mixed Race (non-Hispanic)"
  )
)

## education
LTM_cols$education_4 <- factor(
  LTM_cols$education_3,
  levels = c(
    "Less than High School",
    "High School Diploma or GED",
    "College Degree",
    "Graduate Degree"
  )
)

## primary language
LTM_cols$prim_lang_4 <- factor(LTM_cols$prim_lang_3,
                               levels = c("English", "Spanish", "Other"))

# table 1 -----------------------------------------------------------------

table1 = LTM_cols %>% select(
  preg_provider_4,
  depression_screening,
  racegrp_4,
  insurance_3,
  education_4,
  num_children_cat_3,
  prim_lang_4,
  marital_status_4,
  age_cat_3,
  preg_MH_issue,
  num_PP_visits_8wks
) %>% gt()
