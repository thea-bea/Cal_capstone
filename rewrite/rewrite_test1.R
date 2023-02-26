
# load libraries and data -------------------------------------------------

library(tidyverse)
library(haven)
library()

LTM = haven::read_sav("C:/Users/theab/Dropbox/school/Berkeley/Capstone/Capstone_Data/Capstone_Data_MASTER/LTMdata_2.public.sav")


# data cleaning & variable creation ---------------------------------------

LTM2 = LTM %>% mutate(
  # preg care provider
  preg_provider = case_when(Q805 == 1 ~ 1,
                            Q805 == 2 ~ 2,
                            Q805 == 3 ~ 3,
                            Q805 == 4 ~ 4,
                            Q805 %in% c(5, 6) ~ 5),
  preg_provider = factor(
    preg_provider,
    levels = c(1:5),
    labels = c(
      "OB or OBGYN",
      "Family Medicine Doctor",
      "Doctor Unsure Type",
      "CNM",
      "APP"
    )
  ),
  # depression screening
  depression_screening = factor(
    Q1415c,
    levels = c(1, 2),
    labels = c("Yes", "No")
  ),
  # doula support
  doula = case_when(Q1105 == 1 ~ 1,
                    Q1105 %in% c(2, 3) ~ 0),
  doula = factor(doula, levels = c(1, 0), labels = c("Yes", "No")),
  # race
  racegrp_3 = case_when(
    racegrp == 1 ~ 1,
    racegrp == 2 ~ 2,
    racegrp == 3 ~ 3,
    racegrp == 4 ~ 4,
    racegrp %in% c(5:7) ~ 5
  ),
  racegrp_3 = factor(
    racegrp_3,
    levels = c(1:5),
    labels = c(
      "Hispanic/Latina",
      "White (non-Hispanic)",
      "Black (non-Hispanic)",
      "Asian/Pacific Islander (non-Hispanic)",
      "Other/Mixed Race (non-Hispanic)"
    )
  ),
  # insurance - four options for model testing
  insurance_3 = case_when(
    insurance == 1 ~ 1,
    insurance %in% c(2, 3) ~ 2,
    insurance == 5 ~ 3,
    TRUE ~ NA
  ),
  insurance_3 = factor(
    insurance_3,
    levels = c(1:3),
    labels = c("Medi-Cal", "Private", "Uninsured")
  ),
  insurance_4 = case_when(insurance %in% c(1, 5) ~ 1,
                          insurance %in% c(2, 3) ~ 2,
                          TRUE ~ NA),
  insurance_4 = factor(
    insurance_4,
    levels = c(1, 2),
    labels = c("Medi-Cal or Uninsured", "Private")
  ),
  insurance_5 = case_when(insurance %in% c(2, 3) ~ 1,
                          insurance %in% c(1, 4, 5) ~ 2),
  insurance_5 = factor(
    insurance_5,
    levels = c(1, 2),
    labels = c("Private Insurance = Yes", "Private Insurance = No")
  ),
  insurance_6 = case_when(
    insurance %in% c(2, 3) ~ 1,
    insurance %in% c(1, 5) ~ 2,
    insurance == 4 ~ 3,
    TRUE ~ NA
  ),
  insurance_6 = factor(
    insurance_6,
    levels = c(1:3),
    labels = c(
      "Private Insurance = Yes",
      "Private Insurance = No",
      "Private Insurance = Unsure"
    )
  ),
  #education
  education_3 = case_when(
    Q1860 %in% c(1, 2) ~ 1,
    Q1860 %in% c(3:5) ~ 2,
    Q1860 %in% c(6, 7) ~ 3,
    Q1860 == 8 ~ 4,
    TRUE ~ NA
  ),
  education_3 = factor(
    education_3,
    levels = c(1:4),
    labels = c(
      "Less than High School",
      "High School Diploma or GED",
      "College Degree",
      "Graduate Degree"
    )
  ),
  # number of children as categorical
  num_children_cat_3 = cut(
    Q610,
    breaks = c(0, 1, 2, 10),
    labels = c("1", "2", "3 or more"),
    include.lowest = T,
    right = T
  ),
  # primary language
  prim_lang = case_when(Q1845 %in% c(1, 3) ~ 1,
                        Q1845 == 2 ~ 2,
                        Q1845 %in% c(4, 5) ~ 3,
                        TRUE ~ NA),
  prim_lang = factor(
    prim_lang,
    levels = c(1:3),
    labels = c("English", "Spanish", "Other")
  ),
  # marital status
  marital_status_4 = case_when(Q1820 %in% c(1, 2) ~ 0,
                               Q1820 %in% c(3, 4) ~ 1,
                               TRUE ~ NA),
  marital_status_4 == factor(
    marital_status_4,
    levels = c(0, 1),
    labels = c(
      "Married or Living Together as if Married",
      "Single, Divorced, or Widowed"
    )
  ),
  # age (categorical)
  age_cat_3 = cut(
    Q1842,
    breaks = c(0, 24, 29, 34, 46),
    labels = c("24 and under", "25-29 years", "30-34 years", "35 and over"),
    inclue.lowest = T,
    right = T
  ),
  # binary anxiety and depression
  preg_anxiety_3 = case_when(is.na(Q925a) | is.na(Q925b) ~ NA,
                             Q925a >= 3 | Q925b >= 3 ~ 1,
                             TRUE ~ 0),
  preg_anxiety_3 = factor(
    preg_anxiety_3,
    levels = c(0, 1),
    labels = c("No", "Yes")
  ),
  preg_depression_3 = case_when(is.na(Q925c) | is.na(Q925d) ~ NA,
                                Q925c >= 3 | Q925d >= 3 ~ 1,
                                TRUE ~ 0),
  preg_depression_3 = factor(
    preg_depression_3,
    levels = c(0, 1),
    labels = c("No", "Yes")
  ),
  # composite mental health concern during pregnancy
  preg_MH_issue = case_when(
    preg_anxiety_3 == "No" & preg_depression_3 == "No" ~ 0,
    preg_anxiety_3 == "Yes" &
      preg_depression_3 == "Yes" ~ 1
  ),
  # number postpartum visits in 8 wks after deliv
  ## categorical
  postpartum_visit = case_when(Q1405 == 0 ~ 0,
                               Q1405 == 1 ~ 1,
                               Q1405 == 2 ~ 2,
                               Q1405 >= 3 ~ 3),
  postpartum_visit = factor(
    postpartum_visit,
    levels = c(0:3),
    labels = c("0", "1", "2", "3 or more")
  ),
  ##binary for subanalysis
  pp_visit_SA = ifelse(Q1405 == 0, 0 , 1),
  pp_visit_SA = factor(
    pp_visit_SA,
    levels = c(0, 1),
    lables = c("0", "1 or more")
  )
)


# drop unused columns & export --------------------------------------------


