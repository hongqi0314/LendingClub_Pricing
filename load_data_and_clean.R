## Load data and initial cleanup

#### 0. Load required packages ----
utilALF::load_pkgs()

#### 1. Read data from the .csv file ----
lc_data_2007_to_2018Q4 <- 
  readr::read_csv(
    file = here::here('data', "accepted_2007_to_2018Q4.csv"), 
    col_names = TRUE
  )

lc_data <- lc_data_2007_to_2018Q4 %>%
  dplyr::filter(!is.na(id))

stopifnot(nrow(lc_data) == length(unique(lc_data$id)))

#### 2. Data cleaning and feature selection ----
### 2.1 Re-calculate income, debt, and DTI ----
lc_temp_debtCalc <- lc_data %>% 
  dplyr::filter(
      !(is.na(annual_inc) & is.na(annual_inc_joint))
    , dti >= 0.00  # exclude records with negative DTI.
    , dti < 999.00 # exclude records with DTI 999.00 because it looks like a truncated value.
  ) %>% 
  dplyr::mutate(
    annual_inc_total = annual_inc + 
      ifelse(is.na(annual_inc_joint), 0.00, annual_inc_joint),
    debt_total = annual_inc * (dti / 100) +
      ifelse(is.na(annual_inc_joint) | is.na(dti_joint), 0.00, 
             annual_inc_joint * (dti / 100)), 
    dti_total = round(debt_total / annual_inc_total * 100, 2)
  )
  
### 2.2 Check missing value proportion for all features ----
lc_data_missing <- 
  purrr::map_dbl(lc_temp_debtCalc, ~ round(mean(is.na(.)) * 100, 2)) %>%
  base::sort(., decreasing = TRUE)

## 2.2.1 Drop features missing more than 90% of the records ----
lc_data_nonmiss <- lc_temp_debtCalc %>% 
  dplyr::select(
    names(lc_data_missing)[lc_data_missing <= 90.00]
  ) # Double check needed since it seems that it's reasonable for some features to
    # have huge number of missing values. 
    # For example, "inq_last_12m", "total_cu_tl", or "open_acc_6m".

### 2.3 Re-organize target variable 'loan_status' and use only 'Charged off' and 'Fully Paid' for Lifetime PD model training ----
lc_data_nonCurrent <- lc_data_nonmiss %>% 
  dplyr::mutate(
    loan_status = case_when(
      str_detect(loan_status, regex('charged off$', ignore_case = TRUE)) ~ 'charged_off',
      str_detect(loan_status, regex('fully paid$', ignore_case = TRUE)) ~ 'fully_paid',
      TRUE ~ 'ongoing'
    )
  ) %>%
  dplyr::filter(loan_status != 'ongoing')
