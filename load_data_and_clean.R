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
    , dti >= 0.00 # exclude records with negative DTI.
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
  