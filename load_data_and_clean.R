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

### 2.3 Re-organize target variable 'loan_status' and use only 'Charged off' and 'Fully Paid' for lifetime PD model training ----
lc_data_nonCurrent <- lc_data_nonmiss %>% 
  dplyr::mutate(
    loan_status = case_when(
      str_detect(loan_status, regex('charged off$', ignore_case = TRUE)) ~ 'charged_off',
      str_detect(loan_status, regex('fully paid$', ignore_case = TRUE)) ~ 'fully_paid',
      TRUE ~ 'ongoing'
    )
  ) %>%
  dplyr::filter(loan_status != 'ongoing')

## 2.3.1 Visualize charged-off ratio for each sub grade ----
gg_lc_subgrade_co_ratio <- lc_data_nonCurrent %>% 
  select(
      id
    , sub_grade
    , loan_status
  ) %>% 
  group_by(sub_grade) %>% 
  summarise(
    sub_grade_cnt = n(),
    bad_cnt = sum(loan_status == 'charged_off'),
    bad_ratio = 
      round(sum(loan_status == 'charged_off') / n() * 100, 2)
  ) %>%
  ggplot(aes(x = sub_grade, y = bad_ratio, fill = sub_grade_cnt)) +
  geom_col(colour = 'black') +
  scale_fill_gradient(low = "#00b159", high = "#d11141") +
  geom_text(aes(label = paste0(bad_ratio, '%')), 
            vjust = -0.3, color = 'black', size = 2.7,
            position = position_dodge(.9)) +
  geom_text(aes(label = sub_grade_cnt), 
            vjust = 4.5, color = 'white', size = 3.2,
            position = position_dodge(.9)) +
  xlab('LendingClub Sub Grade') +
  ylab('Charged Off Ratio') +
  scale_y_continuous(breaks = seq(0, 80, 5), labels = paste0(seq(0, 80, 5), '%')) +
  labs(fill = 'Loan Count') +
  theme_bw()

gg_lc_subgrade_co_ratio

gg_lc_subgrade_pct <- lc_data_nonCurrent %>% 
  select(
    id
    , sub_grade
  ) %>% 
  group_by(sub_grade) %>% 
  summarise(
    sub_grade_cnt = n(),
    sub_grade_pct = round(n() / nrow(lc_data_nonCurrent) * 100, 2)
  ) %>% 
  ggplot(aes(x = sub_grade, y = sub_grade_pct, fill = sub_grade_cnt)) +
  geom_col(color = 'purple') +
  scale_y_continuous(breaks = seq(0, 8, 1), labels = paste0(seq(0, 8, 1), '%')) +
  scale_fill_gradient(low = "#00b159", high = "#d11141") +
  geom_text(aes(label = paste0(sub_grade_pct, '%')), 
            vjust = -0.2, color = 'black', size = 3.2,
            position = position_dodge(.9)) +
  xlab('LendingClub Sub Grade') +
  ylab('Sub Grade Percentage') +
  labs(fill = 'Loan Count') +
  theme_bw()

gg_lc_subgrade_pct
