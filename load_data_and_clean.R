## Load data and initial cleanup

#### 0. Load required packages ----
devtools::install_local(path = "E:/RProjects/ALF_RPackage/utilALF")
utilALF::load_pkgs()

#### 1. Read data from the .csv file ----
lc_data_2007_to_2018Q4 <- 
  readr::read_csv(
    file = here::here('data', "accepted_2007_to_2018Q4.csv"), 
    col_names = TRUE
  )

lc_data <- lc_data_2007_to_2018Q4 %>%
  filter(!is.na(id))

stopifnot(nrow(lc_data) == length(unique(lc_data$id)))
