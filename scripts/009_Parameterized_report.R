# This script knits the reports based on parameters specified by the user
#
# Library required programs
library(here)
library(quarto)
library(tidyverse)
#
# Load data
raw_data_0090 <- read.csv(here::here("data_raw/data_0090/data.csv"),na.strings = "", fileEncoding = "UTF-8-BOM")
all_countries <- raw_data_0090$countryterritoryCode %>% unique()
raw_data_0090 %>% select(countriesAndTerritories, countryterritoryCode) %>% unique() %>% print()
#
# insert parameters
country_input <- c("FIN", "NOR", "SWE") # e.g. AUT or all_countries
year_input <- 2021 # e.g. 2021
period_input <- 7:12 # e.g. months jan:aug c(1:8)
#
# year auto naming:
if (!is.null(year_input)){
year_name <- paste0(min(year_input), "-", max(year_input))
} else {
  year_name <- paste0(min(raw_data_0090$year), "-", max(raw_data_0090$year))
}
# period auto naming:
if (!is.null(period_input)){
  period_name <- paste0(min(period_input), "-", max(period_input))  
} else {
  period_name <- paste0(min(raw_data_0090$month), "-", max(raw_data_0090$month))
}
#
# Render docs
for (country_list in country_input){
quarto::quarto_render(
  input = here::here("009_Parameterized_report.qmd"),
  execute_params = list(
    country = country_list,
    year = year_input,
    period = period_input),
  output_file = paste0("009a_",country_list, "_", year_name, "_",period_name,".html"),
  output_format = "dashboard"
)
} 