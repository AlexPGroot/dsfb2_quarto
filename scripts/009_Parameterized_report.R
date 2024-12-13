# This script knits the reports based on parameters specified by the user
#
# Library required programs
library(here)
library(quarto)

# Render docs
quarto::quarto_render(
  input = here::here("009_Parameterized_report.qmd"),
  execute_params = list(country = "AUT"),
  output_file = paste0("AUT", "_009")
)
