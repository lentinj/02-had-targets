# _targets.R file
library(targets)
library(tarchetypes)

tar_source() # Source R/*.R

species <- 2
year_start <- 1979
year_end <- lubridate::year(Sys.Date())
age_end <- 14

tac = 76774
# TODO: This is TAC in scripts_assessment_model()
tac_last_year = 76415

tar_option_set(
  packages = c(),
)

list(
  # Open database from assessment_model
  tar_target(
    pax_db,
    pax::pax_connect("_assessment_model/objects/pax_db", read_only = TRUE),
    format = pax::pax_tar_format_duckdb()
  ),

  ## tables
  tar_target(advice_table_landings, ft_advice_table_landings(pax_db)),

  ## Advice sheets
  tar_quarto(
    advice_en,
    path = "advice_en.qmd",
    execute_params = list(
      tac = tac,
      tac_last_year = tac_last_year,
      year_end = year_end
    ),
    quiet = FALSE
  ),
  tar_quarto(
    advice_is,
    path = "advice_is.qmd",
    execute_params = list(
      tac = tac,
      tac_last_year = tac_last_year,
      year_end = year_end
    ),
    quiet = FALSE
  )
)
