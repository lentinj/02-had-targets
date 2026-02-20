# _targets.R file
library(targets)
library(tarchetypes)

tar_source() # Source R/*.R

species <- 2
year_start <- 1979
year_end <- lubridate::year(Sys.Date())
assessment_year <- year_end - 1
age_end <- 14
TAC <- 76415
HR_MGT <- 0.35

tar_option_set(
  format = pax::pax_tar_format_parquet(),
  packages = c(),
)

list(
  ## Populate local database
  if (nzchar(Sys.getenv("PAX_SOURCE_DB"))) {
    tar_target(
      pax_db,
      pax::pax_connect(Sys.getenv("PAX_SOURCE_DB")),
      format = pax::pax_tar_format_duckdb()
    )
  } else {
    tar_target(
      pax_db,
      ft_populate_pax(
        species,
        year_start,
        year_end,
        sampling_type = c(1, 2, 8, 30, 35)
      ),
      format = pax::pax_tar_format_duckdb()
    )
  },

  ## Generate input data
  tar_target(
    input_data_lw_pred,
    ft_input_data_lw(
      pax_db,
      sampling_type = 30,
      prediction_length_range = 1:150
    )
  ),
  tar_target(
    input_data_maturity_key,
    ft_input_data_maturity_key(
      pax_db,
      lgroup = seq(0, 200, 5),
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113),
        S = NULL
      ),
    )
  ),
  tar_target(
    input_data_igfs_index,
    ft_input_data_si_index(
      pax_db,
      sampling_type = 30,
      tow_number = 0:35,
      lw_key = input_data_lw_pred,
      maturity_key = input_data_maturity_key,
      strata_name = "old_strata",
      tgroup = NULL,
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113),
        S = NULL
      )
    )
  ),
  tar_target(
    input_data_agfs_index,
    ft_input_data_si_index(
      pax_db,
      regions = list(all = 101:115),
      sampling_type = 35,
      tow_number = 0:75,
      gear_id_filter = 77:78,
      strata_name = "new_strata_autumn"
    )
  ),
  tar_target(
    input_data_comm_index,
    ft_input_data_si_index(
      pax_db,
      sampling_type = c(1, 2, 8),
      tgroup = list(t1 = 1:6, t2 = 7:12),
      gear_group = list(
        Other = 'Var',
        BMT = NA, # i.e. unknown gears are BMT
        BMT = c('BMT', 'NPT', 'SHT', 'PGT', 'DRD'),
        LLN = c('HLN', 'LLN', 'GIL'),
        DSE = c('PSE', 'DSE')
      ),
      scale_by_landings = TRUE
    )
  ),
  tar_target(
    # TODO: Starts in 1903, not 1970
    input_data_landings,
    ft_input_data_landings(
      pax_db
    )
  ),
  tar_target(
    input_data,
    ft_input_data_had(
      year_start,
      year_end,
      input_data_comm_index,
      input_data_igfs_index,
      input_data_agfs_index,
      input_data_landings
    )
  ),

  tar_target(
    sam_cn,
    ft_sam_cn(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_cw,
    ft_sam_cw(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      minage = 1,
      maxage = 12,
      tyr = assessment_year
    ),
    format = 'rds'
  ),

  tar_target(
    sam_smb,
    ft_sam_smb(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_smh,
    ft_sam_smh(
      input_data |> dplyr::filter(year < assessment_year, age > 0),
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_sw,
    ft_sam_sw(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_mo,
    ft_sam_mo(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_lf,
    ft_sam_lf(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      sam_cn,
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_pf,
    ft_sam_pf(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      sam_cn,
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_pm,
    ft_sam_pm(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      sam_cn,
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_nm,
    ft_sam_nm(
      input_data |> dplyr::filter(year <= assessment_year, age > 0),
      minage = 1,
      maxage = 12
    ),
    format = 'rds'
  ),

  tar_target(
    sam_dat,
    ft_sam_dat(
      sam_cn,
      sam_cw,
      sam_smb,
      sam_smh,
      sam_sw,
      sam_mo,
      sam_lf,
      sam_pf,
      sam_pm,
      sam_nm,
      assessment_year
    ),
    format = 'rds'
  ),

  tar_target(
    sam_conf,
    ft_sam_conf(
      sam_dat
    ),
    format = 'rds'
  ),

  tar_target(
    sam_fit,
    SAMutils::full_sam_fit(
      sam_dat,
      sam_conf
    ),
    format = 'rds'
  )
)
