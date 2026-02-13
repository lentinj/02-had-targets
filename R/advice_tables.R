ft_advice_table_landings <- function(pcon) {
  # i.e. advice/tables/landings.csv
  dplyr::tbl(pcon, "landings") |>
    pax::pax_landings_by_gear() |>
    dplyr::collect(n = Inf) |>
    dplyr::rename(gear = gear_name, landings = catch) |>
    dplyr::select(-num_boats)
}
