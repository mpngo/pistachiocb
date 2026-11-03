calc_hourly_abs_ppfd_and_gdd <- function(par_data) {
  library(dplyr)

  percent_rad <- c(
    0, 0, 0, 0, 0, 0.012561885, 0.150056376, 0.799629167, 0.530233386,
    0.553310056, 0.368318483, 0.299945444, 0.267868938, 0.287762009,
    0.249916954, 0.316157775, 0.348545353, 0.254491934, 0.27068006,
    0.29734926, 0.014238329, 0, 0, 0
  )
  base_temp <- 7

  hourly <- par_data %>%
    mutate(
      Hour_index = as.integer(Hour) %/% 100,
      Absorbed_PPFD_umol_m2_s = PPFD_umol_m2_s * percent_rad[pmax(pmin(Hour_index,23),0) + 1]
    ) %>%
    select(Date, Hour, Absorbed_PPFD_umol_m2_s, Air_Temp_C)

  daily <- hourly %>%
    group_by(Date) %>%
    summarize(
      Tmin_C = min(Air_Temp_C, na.rm = TRUE),
      Tmax_C = max(Air_Temp_C, na.rm = TRUE),
      Absorbed_PPFD_daily_mol_m2 =
        sum(Absorbed_PPFD_umol_m2_s * 3600, na.rm = TRUE) / 1e6,
      .groups = "drop"
    ) %>%
    mutate(
      Tmean_C = (Tmax_C + Tmin_C) / 2,
      GDD_base7 = pmax(Tmean_C - base_temp, 0),
      Cum_GDD_base7 = cumsum(GDD_base7)
    ) %>%
    select(Date, Absorbed_PPFD_daily_mol_m2, Cum_GDD_base7)

  # attach daily Cum_GDD to each hour for parameter lookup
  hourly <- hourly %>%
    left_join(daily %>% select(Date, Cum_GDD_base7), by = "Date")

  list(hourly = hourly, daily = daily)
}
