#' Daily net assimilation by GDD (hourly integration)
#' @param hourly_df output from calc_hourly_abs_ppfd_and_gdd()$hourly
#' @param resp_hours hours/day to apply Rd (default 24)
#' @param clamp_net floor negative hourly net to 0 (FALSE by default)
#' @return data.frame(Date, Cum_GDD_base7, Daily_Net_Assimilation_umol_m2)
#' @export
daily_net_assimilation_mb_by_gdd <- function(
    hourly_df,
    date_col = "Date",
    ppfd_inst_col = "Absorbed_PPFD_umol_m2_s",
    gdd_col  = "Cum_GDD_base7",
    resp_hours = 24,
    clamp_net = FALSE
){
  df <- hourly_df
  param_tbl <- tibble::tibble(
    GDD     = c(134,309,517,934,1469,1776,2088,2325,2577,2904,3209),
    Rd      = c(3.705,3.721,3.249,2.346,3.317,3.577,3.144,3.726,3.277,1.906,2.177),
    phi_J   = c(0.05,0.049,0.047,0.046,0.045,0.039,0.047,0.043,0.049,0.041,0.036),
    theta_J = c(0.777,0.841,0.862,0.832,0.785,0.955,0.545,0.805,0.752,0.928,0.887),
    k_sat   = c(23.297,29.088,23.261,19.133,24.453,19.078,16.111,14.193,10.21,13.832,10.618)
  )

  df2 <- df %>%
    dplyr::group_by(.data[[date_col]]) %>%
    dplyr::mutate(
      day_gdd = dplyr::first(.data[[gdd_col]]),
      nearest_idx = which.min(abs(param_tbl$GDD - day_gdd)),
      Rd      = param_tbl$Rd[nearest_idx],
      phi_J   = param_tbl$phi_J[nearest_idx],
      theta_J = param_tbl$theta_J[nearest_idx],
      k_sat   = param_tbl$k_sat[nearest_idx]
    ) %>% dplyr::ungroup() %>%
    dplyr::mutate(
      A_gross_s = mb80(.data[[ppfd_inst_col]], k_sat, phi_J, theta_J),
      A_net_s   = if (clamp_net) pmax(A_gross_s - Rd, 0) else A_gross_s - Rd
    )

  df2 %>%
    dplyr::group_by(.data[[date_col]]) %>%
    dplyr::summarize(
      Cum_GDD_base7 = dplyr::first(.data[[gdd_col]]),
      Daily_Net_Assimilation_umol_m2 = sum(A_net_s * 3600, na.rm = TRUE) * (resp_hours/24),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data[[date_col]])
}
