#' Pistachio carbon budget components from CIMIS CSV
#' @param cimis_csv_path path to CIMIS hourly csv
#' @param leaf_num total leaves
#' @param fruit_num total fruits
#' @param A_is_net TRUE if assimilation already subtracts leaf Rd
#' @param clamp_growth floor negative growth increments to zero
#' @return A list containing intermediate model tables and the final daily budget.
#' @export
pistachio_budget_components_from_csv <- function(
    cimis_csv_path, leaf_num, fruit_num,
    A_is_net = TRUE, clamp_growth = TRUE
){
  hourly_raw <- extract_sol_rad(cimis_csv_path)
  objs <- calc_hourly_abs_ppfd_and_gdd(hourly_raw)
  hourly_df <- objs$hourly

  A_tbl <- daily_net_assimilation_mb_by_gdd(hourly_df) %>%
    dplyr::select(Date, Cum_GDD_base7, Daily_Net_Assimilation_umol_m2)
  gdds <- A_tbl$Cum_GDD_base7

  leaf_area <- leaf_area_growth(gdd_value = gdds, leaf_num = leaf_num) %>%
    dplyr::rename(Cum_GDD_base7 = GDD, Leaf_Area_mm2 = Area_mm) %>%
    dplyr::mutate(Leaf_Area_m2 = Leaf_Area_mm2 * 1e-6)

  A_total <- A_tbl %>%
    dplyr::left_join(leaf_area, by = "Cum_GDD_base7") %>%
    dplyr::mutate(A_umol_day = Daily_Net_Assimilation_umol_m2 * Leaf_Area_m2) %>%
    dplyr::select(Date, Cum_GDD_base7, A_umol_day)

  resp_leaf_daily <- respiration_per_g_from_hourly(hourly_df)$daily
  leaf_fw <- leaf_FW_growth(gdd_value = gdds, leaf_num = leaf_num) %>%
    dplyr::rename(Cum_GDD_base7 = GDD)

  R_leaves <- resp_fw_by_gdd(
    resp_leaf_daily, leaf_fw, "Cum_GDD_base7", "Ri_umol_g_day", "FW_g"
  ) %>%
    dplyr::transmute(Cum_GDD_base7, R_leaves_umol_day = Resp_umol_day)

  resp_fruit_daily <- respiration_per_g_from_hourly_repro(hourly_df)$daily
  fruit_fw <- fruit_fw_double_sigmoid_growth(gdd_value = gdds, fruit_num = fruit_num)

  R_fruits <- resp_fw_by_gdd(
    resp_fruit_daily, fruit_fw, "Cum_GDD_base7", "Ri_umol_g_day", "FW_ind"
  ) %>%
    dplyr::transmute(Cum_GDD_base7, R_fruits_umol_day = Resp_umol_day)

  leaf_dw <- leaf_DW_growth(gdd_value = gdds, leaf_num = leaf_num) %>%
    dplyr::rename(Cum_GDD_base7 = GDD)
  leaf_c <- carbon_to_umol(leaf_dw, "DW_g") %>%
    dplyr::arrange(Cum_GDD_base7) %>%
    dplyr::mutate(G_leaves_umol_day = C_umol - dplyr::lag(C_umol, default = 0))
  if (clamp_growth) {
    leaf_c$G_leaves_umol_day <- pmax(leaf_c$G_leaves_umol_day, 0)
  }

  fruit_dw <- fruit_dw_double_sigmoid_growth(gdd_value = gdds, fruit_num = fruit_num)
  fruit_c <- carbon_to_umol(fruit_dw, "DW_ind") %>%
    dplyr::arrange(Cum_GDD_base7) %>%
    dplyr::mutate(G_fruits_umol_day = C_umol - dplyr::lag(C_umol, default = 0))
  if (clamp_growth) {
    fruit_c$G_fruits_umol_day <- pmax(fruit_c$G_fruits_umol_day, 0)
  }

  out <- A_total %>%
    dplyr::left_join(R_leaves, by = "Cum_GDD_base7") %>%
    dplyr::left_join(R_fruits, by = "Cum_GDD_base7") %>%
    dplyr::left_join(
      dplyr::select(leaf_c, Cum_GDD_base7, G_leaves_umol_day),
      by = "Cum_GDD_base7"
    ) %>%
    dplyr::left_join(
      dplyr::select(fruit_c, Cum_GDD_base7, G_fruits_umol_day),
      by = "Cum_GDD_base7"
    ) %>%
    dplyr::mutate(
      R_leaves_umol_day = dplyr::coalesce(R_leaves_umol_day, 0),
      R_fruits_umol_day = dplyr::coalesce(R_fruits_umol_day, 0),
      G_leaves_umol_day = dplyr::coalesce(G_leaves_umol_day, 0),
      G_fruits_umol_day = dplyr::coalesce(G_fruits_umol_day, 0),
      CB_umol_day = if (A_is_net) {
        A_umol_day - R_fruits_umol_day - G_leaves_umol_day - G_fruits_umol_day
      } else {
        A_umol_day - R_leaves_umol_day - R_fruits_umol_day - G_leaves_umol_day - G_fruits_umol_day
      }
    ) %>%
    dplyr::arrange(Date)

  list(
    hourly_raw = hourly_raw,
    hourly = hourly_df,
    assimilation = A_tbl,
    leaf_area = leaf_area,
    respiration_leaves = R_leaves,
    respiration_fruits = R_fruits,
    growth_leaves = dplyr::select(leaf_c, Cum_GDD_base7, G_leaves_umol_day),
    growth_fruits = dplyr::select(fruit_c, Cum_GDD_base7, G_fruits_umol_day),
    budget = out
  )
}

#' Pistachio daily carbon budget from CIMIS CSV
#' @inheritParams pistachio_budget_components_from_csv
#' @return data.frame with A, respiration (leaves/fruits), growth C (leaves/fruits), CB (umol/day)
#' @export
pistachio_daily_carbon_budget_from_csv <- function(
    cimis_csv_path, leaf_num, fruit_num,
    A_is_net = TRUE, clamp_growth = TRUE
){
  pistachio_budget_components_from_csv(
    cimis_csv_path = cimis_csv_path,
    leaf_num = leaf_num,
    fruit_num = fruit_num,
    A_is_net = A_is_net,
    clamp_growth = clamp_growth
  )$budget
}
