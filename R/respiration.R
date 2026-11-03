#' @export
respiration_per_g_from_hourly <- function(
    hourly_df,
    date_col = "Date",
    temp_col = "Air_Temp_C",
    gdd_col  = "Cum_GDD_base7",
    # GDD staging and kinetics
    gdd_cutoffs  = c(32.85, 142.75, 217.80, 240.65, 376.95),
    RTref_nmol   = c(15.35, 12.69, 8.69, 6.14, 1.58),  # nmol g^-1 s^-1 at Tref
    Q10          = c(1.23, 1.64, 1.42, 1.18, 2.64),
    stage_labels = c("-4", "9", "19", "22", ">=36"),
    Tref_C = 20
) {
  stopifnot(length(RTref_nmol) == 5L, length(Q10) == 5L,
            length(stage_labels) == 5L, length(gdd_cutoffs) == 5L)

  req_cols <- c(date_col, temp_col, gdd_col)
  missing_cols <- setdiff(req_cols, names(hourly_df))
  if (length(missing_cols)) {
    stop("hourly_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  breaks <- c(-Inf, gdd_cutoffs, Inf)
  bin_to_stage <- c(1, 2, 3, 4, 4, 5)

  hourly2 <- hourly_df %>%
    mutate(
      bin   = findInterval(.data[[gdd_col]], breaks, rightmost.closed = TRUE),
      sidx  = bin_to_stage[pmax(pmin(bin, length(bin_to_stage)), 1)],
      RTref_nmol_g_s = RTref_nmol[sidx],
      Q10_s          = Q10[sidx],
      stage_label    = stage_labels[sidx],
      RTref_umol_g_s = RTref_nmol_g_s / 1000,
      Ri_umol_g_s    = RTref_umol_g_s * (Q10_s ^ ((.data[[temp_col]] - Tref_C) / 10)),
      Ri_umol_g_hr   = Ri_umol_g_s * 3600
    )

  daily <- hourly2 %>%
    group_by(.data[[date_col]]) %>%
    summarize(
      Cum_GDD_base7 = first(.data[[gdd_col]]),
      stage_label   = dplyr::last(stage_label),
      Ri_umol_g_day = sum(Ri_umol_g_hr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(.data[[date_col]])

  list(
    hourly = hourly2 %>%
      select(all_of(date_col), all_of(temp_col), all_of(gdd_col),
             stage_label, RTref_nmol_g_s, Q10_s, Ri_umol_g_s, Ri_umol_g_hr),
    daily  = daily
  )
}


#' @export
respiration_per_g_from_hourly_repro <- function(
    hourly_df,
    date_col = "Date",
    temp_col = "Air_Temp_C",
    gdd_col  = "Cum_GDD_base7",
    # Reproductive stage boundaries in cumulative GDD.
    gdd_cutoffs  = c(142.75, 240.65, 500),
    # Reproductive parameters (RTref at 20 C in nmol g^-1 s^-1, then Q10)
    RTref_nmol   = c(10.4, 6.58, 3.415, 0.25),
    Q10          = c(1.97, 1.58, 2.54, 3.50),
    stage_labels = c("-4", "9", "16", ">=22"),
    Tref_C = 20
) {
  n_stage <- length(RTref_nmol)
  stopifnot(
    length(Q10) == n_stage,
    length(stage_labels) == n_stage,
    length(gdd_cutoffs) == n_stage - 1L
  )

  req_cols <- c(date_col, temp_col, gdd_col)
  miss <- setdiff(req_cols, names(hourly_df))
  if (length(miss)) stop("hourly_df is missing required columns: ", paste(miss, collapse = ", "))

  breaks <- c(-Inf, gdd_cutoffs, Inf)

  hourly2 <- hourly_df %>%
    mutate(
      bin   = findInterval(.data[[gdd_col]], breaks, rightmost.closed = TRUE),
      sidx  = pmax(pmin(bin, n_stage), 1L),
      RTref_nmol_g_s = RTref_nmol[sidx],
      Q10_s          = Q10[sidx],
      stage_label    = stage_labels[sidx],
      RTref_umol_g_s = RTref_nmol_g_s / 1000,
      Ri_umol_g_s    = RTref_umol_g_s * (Q10_s ^ ((.data[[temp_col]] - Tref_C) / 10)),
      Ri_umol_g_hr   = Ri_umol_g_s * 3600
    )

  daily <- hourly2 %>%
    group_by(.data[[date_col]]) %>%
    summarize(
      Cum_GDD_base7 = first(.data[[gdd_col]]),
      stage_label   = dplyr::last(stage_label),
      Ri_umol_g_day = sum(Ri_umol_g_hr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(.data[[date_col]])

  list(
    hourly = hourly2 %>%
      select(all_of(date_col), all_of(temp_col), all_of(gdd_col),
             stage_label, RTref_nmol_g_s, RTref_umol_g_s, Q10_s, Ri_umol_g_s, Ri_umol_g_hr),
    daily  = daily
  )
}


#' @export
resp_fw_by_gdd <- function(
    resp_df,
    fw_df,
    by_col   = "Cum_GDD_base7",
    resp_col = "Ri_umol_g_day",
    fw_col   = "FW_ind",
    out_col  = "Resp_umol_day"
) {
  if (!all(c(by_col, resp_col) %in% names(resp_df)))
    stop("resp_df must contain: ", by_col, " and ", resp_col)
  if (!all(c(by_col, fw_col) %in% names(fw_df)))
    stop("fw_df must contain: ", by_col, " and ", fw_col)

  r <- resp_df[, c(by_col, resp_col), drop = FALSE]
  f <- fw_df[,   c(by_col, fw_col),   drop = FALSE]
  names(r) <- c("by", "resp")
  names(f) <- c("by", "fw")

  m <- merge(r, f, by = "by", all = FALSE, sort = FALSE)
  m[[out_col]] <- as.numeric(m[["resp"]]) * as.numeric(m[["fw"]])

  names(m)[names(m) == "by"]   <- by_col
  names(m)[names(m) == "resp"] <- resp_col
  names(m)[names(m) == "fw"]   <- fw_col

  m[, c(by_col, resp_col, fw_col, out_col), drop = FALSE]
}
