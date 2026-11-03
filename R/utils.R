#' @keywords internal
NULL

# avoid R CMD check notes for .data
#' @importFrom rlang .data
NULL

inv_logit <- function(z) 1/(1+exp(-z))

# Marshall–Biscoe 1980 non-rectangular hyperbola
# Q_abs (umol m^-2 s^-1), k_sat (umol m^-2 s^-1), phi_J, theta_J
mb80 <- function(Q_abs, k_sat, phi_J, theta_J){
  # A = (phi*Q + Amax - sqrt((phi*Q + Amax)^2 - 4*theta*phi*Q*Amax)) / (2*theta)
  term <- phi_J*Q_abs + k_sat
  disc <- pmax(term^2 - 4*theta_J*phi_J*Q_abs*k_sat, 0)
  (term - sqrt(disc)) / (2*theta_J)
}



#' Extract CIMIS solar radiation and temps
#' @param file_path Character path to the CIMIS CSV file.
#' @return Tibble with Date, Hour, Sol_Rad_Ly, Sol_Rad_MJ, Sol_Rad_Wm2, PPFD_umol_m2_s, Air_Temp_C.
#' @export
#' @importFrom rlang .data
extract_sol_rad <- function(file_path) {
  dat <- readr::read_csv(file_path, show_col_types = FALSE)

  dat |>
    dplyr::transmute(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Hour = as.numeric(`Hour (PST)`),
      Sol_Rad_Ly = as.numeric(`Sol Rad (Ly/day)`),
      Sol_Rad_MJ = Sol_Rad_Ly * 0.04184,
      Sol_Rad_Wm2 = Sol_Rad_MJ * 11.574,
      PPFD_umol_m2_s = bigleaf::Rg.to.PPFD(Rg = Sol_Rad_Wm2),
      Air_Temp_C = (as.numeric(`Air Temp (F)`) - 32) * 5/9
    ) |>
    dplyr::arrange(.data$Date, .data$Hour)  # <- use .data, not rlang::.data
}





