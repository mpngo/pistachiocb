#' @export
carbon_to_umol <- function(df, mass_col, fC = 0.45, molar_mass_C = 12) {
  stopifnot(is.data.frame(df))

  # Resolve the column name from bare name / string / index
  col_name <- if (is.character(mass_col) && length(mass_col) == 1) {
    mass_col
  } else if (is.numeric(mass_col) && length(mass_col) == 1) {
    names(df)[mass_col]
  } else {
    rlang::as_string(rlang::ensym(mass_col))
  }
  if (!col_name %in% names(df)) stop("Column '", col_name, "' not found.")

  mass_vec <- df[[col_name]]
  if (!is.numeric(mass_vec)) stop("Column '", col_name, "' must be numeric (grams).")

  C_g    <- mass_vec * fC
  C_umol <- (C_g / molar_mass_C) * 1e6
  df$C_g    <- C_g
  df$C_umol <- C_umol
  df
}
