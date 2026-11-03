#' @export
leaf_FW_growth <- function(a = 3, b = 160, c = 47, gdd_value = NULL, leaf_num = 1) {
  # Helper to compute logistic leaf FW
  logistic <- function(x) a / (1 + exp(-(x - b) / c))

  if (is.null(gdd_value)) {
    # Return full curve
    x <- seq(0, 3000, by = 100)
    y <- logistic(x) * leaf_num
    return(data.frame(GDD = x, FW_g = y))
  } else {
    # Return just the value for the given GDD
    y <- logistic(gdd_value) * leaf_num
    return(data.frame(GDD = gdd_value, FW_g = y))
  }
}


leaf_DW_growth <- function(a = 1, b = 225, c = 34, gdd_value = NULL, leaf_num = 1) {
  # Helper to compute logistic leaf DW
  logistic <- function(x) a / (1 + exp(-(x - b) / c))

  if (is.null(gdd_value)) {
    # Return full curve
    x <- seq(0, 3000, by = 100)
    y <- logistic(x) * leaf_num
    return(data.frame(GDD = x, DW_g = y))
  } else {
    # Return just the value for the given GDD
    y <- logistic(gdd_value) * leaf_num
    return(data.frame(GDD = gdd_value, DW_g = y))
  }
}

#' @export
leaf_area_growth <- function(a = 8340, b = 78, c = 103, gdd_value = NULL, leaf_num = 1) {
  # Helper to compute logistic leaf area
  logistic <- function(x) a / (1 + exp(-(x - b) / c))

  if (is.null(gdd_value)) {
    # Return full curve
    x <- seq(0, 3000, by = 100)
    y <- logistic(x) * leaf_num
    return(data.frame(GDD = x, Area_mm = y))
  } else {
    # Return just the value for the given GDD
    y <- logistic(gdd_value) * leaf_num
    return(data.frame(GDD = gdd_value, Area_mm = y))
  }
}
