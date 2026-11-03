#' @export
fruit_fw_double_sigmoid_growth <- function(
    gdd_value = NULL,
    A1 = 2.9893885, A2 = 1.2988551,
    m1s = 0.0000000, gamma = -0.4069436,
    log_s1 = -3.3456273, log_s2 = -2.4782438,
    hu_min = 0, hu_max = 3000, step = 100,
    fruit_num = 1
) {
  if (hu_max <= hu_min) stop("hu_max must be > hu_min")

  inv_logit <- function(z) 1 / (1 + exp(-z))
  sigma     <- function(x, m, s) 1 / (1 + exp(-(x - m) / s))
  scale_hu  <- function(x) (x - hu_min) / (hu_max - hu_min)
  x0s       <- scale_hu(0)

  s1  <- exp(log_s1)
  s2  <- exp(log_s2)
  m2s <- m1s + (1 - m1s) * inv_logit(gamma)

  f_pred <- function(x) {
    xs <- scale_hu(x)
    y  <- A1 * (sigma(xs, m1s, s1) - sigma(x0s, m1s, s1)) +
      A2 * (sigma(xs, m2s, s2) - sigma(x0s, m2s, s2))
    pmax(y, 0)
  }

  GDD <- if (is.null(gdd_value)) seq(hu_min, hu_max, by = step) else as.numeric(gdd_value)

  data.frame(
    Cum_GDD_base7 = GDD,
    FW_ind = f_pred(GDD) * fruit_num
  )
}

#' @export
fruit_dw_double_sigmoid_growth <- function(
    gdd_value = NULL,
    A1 = 0.1072093, A2 = 2.0317160,
    m1s = 0.1313343, gamma = -1.2904118,
    log_s1 = -3.6797071, log_s2 = -1.1204041,
    hu_min = 0, hu_max = 3000, step = 100,
    fruit_num = 1
) {
  if (hu_max <= hu_min) stop("hu_max must be > hu_min")

  inv_logit <- function(z) 1/(1 + exp(-z))
  sigma     <- function(x, m, s) 1/(1 + exp(-(x - m)/s))
  scale_hu  <- function(x) (x - hu_min) / (hu_max - hu_min)

  s1  <- exp(log_s1)
  s2  <- exp(log_s2)
  m2s <- m1s + (1 - m1s) * inv_logit(gamma)

  x0s <- scale_hu(0)

  f_pred <- function(x) {
    xs <- scale_hu(x)
    y  <- A1 * (sigma(xs, m1s, s1) - sigma(x0s, m1s, s1)) +
      A2 * (sigma(xs, m2s, s2) - sigma(x0s, m2s, s2))
    pmax(y, 0)
  }

  if (is.null(gdd_value)) {
    GDD <- seq(hu_min, hu_max, by = step)
    data.frame(Cum_GDD_base7 = GDD,
               DW_ind = f_pred(GDD) * fruit_num)
  } else {
    GDD <- as.numeric(gdd_value)
    data.frame(Cum_GDD_base7 = GDD,
               DW_ind = f_pred(GDD) * fruit_num)
  }
}
