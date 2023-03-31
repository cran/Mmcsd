#' Summarise the results of 'mmcsd' fit.
#'
#' @param object A mmcsd fitted model
#' @param ... Additional params passed to summary
#' @return Return NULL and print in terminal the results.
#'
#' @export
#'
#' @examples
#' \donttest{
#' fit <- mmcsd(
#'  score ~ wave + ageg + ecacg + qualifg,
#'  waves = wave, ids = id,
#'  weights = weight, stratum = strata, cluster = cluster,
#'  data = example_data, sigma = "exchangeable"
#' )
#' summary(fit)
#' }

summary.mmcsd <- function(object, ...) {
  coefficientsTable <- createCoefficientsTable(object)

  catCall(object)
  catCoefficientsTable(coefficientsTable, object)
  catSampleDesign(object)
  catSigma(object)
  catSigmaHat(object)

  return(invisible(NULL))
}
