# Declare global variables to avoid R CMD check warnings for non-standard evaluation (NSE)
#' @keywords internal
utils::globalVariables(c("Mask", "Beta", "."))
#' @keywords internal
utils::globalVariables(c("UMAP1", "UMAP2", "color"))
#' @keywords internal
utils::globalVariables(c(
  "Predicted", "Actual", "Freq", "Class", "F1_Score", "conf"
))