#' Generate pattern level data for cell type annotation
#'
#' @param query_fn File path to query .cg
#' @param knowledge_fn File path to pattern file .cm
#' @return A cell by pattern matrix.
#' @useDynLib MethScope, .registration = TRUE
#' @importFrom dplyr select
#' @importFrom stringr str_extract
#' @importFrom tidyr spread
#' @importFrom utils read.table
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @export
#' @examplesIf .Platform$OS.type != "windows"
#' qry <- system.file("extdata", "toy.cg", package = "MethScope")
#' msk <- system.file("extdata", "toy.cm", package = "MethScope")
#' res <- GenerateInput(qry, msk)

GenerateInput <- function(query_fn, knowledge_fn) {

  stopifnot(is.character(query_fn), is.character(knowledge_fn))
  if (.Platform$OS.type == "windows") {
    stop("Testing sequencing data does not support Windows. Please directly use yame to generate inputs.")
  }
  #yame_result <- .Call("yame_summary_cfunc", query_fn, knowledge_fn)
  
  temp_file <- tempfile(fileext = ".txt")
  .Call("yame_summary_cfunc", query_fn, knowledge_fn, temp_file)
  summary_results <- data.table::fread(temp_file, header = TRUE)
  on.exit(unlink(temp_file), add = TRUE)
  summary_results <- summary_results %>%
    dplyr::select('Query','Mask','Beta') %>%
    tidyr::spread(key=Mask,value=Beta,fill = NA)

  column_order <- order(as.numeric(stringr::str_extract(colnames(summary_results), "\\d+")))
  summary_results <- summary_results[, column_order]
  rownames(summary_results) <- summary_results$Query
  summary_results <- summary_results %>% dplyr::select(-"Query")
  summary_results
}


#' Generate reference pattern labels (no default writing)
#'
#' @param binary_file Path to the pattern strings file (one string per line).
#' @param min_CG Minimum CpG count a pattern must have to keep its own ID (default: 50).
#'   Patterns with frequency <= `min_CG` are grouped as "Pna".
#' @param output_path Optional file path to write the resulting labels. If `NULL` (default),
#'   nothing is written and the labels are only returned.
#' @return A character vector of pattern labels (same length/order as the input file).
#' @export
#' @examples
#' \dontrun{
#' # DO write only to a temp location in examples/vignettes/tests:
#' tmp_out <- file.path(tempdir(), "patterns.txt")
#' labs <- GenerateReference("path/to/pattern_strings.txt", min_CG = 50, output_path = tmp_out)
#' # Or skip writing and just get the vector:
#' labs <- GenerateReference("path/to/pattern_strings.txt", min_CG = 50)
#' }
GenerateReference <- function(binary_file, min_CG = 50, output_path = NULL) {
  reference_set <- readLines(binary_file)
  freq_df <- as.data.frame(table(reference_set), stringsAsFactors = FALSE)
  ord <- order(-freq_df$Freq)
  freq_df <- freq_df[ord, , drop = FALSE]
  freq_df$pattern <- paste0("P", seq_len(nrow(freq_df)))
  
  if (any(freq_df$Freq <= min_CG)) {
    freq_df$pattern[freq_df$Freq <= min_CG] <- "Pna"
  }
  
  patterns_out <- freq_df$pattern[match(reference_set, freq_df$reference_set, nomatch = NA)]
  
  # Optional write (only if user explicitly supplies a path)
  if (!is.null(output_path)) {
    utils::write.table(
      patterns_out,
      file = output_path,
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE
    )
  }
  
  return(patterns_out)
}

