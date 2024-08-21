#' convert_strand function
#'
#' Convert tEnds and tStarts columns if the strand negative
#'
#' Belong to the sort_halLiftover_data function of the pipeline function in step 1 : retrieve the homologous fragments of the C. Elegans sequence of inquiry
#'
#' @param df A data frame from HalLiftover with a strand column
#'
#' @return A data frame with two strands columns (query and target)
#'
#' @examples
#' #...
convert_strand <- function(df) {
  df$tSize <- as.numeric(df$tSize)
  tStarts <- strsplit(df$tStarts, ",")
  tEnds <- vector("list", length(tStarts))

  for (i in seq_along(tStarts)) {
    starts <- as.numeric(tStarts[[i]])
    ends <- df$tSize[[i]] - starts
    tEnds[[i]] <- paste(rev(ends), collapse = ",")
  }

  df$tEnds <- sapply(tEnds, function(x) paste(unlist(strsplit(x, ",")), collapse = ","))
  df$tStarts <- sapply(tStarts, function(x) paste(rev(x), collapse = ","))

  return(df)
}
