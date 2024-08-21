#' reverse_blockSizes function
#'
#' Reverse blocksizes depending of the strand
#'
#' Belong to the sort_haliftover_data function in the pipeline function in step 1 : retrieve the homologous fragments of the C. Elegans sequence of inquiry
#'
#' @param df A data frame of aligned fragments from halLiftover
#'
#' @return A data frame with reverse blockSizes
#'
#' @examples
#' #...
reverse_blockSizes <- function(df) {
  df$blockSizes <- sapply(strsplit(df$blockSizes, ","), function(x) paste(rev(x), collapse = ","))
  return(df)
}
