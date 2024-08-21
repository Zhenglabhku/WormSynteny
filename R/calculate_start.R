#' calculate_start function
#'
#' Calculate the start column of a PSL file
#'
#' Belong to the sort_halLiftover_data function in the pipeline function of the step 1 : retrieve the homologous fragments of the C. Elegans sequence of inquiry
#'
#' @param df A data frame issued from halLiftover linux command
#'
#' @return A data frame with start column calculated
#'
#' @examples
#' #...
calculate_start <- function(df) {
  df$blockSizes_list <- lapply(strsplit(df$blockSizes, ","), as.numeric)
  df$tEnds_list <- lapply(strsplit(df$tEnds, ","), as.numeric)

  df$start <- mapply(function(sizes, ends) {
    start_list <- numeric()
    for (i in seq_along(sizes)) {
      start <- ends[i] - sizes[i]
      start_list <- c(start_list, start)
    }
    return(paste(start_list, collapse = ","))
  }, df$blockSizes_list, df$tEnds_list)

  new_df <- data.frame(seqnames = df$seqnames,
                       start = df$start,
                       end = df$tEnds,
                       length = df$blockSizes)

  return(new_df)
}
