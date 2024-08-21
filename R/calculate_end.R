#' calculate_end function
#'
#' Calculate the end column of a PSL file
#'
#' Belong to the sort_halLiftover_data function in the pipeline function of the step 1 : retrieve the homologous fragments of the C. elegans sequence of inquiry
#'
#' @param df A data frame issued from halLiftover linux command
#'
#' @return A data frame with end column calculated
#'
#' @examples
#' #...
calculate_end <- function(df) {
  df$blockSizes_list <- lapply(strsplit(df$blockSizes, ","), as.numeric)
  df$tStarts_list <- lapply(strsplit(df$tStarts, ","), as.numeric)

  df$end <- mapply(function(sizes, starts) {
    end_list <- numeric()
    for (i in seq_along(sizes)) {
      end <- starts[i] + sizes[i]
      end_list <- c(end_list, end)
    }
    return(paste(end_list, collapse = ","))
  }, df$blockSizes_list, df$tStarts_list)

  new_df <- data.frame(seqnames = df$seqnames,
                       start = df$tStarts,
                       end = df$end,
                       length = df$blockSizes)

  return(new_df)
}
