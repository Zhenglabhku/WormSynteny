#' split_start_end function
#'
#' Unlist start and end columns
#'
#' Belong to the sort_haliftover_data function in the pipeline function in step 1 : retrieve the homologous fragments of the C. Elegans sequence of inquiry
#'
#' @param df A data frame issued from halLiftover linux command
#'
#' @return A data frame with unlist start and end columns
#'
#' @examples
#' #...
split_start_end <- function(df) {
  new_df <- data.frame()

  for (i in 1:nrow(df)) {
    starts <- unlist(strsplit(df$start[i], ","))
    ends <- unlist(strsplit(df$end[i], ","))
    length <- unlist(strsplit(df$length[i], ","))
    seqnames <- rep(df$seqnames[i], length(starts))

    temp_df <- data.frame(seqnames, start = as.numeric(starts), end = as.numeric(ends), length = as.numeric(length))
    new_df <- rbind(new_df, temp_df)
  }

  return(new_df)
}
