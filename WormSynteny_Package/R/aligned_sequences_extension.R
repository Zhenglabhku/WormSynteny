#' aligned_sequences_extension function
#'
#' Update aligned sequences table to be display
#'
#' Belong to the server function in the full genes extension button section
#'
#' @param aligned_sequences An aligned sequences data frame to be display
#' @param seqs An aligned sequences used in gggenomes
#'
#' @return An updated aligned sequences data frame to be display
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @examples
#' #...
aligned_sequences_extension <- function(aligned_sequences, seqs){

  df_top <- seqs %>% filter(!(str_detect(seq_id, "filtered")))
  df_bottom <- aligned_sequences %>% filter(str_detect(seq_id, "filtered"))

  df <- rbind(df_top, df_bottom)

  return(df)
}

