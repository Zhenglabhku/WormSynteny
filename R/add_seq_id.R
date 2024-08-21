#' add_seq_id function
#'
#' Add seq_id column to top fragments tables
#'
#' Belong to the pipeline function in step 11 : create aligned sequences, aligned genes and links tables for gggenomes
#'
#' @param df A species specific data frame
#' @param species A string that represents a species name
#'
#' @return A data frame with seq_id column added
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#'
#' @examples
#' #...
add_seq_id <- function(df, species) {
  if(!is.null(df)){
    seq <- str_c(species, df$seqnames, sep = "_")
    df <- df %>% mutate(seq_id = seq)
  }
  return(df)
}
