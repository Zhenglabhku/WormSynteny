#' add_seq_id_extras function
#'
#' Add seq_id column to extras fragments tables
#'
#' Belong to the pipeline function in step 11 : create aligned sequences, aligned genes and links tables for gggenomes
#'
#' @param df A data frame of one species
#' @param species A string that represent a species name
#'
#' @return A data frame with seq_id column added
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#'
#' @examples
#' #...
add_seq_id_extras <- function(df, species) {
  if(!is.null(df)){
    seq_id <- str_c(species, df$seqnames, sep = "_")
    seq_id <- str_c(seq_id, "(filtered)", sep = " ")
    df <- df %>% mutate(seq_id = seq_id)
  }
  return(df)
}
