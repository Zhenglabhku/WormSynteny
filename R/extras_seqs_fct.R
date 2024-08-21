#' extras_seqs_fct function
#'
#' Create a data frame of extras sequences
#'
#' Belong to the pipeline function in step 11 : create aligned sequences, aligned genes and links tables for gggenomes
#'
#' @param extras_genes A data frame of extras genes
#' @param extras_fragments A data frame of extras fragments
#' @param species A species specific data frame
#'
#' @return A data frame of extras sequences
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom stringr str_extract
#'
#' @examples
#' #...
extras_seqs_fct <- function(extras_genes, extras_fragments, species){
  if(!is.null(extras_genes)){
    seqs_species <- add_seq_id_extras(extras_fragments, species)
    extras_seqs <- seqs_species %>% mutate(bin_id = str_c(str_c("C. ", str_extract(seq_id, "[:alpha:]+")), "(filtered)", sep = " "))
  }else{
    extras_seqs <- NULL
  }
  return(extras_seqs)
}
