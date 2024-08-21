#' seqs_fct function
#'
#' Format the aligned sequences data frame :  add bin_id
#'
#' Belong to the pipeline function in step 11 : create aligned sequences, aligned genes and links tables for gggenomes
#'
#' @param genes An aligned genes data frame
#' @param final_data A data frame of aligned fragments
#' @param species A specific species data frame
#'
#' @return A formatting data frame of aligned sequences
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#'
#' @examples
#' #
seqs_fct <- function(genes, final_data, species){
  if(!is.null(genes)){
    seqs_species <- add_seq_id(final_data, species)
    seqs_species <- seqs_species %>% mutate(bin_id = str_c("C.", str_extract(seq_id, "[:alpha:]+"), sep = " ")) %>% select(c("seqnames", "start", "end", "length", "seq_id", "bin_id"))
  }else{
    seqs_species <- NULL
  }
  return(seqs_species)
}
