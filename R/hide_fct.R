#' hide_fct function
#'
#' Hide the extras fragments in the aligned genes, aligned sequences and links tables
#'
#' Belong to the server function in the hide button section
#'
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param aligned_sequences An aligned sequences data frame to be diplay
#' @param genes An aligned genes data frame
#'
#' @return A list of data frames : aligned sequences, seqs, and genes
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @examples
#' #...
hide_fct <- function(seqs, aligned_sequences, genes){

  #Filter the filtered seqs
  seqs <- seqs %>% filter(!(str_detect(seq_id, "\\(filtered\\)")))

  #Filter the filtered aligned_sequences
  aligned_sequences <- aligned_sequences %>% filter(!(str_detect(seq_id, "\\(filtered\\)")))

  #Filter the filtered genes
  if(nrow(genes)!=0){
    genes <- genes %>% filter(!(str_detect(seq_id, "\\(filtered\\)")))
  }

  return(list(seqs = seqs, aligned_sequences = aligned_sequences, genes = genes))
}
