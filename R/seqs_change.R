#' seqs_change function
#'
#' Change the aligned sequences data frame coordinates to be used in gggenomes according to extended genes
#'
#' Belong to the server function in the full genes extension button section
#'
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param genes An aligned genes data frame
#'
#' @return An aligned sequences data frame used in gggenomes
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarize
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#'
#' @examples
#' #...
seqs_change <- function(seqs, genes){

  #Keep filtered seqs
  filtered_seqs <- seqs %>% filter(str_detect(seq_id, "filtered"))

  #Select top seqs
  top_seqs <- seqs %>% filter(!str_detect(seq_id, "filtered"))

  #Keep sequences order
  seqs_order <- unique(top_seqs$seq_id)

  #Merge sequences together and put them back in order
  top_seqs <- top_seqs %>% group_by(bin_id, seq_id, seqnames) %>% summarize(start = min(start), end = max(end), length = (end-start)+1)
  top_seqs <- top_seqs %>% arrange(match(seq_id, seqs_order))

  #Select top genes
  top_genes <- genes %>% filter(!str_detect(seq_id, "filtered"))

  #Create a data frame to store them
  new_seqs <- data.frame(NULL)

  #List of species name to select genes in function of species
  spe <- c("bovis", "becei", "panamensis", "inopinata", "elegans", "tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")

  #Loop over the list to act on each species genes
  for(i in 1:11){

    #Select genes from one species
    genes_species <- top_genes %>% filter(str_extract(seq_id, "[:alpha:]+") %in% spe[i])
    seqs_species <- top_seqs %>% filter(str_extract(seq_id, "[:alpha:]+") %in% spe[i])

    #If no genes for that species
    if(nrow(genes_species)==0){

      #Clean and store them
      seqs_species <- seqs_species %>% select(bin_id, seq_id, seqnames, start, end, length)
      new_seqs <- rbind(new_seqs, seqs_species)

      #If genes for that species
    }else{

      #Reconfigure the seqs
      seqs_species <- seqs_species %>% mutate(start = min(seqs_species$start, genes_species$start), end = max(seqs_species$end, genes_species$end), length = (end-start)+1)

      #Store them
      seqs_species <- seqs_species %>% select(bin_id, seq_id, seqnames, start, end, length)
      new_seqs <- rbind(new_seqs, seqs_species)
    }
  }
  seqs <- rbind(new_seqs, filtered_seqs)
  return(seqs)
}
