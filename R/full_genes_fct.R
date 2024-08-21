#' full_genes_fct function
#'
#' Extend genes to their full length
#'
#' Belong to the server function in the full genes extension section
#'
#' @param genes A data frame of aligned genes
#' @param species A data frame of genes of a certain species
#'
#' @return A data frame of extended aligned genes
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#'
#' @examples
#' #...
full_genes_fct <- function(genes, species){

  #keep order of genes
  seqs_order <- unique(genes$seq_id)

  #keep filtered genes
  genes_filtered <- genes %>% filter(str_detect(seq_id, "filtered"))

  #Select top genes only
  top_genes <- genes %>% filter(!str_detect(seq_id, "filtered"))

  #If multiple seqs : merge them into one sequence
  top_genes <- top_genes %>% group_by(gene_id) %>% filter(length == max(length))

  #Create a data frame to store them
  new_genes <- data.frame(NULL)

  #List of species name to select genes in function of species
  spe <- c("bovis", "becei", "panamensis", "inopinata", "elegans", "tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")

  #Loop over the list to act on each species genes
  for(i in 1:11){

    #Select genes from one species
    genes_species <- top_genes %>% filter(str_extract(seq_id, "[:alpha:]+") %in% spe[i])

    #If no genes for that species
    if(nrow(genes_species)==0){

      #Store it into the created data frame
      genes_species <- genes_species %>% mutate(types =NA, Orthogroup =NA) %>% select(seq_id, seqnames, start, end, length, strand, types, gene_id, gene_biotype, Orthogroup)
      new_genes$types <- as.character(new_genes$types)
      new_genes$Orthogroup <- as.character(new_genes$Orthogroup)
      new_genes <- rbind(new_genes, genes_species)

      #If there is genes for that species > extend them and store them
    }else{

      #Extending genes
      genes_species <- subset_by_overlap(species[[i]], genes_species)
      genes_species <- add_seq_id(genes_species, spe[i])

      #Storing
      genes_species <- genes_species %>% rename(types = "type", length = "width") %>% select(seq_id, seqnames, start, end, length, strand, types, gene_id, gene_biotype, Orthogroup)
      new_genes <- rbind(new_genes, genes_species)

    }

  }

  #Join the two new dfs together
  genes <- rbind(new_genes, genes_filtered)
  genes <- genes %>% arrange(match(seq_id, seqs_order))
  genes <- genes %>% distinct()


  return(genes)
}
