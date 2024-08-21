#' micro function
#'
#' Transform the aligned genes data frame data into microsynteny data
#'
#' Belong to the server function in the microsynteny button section
#'
#' @param genes An aligned genes data frame
#' @param species A species specific data frame
#'
#' @return An aligned genes data frame with CDS gene type
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#'
#' @examples
#' #...
micro <- function(genes, species){

  #Select filtered genes
  filtered <- genes %>% filter(str_detect(seq_id, "filtered"))

  #If filtered genes
  if(nrow(filtered)!=0){

    #Create a data frame to store them
    genes_filtered <- data.frame(NULL)

    #List of species name to select genes in function of species
    spe <- c("bovis", "becei", "panamensis", "inopinata", "elegans", "tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")

    #Loop over the list to act on each species genes
    for(i in 1:11){

      #Select genes from one species
      genes_species <- filtered %>% filter(str_extract(seq_id, "[:alpha:]+") %in% spe[i])

      #If no genes for that species
      if(nrow(genes_species)==0){

        #Store it into the created data frame
        genes_filtered <- rbind(genes_filtered, genes_species)

        #If there is genes for that species > identified them and store them
      }else{
        #Identifying
        genes_species <- genes_fct(species[[i]], genes_species)
        genes_species <- metadata_fct(species[[i]], genes_species)
        genes_species <- check_overlapping(species[[i]], genes_species)
        genes_species <- add_seq_id(genes_species, spe[i])

        #Storing
        genes_species <- genes_species %>% rename(types = "type") %>% select(seq_id, seqnames, start, end, length, strand, types, gene_id, gene_biotype, Orthogroup)
        genes_filtered <- rbind(genes_filtered, genes_species)
      }
    }
    #Add back the specification for filtered genes
    genes_filtered <- genes_filtered %>% mutate(seq_id = paste(seq_id, "(filtered)"))

    #If there is no filtered genes at all
  }else{
    #Clean and store them
    genes_filtered <- filtered %>% mutate(types = NA, Orthogroup =NA) %>% select(seq_id, seqnames, start, end, length, strand, types, gene_id, gene_biotype, Orthogroup)
  }

  #Select genes
  top_genes <- genes %>% filter(!str_detect(seq_id, "filtered"))

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
      new_genes <- rbind(new_genes, genes_species)

      #If there is genes for that species > identified them and store them
    }else{
      #Identifying
      genes_species <- genes_fct(species[[i]], genes_species)
      genes_species <- metadata_fct(species[[i]], genes_species)
      genes_species <- check_overlapping(species[[i]], genes_species)
      genes_species <- add_seq_id(genes_species, spe[i])

      #Storing
      genes_species <- genes_species %>% rename(types = "type") %>% select(seq_id, seqnames, start, end, length, strand, types, gene_id, gene_biotype, Orthogroup)
      new_genes <- rbind(new_genes, genes_species)
    }
  }
  #Join them together
  genes <- rbind(new_genes, genes_filtered)
  genes <- genes %>% distinct()

  return(genes)
}
