#' switch_fct function
#'
#' Switch button : switch extras fragments from bottom to top and vice versa
#'
#' Belong to the server function in the switch button section
#'
#' @param list_species A vector of string that represent species name selected by the user
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param genes An aligned genes data frame
#' @param aligned_sequences An aligned sequences data frame to display
#'
#' @return A list of two data frames : an aligned gene
#' s data frame and an aligned sequences data frame used in gggenomes
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr anti_join
#' @importFrom dplyr rename
#' @importFrom tidyr unite
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#' @importFrom stringr str_remove
#'
#' @examples
#' #...
switch_fct <- function(list_species, seqs, genes, aligned_sequences){

  #change list_species
  l <- lapply(list_species, function(x){str_extract(x, "(?<=C\\. )\\w+")})

  #Select seqs and extras_seqs
  extras_seqs <- seqs %>% filter(str_detect(seq_id, "\\(filtered\\)"))

  if(nrow(extras_seqs)== 0){
    return(list(genes = genes, seqs = seqs))
  }else{

    #Select genes and extras
    extras_genes <- genes %>% filter(str_detect(seq_id, "\\(filtered\\)"))

    if(nrow(extras_genes)== 0){
      return(genes)
    }else{
      genes <- genes %>% filter(!(str_detect(seq_id, "\\(filtered\\)")))

      #Go through the species name list and switch genes and extras_genes for the corresponding species
      for (k in 1 : length(l)) {

        species <- l[[k]]

        ## Select the desired true length sequences
        aligned_sequences_2 <- aligned_sequences %>% filter(str_detect(seq_id,species))
        aligned_sequences_2 <- aligned_sequences_2 %>% filter(!(str_detect(seq_id, "\\(filtered\\)")))

        #Selected the desired genes and change its length based on the true length sequence
        selected_genes <- genes %>% filter(str_detect(seq_id,species))
        selected_extras_genes <- extras_genes %>% filter(str_detect(seq_id,species))

        selected_extras_genes <- subset_by_overlap(selected_extras_genes, aligned_sequences_2)

        #Remove the old ones
        genes <- anti_join(genes, selected_genes)
        extras_genes <- anti_join(extras_genes, selected_extras_genes)

        #create the reconfigured sequences
        selected_extras_genes <- selected_extras_genes %>% mutate(seq_id = str_remove(seq_id, " \\(filtered\\)")) %>% select(seq_id, seqnames, start, end, length, strand, types, gene_id, gene_biotype, Orthogroup)
        selected_genes <- selected_genes %>% mutate(seq_id = str_c(seq_id, " (filtered)")) %>% select(seq_id, seqnames, start, end, length, strand, types, gene_id, gene_biotype, Orthogroup)


        #Put them back in the opposite files
        genes <- rbind(genes, selected_extras_genes)
        extras_genes <- rbind(extras_genes, selected_genes)

      }

      # Order genes and extras_genes
      # Create a vector with the desired order of the species names
      species_order <-  c("bovis", "becei", "panamensis", "inopinata","elegans", "tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")

      # Extract the species name from the seq_id column using regular expressions
      genes$species <- str_extract(genes$seq_id, "[:alpha:]+")
      extras_genes$species <- str_extract(extras_genes$seq_id, "[:alpha:]+")

      # Convert the species column to a factor with the desired order
      genes$species <- factor(genes$species, levels = species_order)
      extras_genes$species <- factor(extras_genes$species, levels = species_order)

      # Order the rows of the data frame by the species column
      genes <- genes[order(genes$species), ] %>%  select(-species) %>% mutate(seq_id = str_remove(seq_id, " \\(filtered\\)"))
      extras_genes <- extras_genes[order(extras_genes$species), ] %>%  select(-species) %>% mutate(seq_id = str_remove(seq_id, " \\(filtered\\)")) %>%  mutate(filtered =  "(filtered)") %>% unite(seq_id, c("seq_id", "filtered"), sep = " ")
      genes <- rbind(genes, extras_genes)

      seqs <- aligned_sequences %>% filter(!(str_detect(seq_id, "\\(filtered\\)")))
      extras_seqs <- genes %>% filter(str_detect(seq_id, "\\(filtered\\)")) %>% select(seqnames, start, end, length, seq_id) %>% mutate(bin_id = str_c(str_c("C. ",str_extract(seq_id, "[:alpha:]+")), "(filtered)", sep = " "))

      seqs <- rbind(seqs, extras_seqs)

      return(list(genes = genes, seqs = seqs))
    }
  }
}
