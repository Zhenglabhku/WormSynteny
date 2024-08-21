#' update_sequences_table function
#'
#' Update the aligned sequences table to be display when switching genes
#'
#' Belong to the server function in the switch button section
#'
#' @param list_species A list of string that represents species names
#' @param seqs An aligned sequences data frame to be display
#'
#' @return An updated aligned sequences data frame to be display
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr anti_join
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#' @importFrom stringr str_remove
#'
#' @examples
#' #...
update_sequences_table <- function(list_species, seqs){

  #change list_species
  l <- lapply(list_species, function(x){str_extract(x, "(?<=C\\. )\\w+")})

  #Select seqs and extras_seqs
  extras_seqs <- seqs %>% filter(str_detect(seq_id, "\\(filtered\\)"))

  if(nrow(extras_seqs)== 0){
    return(seqs)
  }else{
    seqs <- seqs %>% filter(!(str_detect(seq_id, "\\(filtered\\)")))

    #Go through the species name list
    for (k in 1 : length(l)) {

      species <- l[[k]]

      #Save the selected sequences
      select_seqs <- seqs %>% filter(str_detect(seq_id,species))
      select_extras_seqs <- extras_seqs %>% filter(str_detect(seq_id,species)) %>% filter(length %in% max(length)) %>% dplyr::slice(1)

      #Remove species the sequences
      seqs <- anti_join(seqs, select_seqs)
      extras_seqs <- anti_join(extras_seqs, select_extras_seqs)

      #create the reconfigured sequences
      select_extras_seqs <- select_extras_seqs %>% mutate(seq_id = str_remove(seq_id, " \\(filtered\\)"), bin_id =  str_remove(bin_id, " \\(filtered\\)"))
      select_seqs <- select_seqs %>% mutate(seq_id = str_c(seq_id, " (filtered)"), bin_id =  str_c(bin_id, " (filtered)"))

      #Save everything
      seqs <- rbind(seqs, select_extras_seqs)
      extras_seqs <- rbind(extras_seqs, select_seqs)

    }

    #Order seqs
    # Create a vector with the desired order of the species names
    species_order <-  c("bovis", "becei", "panamensis", "inopinata","elegans", "tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")

    # Extract the species name from the seq_id column using regular expressions
    seqs$species <- str_extract(seqs$seq_id, "[:alpha:]+")
    extras_seqs$species <- str_extract(extras_seqs$seq_id, "[:alpha:]+")

    # Convert the species column to a factor with the desired order
    seqs$species <- factor(seqs$species, levels = species_order)
    extras_seqs$species <- factor(extras_seqs$species, levels = species_order)

    # Order the rows of the data frame by the species column
    seqs <- seqs[order(seqs$species), ] %>%  select(-species)
    extras_seqs <- extras_seqs[order(extras_seqs$species), ] %>%  select(-species)

    #Seqs total
    seqs <- rbind(seqs, extras_seqs)
    return(seqs)
  }
}
