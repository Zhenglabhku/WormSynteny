#' show_fct function
#'
#' Update aligned genes, aligned_sequences (displayed), sequences (gggenomes) and links tables accroding to the user species selection
#'
#' Belong to the server function in the show button section
#'
#' @param genes An aligned genes data frame
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param aligned_sequences An aligned sequences data frame to display
#' @param selected_species A list of species names
#'
#' @return A list of data frames
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#'
#' @examples
#' #...
show_fct <- function(genes, seqs, aligned_sequences, selected_species){

  #Create a list
  data <- list(genes = genes, seqs = seqs, aligned_sequences = aligned_sequences, links = NULL)

  #Add elegans species to the list
  new_selection <- add_elegans(selected_species)

  #Update genes if there is some
  if(nrow(genes)!=0){

    #Create a new genes file based on this selection
    genes <- genes %>% filter(str_detect(seq_id, paste(new_selection, collapse = "|")))

    #Save it
    data$genes <- genes
  }

  #Update seqs
  seqs <- seqs %>% filter(str_detect(seq_id, paste(new_selection, collapse = "|")))

  #Save it
  data$seqs <- seqs

  #Update aligned sequences table
  aligned_sequences <- aligned_sequences %>% filter(str_detect(seq_id, paste(new_selection, collapse = "|")))

  #Save it
  data$aligned_sequences <- aligned_sequences

  #Update links
  list_df <- system_linux_2(new_selection)

  #Make links between the selected species
  df <- make_links(list_df, new_selection)

  # Function to create individual data frames
  individual_dfs <- create_individual_dfs(df)

  # Combined dfs (rbind)
  links <- combine_dfs(individual_dfs)

  #Test if links is null
  if(!(is.null(links))){
    links <- links %>% separate(strand, into = c("strand", "strand2"), sep = "(?<=[+|-])(?=[+|-])")
  }else{
    links <- NULL
  }

  data$links <- links

  return(data)
}
