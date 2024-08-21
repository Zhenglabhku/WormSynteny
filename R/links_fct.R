#' links_fct function
#'
#' Select the columns that match the requirements of links table in gggenomes
#'
#' Belong to the pipeline function in step 2 : find pairwise alignment between phylogenetically adjacent species using the homologous fragments
#'
#' @param species1_species2 A data frame of links
#'
#' @return A data frame of links
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#'
#' @examples
#' #...
links_fct <- function(species1_species2){
  if(is.null(species1_species2)){
    return(species1_species2)
  }else{
    links_species <- species1_species2 %>% select(c("start", "end","seq_id","strand", "start2", "end2", "seq_id2"))
    return(links_species)
  }
}
