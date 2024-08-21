#' genes_fct function
#'
#' Intersect genes from aligned fragments tables
#'
#' Belong to the pipeline function in step 6 : find genes/piece of genes in the aligned regions
#'
#' @param species A species specific data frame
#' @param final_species A data frame of the top fragments
#'
#' @return An aligned genes data frame
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#'
#' @examples
#' #...
genes_fct <- function(species, final_species){

  if(is.null(final_species)){
    return(final_species)
  }else if(nrow(final_species)!=0){
    genes_species <- subset_by_intersect(species, final_species) %>% select(seqnames, start, end) %>% mutate(length = (end - start)+1) %>% arrange(seqnames, start)
    return(genes_species)
  }else{
    genes_species <- final_species
    return(genes_species)
  }
}
