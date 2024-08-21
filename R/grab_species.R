#' grab_species function
#'
#' Make a list of species that do not have aligned regions and genes
#'
#' Belong to the render_explanations function in the server function in every section
#'
#' @param genes An aligned genes data frame
#' @param seqs An aligned sequences data frame used in gggenomes
#'
#' @return A list of two vectors of missing genes and missing fragments
#'
#' @importFrom stringr str_extract
#'
#' @examples
#' #...
grab_species <- function(genes, seqs){

  ##Check which species do not have aligned regions at all
  #Species names list
  species <- c("bovis", "becei", "panamensis", "inopinata","elegans","tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")

  #Extract species names from seq_id column
  species_names <- str_extract(seqs$seq_id, "[:alpha:]+")

  #Find missing species
  missing_regions <- setdiff(species,species_names)

  #Check which species have aligned regions but no aligned genes
  #Extract species names from seq_id column
  species_names <- str_extract(genes$seq_id, "[:alpha:]+")

  #Find missing species
  missing_genes <- setdiff(species,species_names)

  return(list(regions = missing_regions, genes = missing_genes))
}
