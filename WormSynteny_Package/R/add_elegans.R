#' add_elegans function
#'
#' Add C. Elegans species name into a vector of string
#'
#' Belong to the server function in the show button section
#'
#' @param input_list A list of strings which represent species names
#'
#' @return A vector of species names to which the species C. Elegans has been added
#'
#' @importFrom stringr str_extract
#'
#' @examples
#' #...
add_elegans <- function(input_list) {

  #change list_species
  input_list <- lapply(input_list, function(x){str_extract(x, "(?<=C\\. )\\w+")})
  if (!"elegans" %in% input_list) {
    input_list <- c(input_list, "elegans")
  }
  species <- c("bovis", "becei", "panamensis", "inopinata", "elegans", "tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")
  sorted_list <- input_list[order(match(input_list, species))]
  return(sorted_list)
}
