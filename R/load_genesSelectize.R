#' load_genesSelectize function
#'
#' Load the genes list in the Coordinates panel
#'
#' Belong to the server function independent of a section
#'
#' @param session A shiny app session
#'
#' @return An updated UI with the list of genes in the Coordinates panel
#'
#' @importFrom shiny updateSelectizeInput
#'
#' @examples
#' #...
load_genesSelectize <- function(session){
  updateSelectizeInput(session, "Genes", choices = list_genes, server = TRUE)
}
