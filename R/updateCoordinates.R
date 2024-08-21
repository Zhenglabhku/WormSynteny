#' updateCoordinates function
#'
#' Update coordinates in the Coordinates panel when the user is selecting a gene
#'
#' Belong to the server function out of a section
#'
#' @param session A shiny app session
#' @param region A region inquiry data frame
#'
#' @return Updated Coordinates panel
#'
#' @examples
#' #...
updateCoordinates <- function(session, region) {

  # Update the inputs with the fetched coordinates
  updateSelectInput(session, "Chr", selected = region$seqnames)
  updateNumericInput(session, "Start", value = region$start)
  updateNumericInput(session, "End", value = region$end)
  updateNumericInput(session, "Gap", value = region$gap)
  updateNumericInput(session, "Filter", value = region$filter_percentage)

  # Update the selectizeInput with the gene choices
  updateSelectizeInput(session, "Genes", choices = list_genes, server = TRUE)
}
