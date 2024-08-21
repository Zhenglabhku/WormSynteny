#' resetCoordinates function
#'
#' Reset coordinates with default coordinates
#'
#' Belong to the server function in the reset button section
#'
#' @param session A shiny app session
#'
#' @return Coordinates updated on the Coordinates panel
#'
#' @examples
#' #...
resetCoordinates <- function(session) {

  # Update the inputs with the fetched coordinates
  updateSelectInput(session, "Chr", selected = "X")
  updateNumericInput(session, "Start", value = 4271165)
  updateNumericInput(session, "End", value = 4274216)
  updateNumericInput(session, "Gap", value = 200)
  updateNumericInput(session, "Filter", value = 16)

}
