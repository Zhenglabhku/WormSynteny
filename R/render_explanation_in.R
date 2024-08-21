#' render_explanation_in function
#'
#' Explanations related to the zoom in button
#'
#' Belong to the server function in zoom in section
#'
#' @param output An output from a shiny app session
#' @param region A region inquiry
#'
#' @return A string for the explanation panel
#'
#' @examples
#' #...
render_explanation_in <- function(output,region){

  output$explanation <- renderText({
    explanation0 <- paste("Zooming in.")
    explanation1 <- paste("Previous start position : ", region$start-500, "New start position : ", region$start)
    explanation2 <- paste("Previous end position : ", region$end+500, "New end position", region$end)

    explanation <- paste(explanation0,explanation1,explanation2,  sep = "\n")
    explanation
  })

}
