#' render_others function
#'
#' Render the region inquiry information on the explanation panel
#'
#' Belong to the server function in every section
#'
#' @param output An ouput from a shiny app session
#' @param region A region inquiry data frame
#' @param input An input from a shiny app session
#'
#' @return Display the region inquiry information on the explanation panel
#'
#' @examples
#' #...
render_others <- function(output, region, input){

  if(!isTruthy(input)){
    s1 <- "Inquiry :\n"
    s2 <- paste0("Start : ", region$start, " End : ", region$end, " Chromosome : ", region$seqnames)
    s3 <- paste0(s1, s2, "\n")

  }else{
    s1 <- "Inquiry :\n"
    s2 <- paste0("C. elegans gene name : ", input)
    s3 <- paste0(s1, s2, "\n")
  }

  s <- paste0(s3, "\n", "Gap value : ", region$gap,"\nFiltering percentage : ", region$filtering_percentage)

  output$others <- renderPrint({cat(s)})
}
