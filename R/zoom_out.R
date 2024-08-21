#' zoom_out function
#'
#' Change the region inquiry coordinates
#'
#' Belong to the server function in the zoom out button section
#'
#' @param region A region inquiry data frame
#'
#' @return A region inquiry data frame with modified coordinates
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @examples
#' #...
zoom_out <- function(region){
  #Zoom out : change start and end coordinates (-500 bp and +500 bp)
  region <- region %>% mutate(start = ifelse(region$start>500,region$start-500,region$start), end = region$end + 500)
  return(region)
}
