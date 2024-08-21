#' zoom_in function
#'
#' Change the region inquiry coordinates
#'
#' Belong to the server function in the zoom in button section
#'
#' @param region A region inquiry data frame
#'
#' @return A region inquiry data frame with modified coordinates
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @examples
#' #...
zoom_in <- function(region){
  #Zoom in : change start and end coordinates (+500 bp and -500 bp)
  region <- region %>% mutate(chr = region$chr, start2 = ifelse((region$end-500)-(region$start+500)<=0, region$start, region$start+500), end = ifelse((region$end-500)-(region$start+500)<=0, region$end, region$end-500)) %>% select(-start) %>% rename(start = "start2")
  return(region)
}
