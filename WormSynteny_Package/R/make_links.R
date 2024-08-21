#' make_links function
#'
#' Make new links according to the user species selection
#'
#' Belong to the server function in the show button section
#'
#' @param list_df A list of aligned fragments data frames
#' @param new_selection A list of species selected by the user
#'
#' @return A list
#'
#' @examples
#' #...
make_links <- function(list_df, new_selection){
  l <- list()
  for(i in 1 : (length(new_selection)-1)){
    species_name <- paste0(new_selection[[i]],"_",new_selection[[i+1]])
    l[[species_name]] <- pslToDataFrame2(list_df,species_name)
  }
  return(l)
}
