#' extras_fragments function
#'
#' Take extras_fragments from top fragments tables
#'
#' Belong to the pipeline function in step 5 : separate the longest homologous fragments from the others
#'
#' @param extras_fragments A data frame containing the extras fragments
#' @param top_fragments A data frame containing all fragments
#'
#' @return A data frame of extras fragments
#'
#' @importFrom dplyr anti_join
#'
#' @examples
#' #...
extras_fragments <- function(extras_fragments, top_fragments){
  if(is.null(top_fragments)){
    return(top_fragments)
  }
  else if(nrow(top_fragments)==0){
    return(top_fragments)
  }
  else{
    extras_fragments <- anti_join(extras_fragments, top_fragments)
    return(extras_fragments)
  }
}
