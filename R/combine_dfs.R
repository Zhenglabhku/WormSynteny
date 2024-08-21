#' combine_dfs function
#'
#' Combine individual data frames
#'
#' Belong to the show_fct function in the server function in the show button section
#'
#' @param individual_dfs A list of data frames
#'
#' @return A data frame
#'
#' @examples
#' #...
combine_dfs <- function(individual_dfs) {
  combined_df <- do.call(rbind, individual_dfs)
  return(combined_df)
}
