#' create_individual_dfs function
#'
#' Create individual data frames from links table
#'
#' Belong to the show_fct in the server function in the show button section
#'
#' @param links A links data frame
#'
#' @return A list of data frames
#'
#' @examples
#' #...
create_individual_dfs <- function(links) {
  individual_dfs <- list()
  for (i in seq_along(links)){
    df_name <- names(links)[[i]]
    df <- links[[i]][, c("start", "end", "seq_id","strand", "start2", "end2", "seq_id2")]
    individual_dfs[[df_name]] <- df
  }
  return(individual_dfs)
}
