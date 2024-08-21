#' render_explanations function
#'
#' Render explanations on the UI
#'
#' Belong to the server function in every section
#'
#' @param output An output from a shny app session
#' @param genes An aligned genes data frame
#' @param seqs An aligned sequences data frame
#'
#' @return Display messages on the explanation panel
#'
#' @examples
#' #...
render_explanations <- function(output,genes, seqs){

  list_df <- grab_species(genes, seqs)
  messages <- messages(list_df$regions, list_df$genes)

  output$explanation <- renderPrint({cat(messages)})
}
