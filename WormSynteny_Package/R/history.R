#' history function
#'
#' Update the History list with the used tables, plot, string and vector
#'
#' Belong to the server function in every section
#'
#' @param filesHistory A list of lists
#' @param plot A gggenomes generated plot
#' @param genes An aligned genes data frame
#' @param seqs An aligned sequences data frame to be display
#' @param aligned_sequences An aligned sequences data frame used in gggenomes
#' @param links A links data frame
#' @param region A region inquiry data frame
#' @param prev_colors A vector of orthogroups and colors
#' @param modifications A string characterizing an app action
#'
#' @return An updated History list
#'
#' @examples
#' #...
history <- function(filesHistory, plot, genes, seqs, aligned_sequences, links, region, prev_colors, modifications){

  filesHistory$plots <- c(filesHistory$plots, list(plot))
  filesHistory$genes <- c(filesHistory$genes, list(genes))
  filesHistory$seqs <- c(filesHistory$seqs, list(seqs))
  filesHistory$aligned_sequences <- c(filesHistory$aligned_sequences, list(aligned_sequences))
  filesHistory$links <- c(filesHistory$links, list(links))
  filesHistory$region <- c(filesHistory$region, list(region))
  filesHistory$prev_colors <- c(filesHistory$prev_colors, list(prev_colors))
  filesHistory$modifications <- c(filesHistory$modifications, list(modifications))

  return()
}
