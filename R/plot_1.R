#' plot_1 function
#'
#' Generate plot case 1 : no genes and links
#'
#' Belong to ggg_plot function in the server function in every section
#'
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param region A region inquiry data frame
#'
#' @return A gggenomes plot
#'
#' @import gggenomes
#' @import ggplot2
#'
#' @examples
#' #...
plot_1 <- function(seqs, region){

  p <- gggenomes(seqs = seqs) +
    geom_seq() +
    geom_bin_label(fontface = "italic",size = 4.5) +
    scale_x_continuous(labels = function(x) {
      x <- x + region$start
      round(x)})+
    theme(axis.text.x=element_text(size=11))

  return(p)
}
