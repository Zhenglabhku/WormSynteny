#' plot_4 function
#'
#' Generate plot case 4 : no C. Elegans genes
#'
#' Belong to the ggg_plot function in the server function in every section
#'
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param genes An aligned genes data frame
#' @param links A links data frame
#' @param region A region inquiry data frame
#'
#' @return A gggenomes plot
#'
#' @import gggenomes
#' @import ggplot2
#'
#' @examples
#' #...
plot_4 <- function(seqs, genes, links, region){

  #Elegans_length
  elegans_length <- region$end - region$start

  p <- gggenomes(seqs = seqs, genes = genes, links = links) +
    geom_seq() +
    geom_gene(aes(fill = ifelse(is.na(Orthogroup) & nrow(genes) == 1, "NA", Orthogroup)), stroke = 0.5) +
    geom_bin_label(size = 4.5, fontface = "italic")+
    geom_link()+
    scale_x_continuous(labels = function(x) {
      x <- x + region$start
      round(x)}) +
    labs(fill = "Orthogroups") +
    scale_fill_manual(values = setNames(genes$color, genes$Orthogroup))+
    theme(legend.text=element_text(size=13),
          legend.title = element_text(size=15),
          axis.text.x=element_text(size=12))+
    ggtitle(bquote(italic("C. elegans") ~ "chr" ~ .(region$seqnames)))+
    theme(plot.title = element_text(hjust = 0.5))

  # Display the plot
  return(p)

}
