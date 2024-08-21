#' plot_2 function
#'
#' Generate plot case 2 : no links
#'
#' Belong to the ggg_plot function in the server function in every section
#'
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param genes An aligned genes data frame
#' @param region A region inquiry data frame
#' @param elegans_file A data frame containg C. Elegans genes names of genes table
#'
#' @return A gggenomes plot
#'
#' @import gggenomes
#' @importFrom cowplot get_legend
#' @importFrom cowplot plot_grid
#' @import ggplot2
#'
#'
#' @examples
#' #...
plot_2 <- function(seqs, genes, region, elegans_file){

  #Elegans_length
  elegans_length <- region$end - region$start

  #Get the gene_name information
  elegans_genes <- gene_name_fct(genes, region, elegans_file)

  p1 <- gggenomes(seqs = seqs, genes = genes) +
    geom_seq() +
    geom_gene(aes(fill = ifelse(is.na(Orthogroup) & nrow(genes) == 1, "NA", Orthogroup)), stroke = 0.5) +
    geom_bin_label(size = 4.5, fontface = "italic")+
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


  ## Create a second legend for C.elegans genes
  p2 <- gggenomes(seqs = seqs, genes = elegans_genes) +
    geom_seq() +
    geom_gene(aes(fill = gene_name), stroke = 0.5) +
    geom_bin_label(size = 4.5, expand_left = 0.8)+
    labs(fill =bquote(italic("C. elegans") ~ "genes"))+
    scale_fill_manual(values = setNames(elegans_genes$color, elegans_genes$gene_name))+
    theme(legend.text=element_text(size=13, face = "italic"),
          legend.title = element_text(size=15))

  #Remove the first legend
  p3 <- p1 + theme(legend.position = "none")

  # Combine the two legends using cowplot
  legend1 <- get_legend(p1)
  legend2 <- get_legend(p2)
  combined_legend <- plot_grid(legend1, legend2, ncol = 1, align = "v", rel_heights = c(1, 1))

  # Add the combined legend to the plot
  plot_with_legend <- plot_grid(p3, combined_legend, ncol = 2, align = "h", axis = "tb", rel_widths = c(0.7, 0.3))

  # Display the plot
  return(plot_with_legend)
}
