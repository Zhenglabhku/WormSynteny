#' gene_name_fct function
#'
#' Create a table with C. Elegans genes names, orthogroup and color information
#'
#' Belong to the plot_2 and plot_3 functions in the ggg_plot function from the server function in every section
#'
#' @param genes A data frame of aligned genes
#' @param region The region inquiry data frame
#' @param elegans The C. Elegans genes data frame
#'
#' @return A data frame of aligned genes with C. Elegans genes names
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom stringr str_detect
#'
#' @examples
#' #...
gene_name_fct <- function(genes, region, elegans){

  #Create the elegans gene_id data frame to link gene_id, orthogroup and color information
  elegans_length <- region$end - region$start
  elegans_rows <- genes %>% filter(str_detect(seq_id, "elegans"))
  elegans_metadata <- elegans %>% filter(gene_id %in% elegans_rows$gene_id) %>% select("gene_name", "gene_id")
  elegans_rows$gene_id <- as.character(elegans_rows$gene_id)
  elegans_genes <- left_join(elegans_rows, elegans_metadata, by = "gene_id")

  return(elegans_genes)
}
