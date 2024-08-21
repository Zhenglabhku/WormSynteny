#' ggg_plot function
#'
#' Create gggenomes plots in functions of the three main tables
#'
#' Belong to the the server function in every section
#'
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param genes An aligned genes data frame
#' @param links A links data frame
#' @param region An inquiry region data frame
#' @param elegans_file The C. Elegans genes data frame
#'
#' @return A gggenomes plot
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#'
#' @examples
#' #...
ggg_plot <- function(seqs, genes, links, region, elegans_file){

  #If sequences have only one row : no need to bother with links
  if(nrow(seqs)== 1){

    if(!is.null(genes)){
      return(plot_2(seqs, genes, region, elegans_file))

    }else{
      return(plot_1(seqs, region))

    }
    #If sequences have more than one row : need to bother with links
  }else{

    #No links, no genes but some alignments
    if(is.null(links) & is.null(genes)){
      return(plot_1(seqs, region))

      #No links but genes
    }else if(is.null(links) & !is.null(genes)){
      return(plot_2(seqs, genes, region, elegans_file))

    }
    #Links and genes
    else if(!is.null(links) & !is.null(genes)){

      #Links present in the plot
      if(nrow(links)!=0 & has_links(seqs, links)){

        elegans_genes <- genes %>% filter(str_detect(seq_id, "elegans"))

        #Presence of elegans genes
        if(nrow(elegans_genes)!=0){
          return(plot_3(seqs, genes, links, region, elegans_file))

          #No elegans genes
        }else{
          return(plot_4(seqs, genes, links, region))

        }
        #No links or links but no containing the right coordinates
      }else{
        elegans_genes <- genes %>% filter(str_detect(seq_id, "elegans"))

        #Presence of elegans genes
        if(nrow(elegans_genes)!=0){
          return(plot_2(seqs, genes, region, elegans_file))

          #No elegans genes
        }else{
          return(plot_5(seqs, genes, region))

        }
      }
    }
  }
}
