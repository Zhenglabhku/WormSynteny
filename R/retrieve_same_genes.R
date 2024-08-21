#' retrieve_same_genes function
#'
#' Retrieve the same genes in the extras fragments and top fragments
#'
#' Belong to the pipeline function in step 10 : remove same genes from extras fragments and place them into top fragments
#'
#' @param genes_file A top genes data frame
#' @param extras_genes An extras genes data frame
#'
#' @return An extras genes data frame
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#'
#' @examples
#' #...
retrieve_same_genes <- function(genes_file, extras_genes){
  if(is.null(extras_genes)){
    return(extras_genes)
  }else{
    if(nrow(extras_genes)== 0){
      return(extras_genes)
    }
    else{
      extras_genes <- extras_genes %>% filter(gene_id %in% genes_file$gene_id)
      return(extras_genes)
    }
  }
}

