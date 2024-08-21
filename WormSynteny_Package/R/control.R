#' control function
#'
#' Remove same genes in extras fragments files
#'
#' Belong to the pipeline function in step 10 : remove same genes from extras fragments and place them into top fragments
#'
#' @param genes_file An aligned genes data frame
#' @param extras_genes An extras genes data frame
#'
#' @return An extras genes data frame
#'
#' @importFrom  dplyr anti_join
#'
#' @examples
#' #...
control <- function(genes_file, extras_genes){
  if(is.null(extras_genes)){
    return(extras_genes)
  }else{
    if(nrow(extras_genes)== 0){
      return(extras_genes)
    }
    else{
      extras_genes <- anti_join(extras_genes, genes_file, by = "gene_id")
      return(extras_genes)
    }
  }
}
