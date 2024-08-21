#' messages function
#'
#' Create messages in functions of missing fragments and genes
#'
#' Belong to the render_explanations function in the server function in every section
#'
#' @param regions A vector of species that do not have fragments
#' @param genes A vector of species that do not have genes
#'
#' @return A string : the messages shown in explanation panel
#'
#' @examples
#' #...
messages <- function(regions, genes){

  if(length(regions)!=0){
    regions <- lapply(regions, function(x) paste0("C. ", x))
    s1 <- "List of species that do not have aligned regions for the selected C. Elegans region : \n"
    s2 <- paste(regions, collapse = ", ")
    s3 <- paste(s1,s2, "\n")

  }else{
    s3 <- "Every species have aligned regions for the selected C. Elegans region.\n"
  }

  if(length(genes)!=0){
    genes <- lapply(genes, function(x) paste0("C. ", x))
    s4 <- "List of species that do not have genes in the aligned regions for the selected C. Elegans region : \n"
    s5 <- paste(genes,collapse = ", ")
    s6 <- paste0(s4, s5, "\n")

  }else{
    s6 <- "Every species that have aligned regions have genes for the selected C. Elegans region.\n"
  }

  s7 <- paste0(s3, "\n", s6)
  return(s7)
}
