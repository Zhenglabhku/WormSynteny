#' create_region function
#'
#' Create the region inquiry : seqnames, start, end, gap, filtering_value, gene_name
#'
#' Belong to the server function in search, zoom in and out button sections
#'
#' @param chr A string as a chromosome name
#' @param start A integer as the start
#' @param end A integer as the end
#' @param gap A integer as the maximum length between two fragments
#' @param filtering_percentage A integer as the filtering percentage
#' @param gname A string as the gene name or NULL
#'
#' @return A single row table
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#'
#' @examples
#'
#' # Coordinates filling case
#' # region <- create_region("X", 4271165, 4274216, NULL, 200, 16)
#' # Gene selection case
#' # region <- create_region("X", 4271165, 4274216, "mec-17", 200, 16)
create_region <- function(chr, start, end, gap, filtering_percentage, gname){

  #Get the path
  folder_ubuntu <- get_ubuntu_path()

  #if gname is empty
  if(!shiny::isTruthy(gname)){
    #Create a data frame with the provided region
    region <- data.frame(seqnames = chr, start = start, end = end, length = (end-start)+1, gap = gap, filtering_percentage = filtering_percentage)

    #Create a GTF file of the selected region
    region_gr <- GenomicRanges::makeGRangesFromDataFrame(region, keep.extra.columns = TRUE)
    rtracklayer::export(region_gr, paste0(folder_ubuntu, "region.gtf"))
  }else{

    region <- elegans %>% filter(gene_name == gname) %>% rename(length = "width") %>% select(seqnames, start, end, length) %>% mutate(gap = gap, filtering_percentage = filtering_percentage)
    #Create a GTF file of the selected region
    region_gr <- GenomicRanges::makeGRangesFromDataFrame(region, keep.extra.columns = TRUE)
    rtracklayer::export(region_gr, paste0(folder_ubuntu, "region.gtf"))

  }
  return(region)
}
