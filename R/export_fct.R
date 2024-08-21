#' export_fct function
#'
#' Export the sorted tables from halLiftover linux command in the ubuntu folder
#'
#' Belong to the pipeline function in step 2 : find pairwise alignment between phylogenetically adjacent species using the homologous fragments
#'
#' @param sorted_data A data frame issued from HalLiftover linux command
#' @param species_name A species string name
#'
#' @return Exported tables into the ubuntu folder
#'
#' @examples
#' #...
#'
export_fct <- function(sorted_data, species_name){

  #Get the path
  folder_ubuntu <- get_ubuntu_path()

  if(!(is.null(sorted_data))){
    if(nrow(sorted_data) != 0){
      sorted_data_gr <- GenomicRanges::makeGRangesFromDataFrame(sorted_data, keep.extra.columns = TRUE)
      folder <- paste0(folder_ubuntu, "sorted_",species_name ,".gtf")
      rtracklayer::export(sorted_data_gr, folder)
    }
  }
}
