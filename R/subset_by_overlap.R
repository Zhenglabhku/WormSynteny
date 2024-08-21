#' subset_by_overlap function
#'
#' Overlap the aligned fragments data frame
#'
#' Belong to the check_overlapping function in the pipeline function in step 8 : check if there is some "hidden genes" inside the intersected genes found
#'
#' @param species A species specific data frame
#' @param aligned_coord_species An aligned fragments data frame
#'
#' @return A data frame of overlapped genes
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' #...
subset_by_overlap <- function(species, aligned_coord_species){

  #Use Granges package : create Granges object first
  aligned_coord_species_gr <- GenomicRanges::makeGRangesFromDataFrame(aligned_coord_species, keep.extra.columns = TRUE)
  species_gr <-  GenomicRanges::makeGRangesFromDataFrame(species, keep.extra.columns = TRUE)

  #Apply sybsetOverlaps on Granges objects
  overlap_species <- IRanges::subsetByOverlaps(species_gr, aligned_coord_species_gr, type = "any", ignore.strand = TRUE)

  #Convert grange objects to data frames
  overlap_species <- overlap_species %>% as.data.frame()

  return(overlap_species)
}
