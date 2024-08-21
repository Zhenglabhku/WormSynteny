#' subset_by_intersect function
#'
#' Intersect the aligned fragments data frame
#'
#' Belong to the genes_fct function in the pipeline function in step 6 : find genes/piece of genes in the aligned regions
#'
#' @param species A species specific data frame
#' @param aligned_coord_species An aligned fragments data frame
#'
#' @return A data frame of intersected genes
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' #...
subset_by_intersect <- function(species, aligned_coord_species){

  #Use Granges package : create Granges object first
  aligned_coord_species_gr <- GenomicRanges::makeGRangesFromDataFrame(aligned_coord_species, keep.extra.columns = TRUE)
  species_gr <-  GenomicRanges::makeGRangesFromDataFrame(species, keep.extra.columns = TRUE)

  #Apply intersect on Granges objects
  overlapping_genes <- GenomicRanges::intersect(species_gr, aligned_coord_species_gr, ignore.strand = TRUE)

  #Convert grange objects to data frames
  overlapping_genes <- overlapping_genes %>% as.data.frame()

  return(overlapping_genes)
}
