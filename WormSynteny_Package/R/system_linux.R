#' system_linux function
#'
#' Retrieve the homologous fragments of the C. Elegans sequence of inquiry with the halLiftover linux command
#'
#' Belong to the pipeline function in step 1 : retrieve the homologous fragments of the C. Elegans sequence of inquiry
#'
#' @param species A vector of string that represent species names
#'
#' @return A list of data frames under the PSL format
#'
#' @importFrom utils read.table
#'
#' @examples
#' #...
#'
system_linux <- function(species) {

  #Get the paths
  folder_ubuntu <- get_ubuntu_path()
  path_gtf2bed <- get_gtf2bed_path()
  path_halLiftover <- get_halLiftover_path()

  #Generate the region gtf file
  list_df <- list()
  system(paste0("rm ", folder_ubuntu, "region_*"))

  #Convert singletons_elegans GTF file to BED files
  system(paste0(path_gtf2bed, " < ", folder_ubuntu, "region.gtf > ", folder_ubuntu, "region.bed"))

  #Generate the psl files
  for(i in species){
    tryCatch({
      system(paste0(path_halLiftover," ", folder_ubuntu, "evolver_Caenorhabditis.hal --outPSLWithName caenorhabditis_elegans ", folder_ubuntu, "region.bed caenorhabditis_",i, " ",folder_ubuntu,"region_", i, ".psl --bedType 4"))
      list_df[[i]] <- read.table(paste0(folder_ubuntu, "region_", i,".psl"))
    }, error=function(e){print(i)})
  }
  return(list_df)
}
