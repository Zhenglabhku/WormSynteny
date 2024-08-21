#' system_linux_2 function
#'
#' Find pairwise alignment between phylogenetically adjacent species using the halLiftover linux command
#'
#' Belong to the pipeline function in step 2 : find pairwise alignment between phylogenetically adjacent species using the homologous fragments
#'
#' @param species A vector of string that represent species names
#'
#' @return A list of data frames under the PSL format
#'
#' @importFrom utils read.table
#'
#' @examples
#' #...
system_linux_2 <- function(species) {

  #Get the paths
  folder_ubuntu <- get_ubuntu_path()
  path_gtf2bed <- get_gtf2bed_path()
  path_halLiftover <- get_halLiftover_path()

  #Generate the necessary files
  list_df <- list()
  for(i in 1 : length(species)){
    tryCatch({
      system(paste0(path_gtf2bed, " < ", folder_ubuntu, "sorted_",species[i],".gtf > ", folder_ubuntu,"sorted_",species[i],".bed"))
    }, error=function(e){print(i)})
  }
  for(i in 2 : length(species)-1){
    tryCatch({
      system(paste0(path_halLiftover," ", folder_ubuntu,"evolver_Caenorhabditis.hal --outPSLWithName caenorhabditis_",species[i]," ", folder_ubuntu,"sorted_",species[i],".bed caenorhabditis_",species[i+1]," ",folder_ubuntu, species[i],"_",species[i+1],".psl --bedType 4"))
    }, error=function(e){print(i)})
    myfile <- paste0(folder_ubuntu,species[i],"_",species[i+1],".psl")
    species1_species2 <- paste0(species[i],"_",species[i+1])
    if (file.exists(myfile)) {
      file_info <- file.info(myfile)
      if(file_info$size != 0){
        list_df[[species1_species2]] <- read.table(paste0(folder_ubuntu,species[i],"_",species[i+1],".psl"))
      }else{
        list_df[[species1_species2]] <- list()
      }
    }else{
      list_df[[species1_species2]] <- list()
    }
  }
  return(list_df)
}
