#' pslToDataFrame2 function
#'
#' From PSL file format to data frame format case 2 : species as queries
#'
#' Belong to the pipeline function in step 2 : find pairwise alignment between phylogenetically adjacent species using the homologous fragments
#'
#' @param elegans_species A PSL format file
#' @param species_name A string that represents a species name
#'
#' @return A data frame of aligned fragments
#'
#' @examples
#' #...
pslToDataFrame2 <- function(list_df, species_names){

  species1_species2 <- list_df[[species_names]]
  if(length(species1_species2)==0){
    species1_species2 <- NULL
    return(species1_species2)
  }else{
    #Make it clean
    species <- strsplit(species_names, "_")
    species1_species2 <- list_df[[species_names]]
    colnames(species1_species2) <- c("gene_id","matches", "misMatches" ,"repMatches","nCount","qNumInsert","qBaseInsert","tNumInsert","tBaseInsert" ,"strand","scaff","qSize","start","end","seq_id2","tSize","start2","end2","blockCount","blockSizes","qStarts","tStarts")

    #Create seq_id and seq_id2 to match gggenomes requirements
    species1_species2$seq_id <- paste(species[[1]][1],species1_species2$scaff, sep = "_")
    species1_species2$seq_id2 <- paste( species[[1]][2],species1_species2$seq_id2, sep = "_")

    return(species1_species2)
  }
}
