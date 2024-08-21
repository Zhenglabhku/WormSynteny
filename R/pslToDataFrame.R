#' pslToDataFrame function
#'
#' From PSL file format to data frame format case 1 : C. Elegans as query
#'
#' Belong to the pipeline function in step 1 : retrieve the homologous fragments of the C. elegans sequence of inquiry
#'
#' @param elegans_species A PSL format file
#' @param species_name A string that represents a species name
#'
#' @return A data frame of aligned fragments
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyr separate
#' @importFrom dplyr rename
#'
#' @examples
#' #...
pslToDataFrame <- function(elegans_species, species_name){

  if(is.null(elegans_species)){
    return(elegans_species)
  }else{
    #Select the data frame
    colnames(elegans_species) <- c("gene_id", "matches", "misMatches" ,"repMatches","nCount","qNumInsert","qBaseInsert","tNumInsert","tBaseInsert" ,"strand","scaff","qSize","start","end","seq_id2","tSize","start2","end2","blockCount","blockSizes","qStarts","tStarts")

    #Create seq_id and seq_id2
    elegans_species$seq_id <- paste("elegans",elegans_species$scaff, sep = "_")
    elegans_species <- elegans_species %>% rename(seqnames = "seq_id2")
    elegans_species$seq_id2 <- paste(species_name,elegans_species$seqnames, sep = "_")

    #Create strand and strand2 (separate in two columns the column containing both strands)
    elegans_species <- elegans_species %>% mutate(new_strand = gsub("([[:punct:]])([[:punct:]])", "\\1 \\2", elegans_species$strand)) %>% select(-strand) %>% separate(new_strand, into = c("strand", "strand2"), sep = " ")

    return(elegans_species)
  }
}
