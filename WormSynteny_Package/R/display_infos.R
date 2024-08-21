#' display_infos function
#'
#' Display information nicely in the UI table panels
#'
#' Belong to the server function in every sections
#'
#' @param genes The aligned genes data frame
#' @param aligned_sequences The aligned sequences data frame
#' @param links The links data frame
#' @param region The region data frame
#'
#' @return A list containing the genes, the aligned sequences, the links and the region data frames
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr ungroup
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#'
#' @examples
#' #...
display_infos <- function(genes, aligned_sequences, links, region){

  #If macro_synteny then add a coverage column
  if("gene" %in% genes$types){
    #Create lists
    species <- c("bovis", "becei", "panamensis", "inopinata","elegans","tropicalis", "remanei", "latens", "tribulationis", "briggsae", "nigoni")
    l <- list()

    for(i in 1:length(species)){
      df <- species_macro[[i]]
      g <- genes %>% filter(str_extract(seq_id, "[:alpha:]+") %in% species[i])
      g1 <- left_join(g, df, by = "gene_id")
      g2 <- g1 %>% mutate(coverage = ifelse(length == width, "FULL", "PARTIAL")) %>% mutate(coverage = ifelse(coverage == "FULL", "FULL", paste0("PARTIAL ","(", round(length/width * 100), "%)")))
      g <- g %>% mutate(coverage = g2$coverage)
      l[[i]] <- g
    }
    genes <- as.data.frame(do.call(rbind, l))

    #Add gene_name
    gene_name <- gene_name_fct(genes, region, elegans) %>% select(gene_id, gene_name)
    genes <- left_join(genes, gene_name, by = c("gene_id")) %>% select(seq_id:gene_id,gene_name, gene_biotype:coverage)

    #Add bin_id and delete seq_id
    genes <- genes %>% mutate(seq_id = ifelse(str_detect(seq_id, "(filtered)"), str_c(str_c("C. ", str_extract(seq_id, "[:alpha:]+")), "(filtered)", sep = " "), str_c(str_c("C. ", str_extract(seq_id, "[:alpha:]+")),sep = " "))) %>% rename(bin_id = "seq_id")

    #Change numeric types to integer
    genes$start <- as.integer(genes$start)
    genes$end <- as.integer(genes$end)
    genes$length <- as.integer(genes$length)

    ## Put the column names in capital letter
    colnames(genes) <- c("Bin_id", "Seqname", "Start", "End", "Length", "Strand", "Type","Gene_id", "Gene_name", "Gene_biotype", "Orthogroup", "Coverage")

  }else{
    #Add gene_name
    gene_name <- gene_name_fct(genes, region, elegans) %>% select(gene_id, gene_name)
    genes <- left_join(genes, gene_name, by = c("gene_id")) %>% select(seq_id:gene_id,gene_name, gene_biotype, Orthogroup)

    #Add bin_id and delete seq_id
    genes <- genes %>% mutate(seq_id = ifelse(str_detect(seq_id, "(filtered)"), str_c(str_c("C. ",str_extract(seq_id, "[:alpha:]+")), "(filtered)", sep = " "), str_c(stringr::str_c("C. ", str_extract(seq_id, "[:alpha:]+")),sep = " "))) %>% rename(bin_id = "seq_id")

    #Change numeric types to integer
    genes$start <- as.integer(genes$start)
    genes$end <- as.integer(genes$end)
    genes$length <- as.integer(genes$length)

    ## Put the column names in capital letter
    colnames(genes) <- c("Bin_id", "Seqname", "Start", "End", "Length", "Strand", "Type", "Gene_id", "Gene_name", "Gene_biotype", "Orthogroup")

  }

  #Change numeric types to integer
  aligned_sequences$start <- as.integer(aligned_sequences$start)
  aligned_sequences$end <- as.integer(aligned_sequences$end)
  aligned_sequences$length <- as.integer(aligned_sequences$length)

  ## Put the column names in capital letter
  aligned_sequences <- aligned_sequences %>% ungroup() %>% select(bin_id, seqnames, start, end, length)

  colnames(aligned_sequences) <- c("Bin_id", "Seqname", "Start", "End", "Length")

  if(!is.null(links)){
    ## Links
    links <- links %>% separate(seq_id, into = c("bin_id", "seqnames"), sep = "_", extra = "merge") %>% mutate(bin_id = str_c("C. ",bin_id))
    links <- links %>% separate(seq_id2, into = c("bin_id2", "seqnames2"), sep = "_", extra = "merge") %>% mutate(bin_id2 = str_c("C. ",bin_id2))
    links <- links %>% select(c("bin_id", "seqnames", "start", "end","strand", "bin_id2", "seqnames2", "start2", "end2", "strand2"))
    colnames(links) <- c("Bin_id", "Seqname", "Start", "End","Strand", "Bin_id2", "Seqname2", "Start2", "End2", "Strand2")

  }

  return(list(genes = genes, aligned_sequences = aligned_sequences, links = links))
}
