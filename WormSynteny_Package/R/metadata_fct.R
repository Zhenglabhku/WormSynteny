#' metadata_fct function
#'
#' Add metadata to the aligned genes data frames
#'
#' Belong to the pipeline function in step 7 : add metadata to genes/piece of genes in the aligned regions and step 9 : find genes/piece of genes in the removed regions too
#'
#' @param species A specific species data frame
#' @param genes_species An aligned genes data frame
#'
#' @return An aligned genes data frame that contains metadata
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr slice
#'
#' @examples
#' #...
metadata_fct <- function(species, genes_species){

  if(is.null(genes_species)){
    return(genes_species)
  }else{
    if(nrow(genes_species) != 0){
      l <- list()
      for(i in 1:nrow(genes_species)){
        genes <- genes_species[i,]
        metadata <- subset_by_overlap(species, genes)
        if(nrow(metadata) > 1){
          metadata_gene_type <- metadata %>% filter(width == max(width)) %>% slice(1)
          metadata_gene_type <- metadata_gene_type %>% select(c("strand","type","gene_id","gene_biotype","Orthogroup"))
          genes <- cbind(genes, metadata_gene_type)
        }else{
          metadata <- metadata %>% select(c("strand","type","gene_id","gene_biotype","Orthogroup"))
          genes <- cbind(genes, metadata)
        }
        l[[i]] <- genes
      }
      genes_species <-  data.frame(do.call(rbind, l))
      return(genes_species)

    }else{
      metadata <-  species %>% select(c("strand","type","gene_id","gene_biotype","Orthogroup"))
      metadata <- metadata[0, ]
      genes_species <- cbind(genes_species, metadata)

      return(genes_species)
    }
  }
}
