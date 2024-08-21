#' check_overlapping function
#'
#' Overlap genes
#'
#' Belong to the pipeline function in step 8 : check if there is some hidden genes inside the intersected genes found
#'
#' @param species A species specific data frame
#' @param df An aligned genes data frame
#'
#' @return An updated aligned genes data frame
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' #...
check_overlapping <- function(species, df){
  if(!is.null(df)){
    if(nrow(df)!=0){
      new_genes <- data.frame()
      for(i in 1 : nrow(df)){
        genes <- subset_by_overlap(species, df[i,])
        if(nrow(genes)>1){
          for(j in 1 : nrow(genes)){
            if(genes$start[j] < df$start[i]){
              genes$start[j] <- df$start[i]
            }
            if(genes$end[j] > df$end[i]){
              genes$end[j] <- df$end[i]
            }
          }
        }else{
          genes <- df[i,]
        }
        genes <- genes %>% mutate(length = (end - start)+1) %>% select(seqnames, start, end, length, strand, type, gene_id, gene_biotype, Orthogroup)
        new_genes <- rbind(new_genes, genes)
      }
      new_genes <- new_genes %>% mutate(length = (end - start)+1) %>% select(seqnames, start, end, length, strand, type, gene_id, gene_biotype, Orthogroup)
      df <- new_genes
    }
  }
  return(df)
}
