#' has_links function
#'
#' Test if there is links
#'
#' Belong to the ggg_plot function in the server function in every section
#'
#' @param seqs An aligned sequences data frame used in gggenomes
#' @param links A links data frame
#'
#' @return A boolean : TRUE if there is links and FALSE if there is not
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#'
#' @examples
#' #...
has_links <- function(seqs, links){

  ## Rename the seq_id by seqnames
  seqs_new <- seqs %>% select(-seqnames) %>% rename(seqnames = "seq_id")

  ## Filter df with sequences present in seqs
  df <- links %>% filter(seq_id %in% seqs_new$seqnames)
  df <- df %>% filter(seq_id2 %in% seqs_new$seqnames)

  ## Filter overlapping coordinates
  df <- df %>% rename(seqnames = "seq_id", seqnames2 = "seq_id2")
  df_overlap <- subset_by_overlap(df, seqs_new)
  df_overlap <- df_overlap %>% rename(start3 = "start", start = "start2", end3 = "end", end= "end2", seqnames3 = "seqnames", seqnames = "seqnames2")
  df_overlap <- subset_by_overlap(df_overlap, seqs_new)

  if(nrow(df_overlap) > 0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
