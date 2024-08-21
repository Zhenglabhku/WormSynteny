#' transform_retrieved function
#'
#' Add seq_id in retrieved genes tables
#'
#' Belong to the pipeline function in step 10 : remove same genes from extras fragments and place them into top fragments
#'
#' @param df A data frame of same genes between extras and top fragments
#' @param species A specific species data frame
#'
#' @return A data frame of retrieved genes with seq_id column added
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom stringr str_extract
#'
#' @examples
#' #...
transform_retrieved <- function(df, species) {
  if(!is.null(df)){
    seq <- str_c(species, df$seqnames, sep = "_")
    df <- df %>% mutate(seq_id = seq, bin_id = str_c("C.", str_extract(seq_id, "[:alpha:]+"), sep = " ")) %>% select(c("seqnames", "start", "end", "length", "seq_id", "bin_id"))
  }
  return(df)
}
