#' sort_haliftover_data function
#'
#' Formatting the aligned fragments data frame from halLiftover command
#'
#' Belong to the pipeline function in step 1 : retrieve the homologous fragments of the C. Elegans sequence of inquiry
#'
#' @param df A data frame from halLiftover command
#'
#' @return A formatted data frame
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom tidyr separate
#'
#' @examples
#' #...
sort_haliftover_data <- function(df){

  if(!is.null(df)){
    # Remove the last comma in each element of the 'numbers' column
    df$blockSizes <- gsub(",$", "", df$blockSizes)
    df$tStarts <- gsub(",$", "", df$tStarts)

    #Filter one block reads : no need to change anything
    filter_block_one <- df %>% filter(blockCount == 1)
    filter_block_one <- filter_block_one %>% select(c("seqnames", "start2", "end2", "blockSizes")) %>% rename(start = "start2", end = "end2", length = "blockSizes")

    #Filter other blocks (>1)
    filter_blocks <- df %>% filter(blockCount > 1)

    #Filter rows whose strands are positive
    positive <- filter_blocks %>% filter(strand2 == "+")

    if(nrow(positive) != 0){
      positive <- calculate_end(positive)
      positive <- split_start_end(positive)

    }else{
      positive <- positive %>% select(c("seqnames","start2", "end2", "blockSizes")) %>% rename(start = "start2", end = "end2", length = "blockSizes")

    }
    #Filter rows whose strands are positive
    negative <- filter_blocks %>% filter(strand2 == "-")

    if(nrow(negative) != 0){
      negative <- convert_strand(negative)
      negative <- reverse_blockSizes(negative)
      negative <- calculate_start(negative)
      negative <- split_start_end(negative)

    }else{
      negative <- negative %>% select(c("seqnames","start2", "end2", "blockSizes")) %>% rename(start = "start2", end = "end2", length = "blockSizes")
    }

    sorted_data <- rbind(filter_block_one, positive, negative)
    sorted_data$length <- as.numeric(sorted_data$length)
  }else{
    sorted_data <- NULL
  }
  return(sorted_data)
}
