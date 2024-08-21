#' filtering function
#'
#' Filter assembled fragments according to a filtering value
#'
#' Belong to the pipeline function in step 4 : filter assembled homologous fragments through a filtering percentage of the sequence of inquiry length
#'
#' @param stitch_data A data frame of assembled fragments
#' @param region The region inquiry data frame
#'
#' @return A data frame of filtered assembled fragments
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr across
#' @examples
#' #...
filtering <- function(stitch_data, region){

  if(is.null(stitch_data)){
    return(stitch_data)
  }else{
    if(nrow(stitch_data)> 0){
      filtered_df <- stitch_data %>% mutate(length = rowSums(across(c(length, gap)))) %>% filter(length > region$filtering_percentage/100*region$length) %>% select(-gap)
      return(filtered_df)
    }else{
      stitch_data <- stitch_data %>% select(-gap)
      return(stitch_data)
    }
  }
}
