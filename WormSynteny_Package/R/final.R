#' final function
#'
#' Keep only the longest assembled fragment
#'
#' Belong to the pipeline function in step 5 : separate the longest homologous fragments from the others
#'
#' @param filtered_data A data frame of the assembled fragments
#'
#' @return A data frame of assembled fragments
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr slice
#'
#' @examples
#' #...
final <- function(filtered_data){
  if(is.null(filtered_data)){
    return(filtered_data)
  }
  else if(nrow(filtered_data)==0){
    return(filtered_data)
  }else{
    final_data <- filtered_data %>% filter(length == max(filtered_data$length))
    if(nrow(final_data) > 1){
      final_data <- final_data %>% arrange(seqnames) %>% slice(1)
      return(final_data)
    }else{
      return(final_data)
    }
  }
}
