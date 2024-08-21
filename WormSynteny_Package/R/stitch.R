#' stitch function
#'
#' Stitch adjacent fragments together below a certain gap
#'
#' Belong to the pipeline function in step 3 : assemble adjacent fragments according to a gap value
#'
#' @param raw_coord An aligned raw fragments data frame
#' @param gap_max A gap value as integer
#'
#' @return An aligned assembled fragments data frame
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#'
#' @examples
#' #...
stitch <- function(raw_coord, gap_max){
  if(is.null(raw_coord)){
    return(raw_coord)

  }else{
    l_1 <- list()
    df_distinct <- raw_coord %>% distinct(seqnames)

    for(k in 1 : nrow(df_distinct)){
      df <- raw_coord %>% filter(seqnames %in% df_distinct$seqnames[k]) %>% arrange(seqnames, start)
      stich_df <- df[1,]
      i <- 1
      n <- 1
      gap <- 0
      gap_cumul <- 0
      max_end <- 0
      l_2 <- list()
      while(n < nrow(df)){
        #Initialize at line 1 (n==1)
        gap <- 0
        max_end <- df$end[n]
        stich_df$end <- df$end[n]
        stich_df$start <- df$start[n]
        length <- df$length[n]
        stich_df$length <- length
        stich_df$gap <- gap

        #Calculate the gap
        gap <- df$start[n+1] - df$end[n]
        gap_cumul <- gap

        #Stich
        #if start of other line is included between start and end, end added with gap_max value
        while( df$start[n+1]>=  df$start[n] &  df$start[n+1]<=  (max_end + gap_max)){
          n <- n + 1
          if(df$end[n] >= max_end){
            stich_df$end <- df$end[n]
            max_end <- df$end[n]
          }else{
            stich_df$end <- max_end
          }
          length <- df$length[n]  + length
          stich_df$length <- length
          stich_df$gap <- gap_cumul

          if(n == nrow(df)){

            break

          }else{
            gap <- df$start[n+1] - df$end[n]
            gap_cumul <- gap + gap_cumul

          }
        }
        l_2[[i]] <- stich_df
        i <- i + 1
        n <- n + 1
      }
      if(n == nrow(df)){
        gap <- 0
        stich_df$end <- df$end[n]
        stich_df$start <- df$start[n]
        length <- df$length[n]
        stich_df$length <- length
        stich_df$gap <- gap
        l_2[[i]] <- stich_df
      }
      df_stich <- as.data.frame(do.call(rbind, l_2))
      l_1[[k]] <- df_stich
    }
    df <- as.data.frame(do.call(rbind,l_1))
    return(df)
  }
}
