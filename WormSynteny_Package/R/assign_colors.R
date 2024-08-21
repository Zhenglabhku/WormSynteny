#' assign_colors function
#'
#' Assign colors to orthogroups in a vector while plotting
#'
#' Belong to the server function in every section
#'
#' @param genes A data frame of genes names
#' @param prev_colors A vector of colors
#'
#' @return A list containing a data frame and a map
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @examples
#' #...
assign_colors <- function(genes, prev_colors){

  color_palette <- c("#92C5DEFF", "#EEB479FF", "#AFE1AF","#E88471FF", "#E9E29CFF", "#CF597EFF", "#E78AC3FF", "#FFD92FFF", "#009392FF", "#FF9E9D", "#FF3D7FFF",  "#EBA07EFF", "#A8554EFF", "#4393C3FF", "#39B185FF", "#9CCB86FF" ,"#A6D854FF","#803233FF","#ED3F39FF","#E46844FF", "#DC876CFF","#FFCC65FF", "#FF9932FF")


  ##Get unique orthogroups in the vector
  unique_ortho <- unique(genes$Orthogroup)

  #Take the commom colors and orthogroups
  common_ortho <- intersect(unique_ortho, names(prev_colors))
  ortho_color_map <- prev_colors[common_ortho]

  if(length(setdiff(unique_ortho, names(prev_colors))) == 0){

    #Create a column colors
    colored_vector <- ortho_color_map[genes$Orthogroup]

    #Create a data frame with the original vector and the colored vector as columns
    genes <- genes %>% mutate(color = colored_vector)

  }else{

    #Take the new OGs
    diff_ortho <- setdiff(unique_ortho, names(prev_colors))

    #Get the new numbers of colors
    new_colors <- color_palette[1:length(unique_ortho)]

    #Grab the new colors by comparing them to the common colors
    new_colors <- setdiff(new_colors, ortho_color_map)

    #Assign the new colors to the new OGs
    new_ortho_color_map <- setNames(ifelse(is.na(diff_ortho), "#808080", new_colors), diff_ortho)

    #Add the new set of ortho and colors to the previous one
    ortho_color_map <- c(prev_colors, new_ortho_color_map)

    #Create a column of colors
    colored_vector <- ortho_color_map[genes$Orthogroup]

    #Create a data frame with the original vector and the colored vector as columns
    genes <- genes %>% mutate(color = colored_vector)
  }
  return(list(genes = genes, colors = ortho_color_map))
}
