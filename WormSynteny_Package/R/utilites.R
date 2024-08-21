#' Functions to get the make sure all linux utilities could be use.
#'
#' @return The gtf2bed command path
#'
#' @examples
#' #...
get_gtf2bed_path <- function() {
  path <- Sys.which("gtf2bed")
  if (path == "") {
    stop("The 'gtf2bed' folder could not be found in the installed package.")
}

return(path)
}

get_halLiftover_path <- function() {
  path <- Sys.which("halLiftover")
  if (path == "") {
    stop("Could not find halLiftver. To install, look here -> https://github.com/ComparativeGenomicsToolkit/hal.git")
  }

  return(path)
}
