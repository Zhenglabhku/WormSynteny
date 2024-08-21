#' Function to get the path to the 'ubuntu' folder within the package
#'
#' @return The ubuntu folder path
#'
#' @examples
#' #...
get_ubuntu_path <- function() {
  path <- system.file("ubuntu", package = "WormSyntenyPackage")
  path <- paste0(path, "/")

  if (path == "") {
    stop("The 'ubuntu' folder could not be found in the installed package.")
  }

  return(path)
}
