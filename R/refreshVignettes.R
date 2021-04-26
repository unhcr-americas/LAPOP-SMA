#' refreshVignettes
#' @md copy all Vignettes and generate package documentation
#' 
#' @examples
#' \dontrun{
#'  
#' }
#' @export 
refreshVignettes <- function( ) {
  
  pkgdown::build_site()
  
}

