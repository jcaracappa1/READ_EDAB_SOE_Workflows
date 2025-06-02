#' Checks if number is even
#'
#' descriptions
#'
#' @param x numeric. Number to check
#'
#' @return logical
#' 
#' @export

is_even <- function(x){
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) != 1) {
    stop("Input must be a single number")
  }
  
  if (x %% 2 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}