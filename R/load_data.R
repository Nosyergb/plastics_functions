#' Load Break Free From Plastic Data
#'
#' @return a data frame
#'
#' @importFrom tidytuesdayR tt_load
#' @export

load_data <- function() {
  tuesdata <- tt_load('2021-01-26')
  plastics <- tuesdata$plastics

  return(plastics)
}



