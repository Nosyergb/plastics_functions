#' Load Break Free From Plastic Data
#'
#' @return a data frame
#' @export
load_data <- function() {
  file_path <- system.file(
    "Updated_Plastics.csv",
    package = "projectr"
  )

  read.csv(file_path)
}
