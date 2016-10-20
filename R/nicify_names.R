#' Nicify names
#'
#' @importFrom stringr str_replace_all str_replace
#' @importFrom dplyr %>%
#' @param df Dataframe
#' @param lowercase Whether to lowercase names or not
#' @param abbrev Unused.
#' @export
nicify_names <- function(df, lowercase = TRUE, abbrev = FALSE){
  newnames <- names(df)
  if (abbrev){
    newnames <- newnames %>%
      str_replace("%", " pct ")
  }
  newnames <- newnames %>%
    do_if(lowercase, tolower) %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    make.names(unique = TRUE, allow_ = TRUE)
  names(df) <- newnames
  df
}

#' @importFrom purrr invoke
do_if <- function(.x, .p, .f){
  my_f <- if (isTRUE(.p)) .f else identity
  invoke(my_f, list(.x))
}
