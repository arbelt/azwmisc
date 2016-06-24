#' Nicify names
#'
#' @importFrom stringr str_replace_all
#' @importFrom dplyr %>%
#' @export
nicify_names <- function(df, lowercase = TRUE, abbrev = FALSE){
  names(df) <- names(df) %>%
    do_if(lowercase, tolower) %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    make.names(unique = TRUE, allow_ = TRUE)
  df
}

#' @importFrom purrr invoke
do_if <- function(.x, .p, .f){
  my_f <- if (isTRUE(.p)) .f else identity
  invoke(my_f, list(.x))
}
