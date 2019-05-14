#' Nicify names
#'
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom magrittr %>%
#' @param df Dataframe
#' @param style Naming style convention.  Currently only 'snake_case' is supported.
#' @param abbrev Logical. Whether to use common abbreviations such as 'pct' for '\%'
#' @export
nicify_names <- function(df, style = c("snake_case"), abbrev = FALSE){
  newnames <- names(df)
  if (abbrev){
    newnames <- newnames %>%
      stri_replace_all_fixed("%", " pct ")
  }
  newnames <- newnames %>%
    ## do_if(lowercase, tolower) %>%
    sep_words_all %>%
    words_to_snake %>%
    make.names(unique = TRUE, allow_ = TRUE)
  names(df) <- newnames
  df
}

#' @importFrom rlang exec
do_if <- function(.x, .p, .f){
  my_f <- if (isTRUE(.p)) .f else identity
  exec(my_f, .x)
}

#' @importFrom stringi stri_replace_all_regex stri_split_regex
#' @importFrom purrr map flatten_chr
sep_words_camelcase <- function(x){
  do_split <- function(.x){
    stri_split_regex(.x, "(?<=[A-Za-z])(?=[A-Z][a-z])")
  }
  if (is.list(x)) {
    return(map(x, ~ flatten_chr(do_split(.x))))
  }
  return(do_split(x))
}

sep_words_nonalphanum <- function(x){
  do_split <- function(.x){
    stri_split_regex(.x, "[^A-Za-z0-9]+")
  }
  if (is.list(x)) {
    return(map(x, ~ flatten_chr(do_split(.x))))
  }
  return(do_split(x))
}

sep_words_all <- function(x){
  x %>% sep_words_nonalphanum %>%
    sep_words_camelcase %>%
    map(~ discard(.x, ~ nchar(.x) < 1))
}

#' @importFrom stringi stri_c
words_to_snake <- function(x){
  x %>% map(tolower) %>%
    map(~ stri_c(.x, collapse="_"))
}
