#' bit_count
#'
#' Count the number of set bits.
#'
#' @export
#' @importFrom purrr detect
bit_count <- function(x){
  v <- x
  count <- as.integer(v > 0)
  while(sum(v) > 0){
    v <- bitwAnd(v, v-1)
    count <- count + (v > 0)
  }
  count
}

#' bit_right
#'
#' Get the index of the rightmost bit (starting at 1). Returns 0 if no bits set.
#'
#' @export
#' @importFrom magrittr %>%
bit_right <- function(x){
  mask <- bitwAnd(x, bitwNot(x-1))
  log2(mask) %>% {ifelse(. < 0, 0, . + 1)}
}


#' bit_to_set
#'
#' Convert bitmask vector to a list of sets.
#'
#' @export
#' @importFrom purrr map2 discard
bit_to_set <- function(x){
  .result <- vector(mode="list", length=length(x))
  while(any(x > 0)){
    .right <- bitwAnd(x, -x)
    .right_list <- as.list(.right) %>% map(~discard(., ~. < 1))
    .result <- map2(.result, .right_list, c)
    x <- x-.right
  }
  .result %>% map(~ log2(.) + 1)
}
