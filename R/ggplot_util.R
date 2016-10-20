#' geom_txt
#'
#' Helper for \code{geom_text} with some defaults.
#'
#' @param family Font family.  Defaults to theme-defined family.
#' @param size Font size. Defaults to 3.
#' @param colour Font colour. Defaults to \code{#2b2b2b}
#' @param ... Passed to \code{geom_text}.
#'
#' @importFrom ggplot2 theme_get
#' @export
geom_txt <- function(...,
                     family = theme_get()$text$family,
                     size = 3,
                     colour = "#2b2b2b"){
  geom_text(..., family = family, size = size, colour = colour)
}

#' geom_lbl
#'
#' Helper for \code{geom_label} with some defaults.
#'
#' @param family Font family. Defaults to theme-defined family.
#' @param size Font size. Defaults to 3.
#' @param label.size Label size. Defaults to 0.
#' @param ... Additional parameters passed to \code{geom_label}
#'
#' @importFrom ggplot2 geom_label
#' @export
geom_lbl <- function(...,
                     family = theme_get()$text$family,
                     size = 3,
                     label.size = 0){
  geom_label(..., family = family, size = size, label.size = label.size)
}
