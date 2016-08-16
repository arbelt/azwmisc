#' NYT ggplot theme
#'
#' Complete theme based on \code{theme_minimal} that applies NYT-style. Font to
#' be used should be specified as \code{base_family}.
#'
#' Original post at \url{http://rud.is/b/2016/03/16/supreme-annotations/}
#' @import ggplot2
#' @inheritParams theme_nyt_
#' @param ... Parameters passed to \code{theme_minimal}
#' @export
theme_nyt <- function(..., flip = FALSE){
  my_theme <- theme_minimal(...)
  my_theme + theme_nyt_(flip = flip)
}

#' Partial NYT theme
#'
#' Partial theme that applies NYT-style modifications.
#' @export
#' @param flip Whether to flip the axes
#' @param ... Unused.
theme_nyt_ <- function(..., flip = FALSE){
  dotted_line <- element_line(color = "#2b2b2b", linetype = "dotted", size = 0.15)
  solid_line <- element_line(color = "#2b2b2b", size = 0.15)
  mytheme_elems <- list(
    legend.position = "right",
    legend.justification = c(0,1),
    legend.text = element_text(size = 8, face = "bold"),
    legend.key.size = unit(3, "mm"),
    plot.margin = unit(rep(0.5,4), "cm"),
    panel.grid = element_line(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = if (flip) dotted_line else element_blank(),
    panel.grid.major.y = if (flip) element_blank() else dotted_line,
    axis.text.y = element_text(margin = margin(r = 0)),
    axis.ticks.x = if (!flip) solid_line else element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length = unit(5, "pt"),
    axis.line = element_line(size = 0.15),
    axis.line.x = if (flip) element_blank() else solid_line,
    axis.line.y = if (!flip) element_blank() else solid_line,
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 15))
    )
  valid_elems <- ggplot2:::.element_tree %>% names %>%
    intersect(names(mytheme_elems))
  invoke(ggplot2::theme, mytheme_elems[valid_elems])
}
