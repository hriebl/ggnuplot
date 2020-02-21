#' @export

gnucolors <- c(
  "darkviolet",
  "#009e73",
  "#56b4e9",
  "#e69f00",
  "#f0e442",
  "#0072b2",
  "#e51e10",
  "black"
)

#' @export

gnupalette <- function(n) {
  gnucolors[(seq.int(0, n - 1) %% length(gnucolors)) + 1]
}

#' @export

gnubreaks <- function(nbreaks = 5, padding = 0.1) {
  function(limits) {
    width <- limits[2] - limits[1]
    min <- limits[1] + padding * width
    max <- limits[2] - padding * width
    seq.int(min, max, length.out = nbreaks)
  }
}

#' @export

gnulabels <- function() {
  function(breaks) {
    nbreaks <- length(breaks)
    min <- breaks[1]
    max <- breaks[nbreaks]
    between <- (max - min) / (nbreaks - 1)
    digits <- -floor(log10(between))
    try <- round(breaks, digits)
    if (max(abs((diff(try) - between) / between)) <= 0.05) {
      breaks <- try
    } else {
      breaks <- round(breaks, digits + 1)
    }
    format(breaks)
  }
}


#' gnuplot's default color palette for discrete data, ported to ggplot2
#'
#' These functions provide gnuplot's default color palette. Use the function
#' `scale_color_gnuplot()` with ggplot2, and the function `gnupalette()` or the
#' vector `gnucolors` otherwise.
#'
#' Attention: Do not use this palette with more than eight categories, as the
#' colors are recycled on the ninth category!
#'
#' @usage
#' scale_color_gnuplot(..., na.value = "grey50", aesthetics = "color")
#' gnupalette(n)
#'
#' @inheritDotParams ggplot2::discrete_scale
#' @inheritParams ggplot2::scale_colour_hue
#' @param n The number of colors to return from the palette
#'
#' @seealso
#' [ggplot2::scale_color_hue()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_gnuplot() +
#'   scale_x_gnuplot() +
#'   scale_y_gnuplot() +
#'   theme_gnuplot()
#'
#' gnupalette(3)
#' gnucolors[1:3]
#' @aliases gnucolors gnupalette
#' @importFrom ggplot2 discrete_scale
#' @export

scale_color_gnuplot <- function(...,
                                na.value = "grey50",
                                aesthetics = "color") {
  discrete_scale(aesthetics, "gnuplot", gnupalette, na.value = na.value, ...)
}


#' gnuplot axes for ggplot2
#'
#' These functions try to choose pretty axis breaks/ticks and labels.
#' They also set up secondary axes.
#'
#' @usage
#' scale_x_gnuplot(nbreaks = 5, padding = 0.1, breaks, labels, sec.axis, ...)
#' scale_y_gnuplot(nbreaks = 5, padding = 0.1, breaks, labels, sec.axis, ...)
#' gnubreaks(nbreaks = 5, padding = 0.1)
#' gnulabels()
#'
#' @param nbreaks The number of breaks/ticks on the axis
#' @param padding The amount of space between the outmost ticks and the
#'                plot borders relative to the plot width. A number between
#'                0 and 0.5.
#' @inheritParams ggplot2::scale_x_continuous
#' @inheritDotParams ggplot2::scale_x_continuous
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_gnuplot() +
#'   scale_x_gnuplot() +
#'   scale_y_gnuplot() +
#'   theme_gnuplot()
#'
#' # Alternatively, without the secondary axis:
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_gnuplot() +
#'   scale_x_gnuplot(sec.axis = waiver()) +
#'   scale_y_gnuplot(sec.axis = waiver()) +
#'   theme_gnuplot()
#'
#' breaks <- gnubreaks()(limits = c(0, 1000))
#' gnulabels()(breaks)
#' @aliases gnubreaks gnulabels scale_y_gnuplot
#' @importFrom ggplot2 dup_axis scale_x_continuous
#' @export

scale_x_gnuplot <- function(nbreaks = 5, padding = 0.1, breaks, labels,
                            sec.axis, ...) {
  if (missing(breaks)) breaks <- gnubreaks(nbreaks, padding)
  if (missing(labels)) labels <- gnulabels()
  if (missing(sec.axis)) sec.axis <- dup_axis(labels = NULL, name = "")

  scale_x_continuous(
    breaks = breaks,
    labels = labels,
    sec.axis = sec.axis,
    ...
  )
}

#' @importFrom ggplot2 dup_axis scale_y_continuous
#' @export

scale_y_gnuplot <- function(nbreaks = 5, padding = 0.1, breaks, labels,
                            sec.axis, ...) {
  if (missing(breaks)) breaks <- gnubreaks(nbreaks, padding)
  if (missing(labels)) labels <- gnulabels()
  if (missing(sec.axis)) sec.axis <- dup_axis(labels = NULL, name = "")

  scale_y_continuous(
    breaks = breaks,
    labels = labels,
    sec.axis = sec.axis,
    ...
  )
}


#' gnuplot theme for ggplot2
#'
#' This theme makes ggplot2 look like gnuplot.
#' It is based on [ggplot2::theme_linedraw()] and has inward ticks.
#'
#' @inheritParams ggplot2::theme_linedraw
#'
#' @seealso
#' [ggplot2::theme_linedraw()], [ggplot2::theme()]
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_gnuplot() +
#'   scale_x_gnuplot() +
#'   scale_y_gnuplot() +
#'   theme_gnuplot()
#' @importFrom ggplot2 element_blank element_rect element_text margin theme
#'                     theme_linedraw unit %+replace%
#' @export

theme_gnuplot <- function(base_size = 11,
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  large <- 1.5 * base_size
  small <- 0.75 * base_size
  tiny <- 0.375 * base_size

  theme_linedraw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace% theme(
    axis.text = element_text(),
    axis.text.x = element_text(),
    axis.text.x.bottom = element_text(margin = margin(t = large)),
    axis.text.x.top = element_text(margin = margin(b = large)),
    axis.text.y = element_text(),
    axis.text.y.left = element_text(margin = margin(r = large)),
    axis.text.y.right = element_text(margin = margin(l = large)),
    axis.title.x = element_text(),
    axis.title.x.bottom = element_text(margin = margin(t = small)),
    axis.title.x.top = element_text(margin = margin(b = small)),
    axis.title.y = element_text(),
    axis.title.y.left = element_text(angle = 90, margin = margin(r = small)),
    axis.title.y.right = element_text(angle = -90, margin = margin(l = small)),
    axis.ticks.length = unit(-small, "pt"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(margin = margin(tiny, tiny, tiny, tiny))
  )
}
