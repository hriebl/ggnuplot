#' The gnuplot color palette for discrete data, ported to ggplot2
#'
#' These functions provide gnuplot's default color palette.
#' Use `scale_color_gnuplot()` and `scale_fill_gnuplot()` with ggplot2,
#' and `gnupalette()` or the vector `gnucolors` otherwise.
#'
#' @usage
#' scale_color_gnuplot(..., na.value = "gray50", aesthetics = "color")
#' scale_fill_gnuplot(..., na.value = "gray50", aesthetics = "fill")
#' gnupalette(n)
#'
#' @inheritParams ggplot2::scale_color_discrete
#' @param n The number of colors to return
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
#'
#' @aliases scale_colour_gnuplot scale_fill_gnuplot gnupalette gnucolors
#' @importFrom ggplot2 discrete_scale
#' @export

scale_color_gnuplot <- function(..., na.value = "gray50",
                                aesthetics = "color") {
  discrete_scale(aesthetics, "gnuplot", gnupalette, na.value = na.value, ...)
}

#' @export

scale_colour_gnuplot <- scale_color_gnuplot

#' @export

scale_fill_gnuplot <- function(..., na.value = "gray50",
                               aesthetics = "fill") {
  discrete_scale(aesthetics, "gnuplot", gnupalette, na.value = na.value, ...)
}

#' @export

gnupalette <- function(n) {
  if (n > length(gnucolors)) {
    stop("The gnuplot color palette has only ", length(gnucolors), " colors")
  }

  gnucolors[1:n]
}

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


#' gnuplot-like (continuous) axes for ggplot2
#'
#' These functions set up gnuplot-like secondary axes. They also try to choose
#' pretty breaks/ticks for continuous data. Your mileage with the breaks/ticks
#' may vary, so be sure to try different settings.
#'
#' @usage
#' scale_x_gnuplot(breaks = gnubreaks(), sec.axis = gnuaxis(), ...)
#' scale_y_gnuplot(breaks = gnubreaks(), sec.axis = gnuaxis(), ...)
#' gnubreaks(n = 5, padding = 0.1)
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @param n The number of breaks/ticks to return
#' @param padding The amount of space between the outermost breaks/ticks and
#'                the axis limits relative to the axis range. A number between
#'                0 and 0.5.
#'
#' @seealso
#' The [labeling package][labeling::labeling-package] for alternative
#' break/tick functions, and [`ggplot2::dup_axis()`][ggplot2::sec_axis],
#' for which `gnuaxis()` is an alias
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
#' @aliases scale_y_gnuplot gnubreaks gnuaxis
#' @importFrom ggplot2 scale_x_continuous
#' @export

scale_x_gnuplot <- function(breaks = gnubreaks(), sec.axis = gnuaxis(), ...) {
  scale_x_continuous(breaks = breaks, sec.axis = sec.axis, ...)
}

#' @importFrom ggplot2 scale_y_continuous
#' @export

scale_y_gnuplot <- function(breaks = gnubreaks(), sec.axis = gnuaxis(), ...) {
  scale_y_continuous(breaks = breaks, sec.axis = sec.axis, ...)
}

#' @export

gnubreaks <- function(n = 5, padding = 0.1) {
  function(limits) {
    range <- limits[2] - limits[1]

    min <- limits[1] + padding * range
    max <- limits[2] - padding * range

    breaks <- seq(min, max, length.out = n)

    # find appropriate rounding

    between <- (max - min) / (n - 1)
    digits <- -floor(log10(between))
    rounded <- round(breaks, digits)

    while (any(abs(diff(rounded) - between) > 0.01 * between)) {
      digits <- digits + 1
      rounded <- round(breaks, digits)
    }

    rounded
  }
}

#' @importFrom ggplot2 derive dup_axis
#' @export

gnuaxis <- function(trans = ~., name = "", breaks = derive(), labels = NULL,
                    guide = derive()) {
  dup_axis(trans, name, breaks, labels, guide)
}


#' gnuplot theme for ggplot2
#'
#' This theme makes ggplot2 look like gnuplot.
#' It is based on [`ggplot2::theme_linedraw()`][ggplot2::ggtheme]
#' and has inward ticks.
#'
#' @inheritParams ggplot2::theme_linedraw
#'
#' @seealso
#' The [default ggplot2 themes][ggplot2::ggtheme] and [ggplot2::theme()]
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
#' @importFrom ggplot2 element_blank element_rect element_text margin theme
#'                     theme_linedraw unit %+replace%
#' @export

theme_gnuplot <- function(base_size = 11, base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  large <- 1.5 * base_size
  medium <- 1.125 * base_size
  small <- 0.75 * base_size
  tiny <- 0.375 * base_size

  theme_linedraw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace% theme(
    axis.title.x       = element_text(vjust = 1, margin = margin(t = small)),
    axis.title.x.top   = element_text(vjust = 0, margin = margin(b = small)),
    axis.title.y       = element_text(vjust = 1, angle =  90, margin = margin(r = small)),
    axis.title.y.right = element_text(vjust = 0, angle = -90, margin = margin(l = small)),
    axis.text          = element_text(),
    axis.text.x        = element_text(vjust = 1, margin = margin(t = large)),
    axis.text.x.top    = element_text(vjust = 0, margin = margin(b = large)),
    axis.text.y        = element_text(hjust = 1, margin = margin(r = large)),
    axis.text.y.right  = element_text(hjust = 0, margin = margin(l = large)),
    axis.ticks.length  = unit(-small, "pt"),
    legend.text        = element_text(),
    panel.grid         = element_blank(),
    strip.background   = element_blank(),
    strip.placement    = "outside",
    strip.text         = element_text(margin = margin(tiny, tiny, medium, tiny)),
    strip.text.y       = element_text(angle = -90, margin = margin(tiny, tiny, tiny, medium))
  )
}
