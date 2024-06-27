#' Taylor Diagram Geom
#'
#' Creates a Taylor diagram for comparing models to observations.
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data.
#' @param position Position adjustment, either as a string, or the result of a position adjustment function.
#' @param na.rm If FALSE (the default), removes missing values with a warning. If TRUE, silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ... Other arguments passed on to layer.
#' @return A ggplot2 layer.
#' @examples
#' obs <- rnorm(100)
#' mod1 <- obs + rnorm(100, sd = 0.5)
#' mod2 <- obs + rnorm(100, sd = 1.0)
#'
#' obsmod <- tibble(
#'   obs = rep(obs, 2),
#'   mod = c(mod1, mod2),
#'   covariate = rep(c("Model 1", "Model 2"), each = 100)
#' )
#'
#' ggplot(data = obsmod) +
#'   geom_taylor(aes(obs, mod, color = covariate))
#'
#' @export
geom_taylor <- function(mapping = NULL, data = NULL, stat = "taylor",
                        position = "identity", ..., na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE) {
  default_aes <- ggplot2::aes(x = ..x.., y = ..y.., color = NULL)
  if (!is.null(mapping)) {
    mapping <- modifyList(default_aes, mapping)
  } else {
    mapping <- default_aes
  }

  ggplot2::layer(
    stat = StatTaylor, geom = GeomTaylor, mapping = mapping, data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_taylor
#' @usage NULL
StatTaylor <- ggplot2::ggproto(
  "StatTaylor", ggplot2::Stat,
  compute_group = function(data, scales) {
    sd_obs <- sd(data$x)
    sd_mod <- sd(data$y)
    corr <- cor(data$x, data$y)
    centered_rmsd <- sqrt(sd_obs^2 + sd_mod^2 - 2 * sd_obs * sd_mod * corr)

    data.frame(
      x = acos(corr) / (pi/2),  # Map correlation to [0, 1] for polar coordinates
      y = sd_mod,
      sd_obs = sd_obs,
      sd_mod = sd_mod,
      corr = corr,
      centered_rmsd = centered_rmsd
    )
  },
  required_aes = c("x", "y")
)

#' @rdname geom_taylor
#' @usage NULL
GeomTaylor <- ggplot2::ggproto(
  "GeomTaylor",
  ggplot2::GeomPoint,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(shape = 19, colour = "black", size = 3, fill = NA, alpha = NA, stroke = 0.5),

  draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_scales)
    ggplot2::GeomPoint$draw_panel(coords, panel_scales, coord, na.rm = na.rm)
  }
)

coord_taylor <- function(xlim = c(0, 1), ylim = c(0, 1)) {
  ggplot2::ggproto(
    "CoordTaylor", ggplot2::CoordPolar,
    theta = "x", r = "y",
    start = 0,  # Explicitly set start
    direction = 1,  # Explicitly set direction
    limits = list(x = xlim, y = ylim),

    setup_panel_params = function(scale_x, scale_y, params = list(), self) {
      ret <- ggplot2::ggproto_parent(ggplot2::CoordPolar, self)$setup_panel_params(scale_x, scale_y, params)

      ret$r.range <- self$limits$y
      if (diff(ret$r.range) == 0) {
        ret$r.range <- c(0, max(ret$r.range) + 0.1)
      }
      ret$r.major <- pretty(ret$r.range)
      ret$r.minor <- seq(ret$r.range[1], ret$r.range[2], length.out = 100)

      ret$theta.range <- c(0, 2 * pi)
      ret$theta.major <- seq(0, 2 * pi, length.out = 9)[-9]
      ret$theta.minor <- seq(0, 2 * pi, length.out = 100)

      # Ensure lengths of theta.labels and r.major match
      ret$theta.labels <- round(cos(ret$theta.major), 2)
      ret$theta.labels <- ret$theta.labels[seq_along(ret$r.major)]
      ret$r.labels <- as.character(round(ret$r.major, 2))

      ret
    },

    render_bg = function(panel_params, theme, self) {
      ggplot2::ggproto_parent(ggplot2::CoordPolar, self)$render_bg(panel_params, theme)
    },

    render_axis_h = function(panel_params, theme, self) {
      ggplot2::ggproto_parent(ggplot2::CoordPolar, self)$render_axis_h(panel_params, theme)
    },

    render_axis_v = function(panel_params, theme, self) {
      ggplot2::ggproto_parent(ggplot2::CoordPolar, self)$render_axis_v(panel_params, theme)
    }
  )
}

# Create the plot
ggplot(data = your_data) +
  geom_taylor(aes(x = obs, y = mod, color = model)) +
  coord_taylor() +
  scale_x_continuous(name = "Correlation", breaks = seq(0, 1, by = 0.2),
                     labels = function(x) round(cos(x * pi/2), 2)) +
  scale_y_continuous(name = "Standard Deviation") +
  theme_minimal()



# Helper functions for coord_taylor
gridCircle <- function(r, xc = 0, yc = 0, col = "grey90", ...) {
  t <- seq(0, 2*pi, length.out = 100)
  x <- xc + r * cos(t)
  y <- yc + r * sin(t)
  grid::polygonGrob(x, y, default.units = "native", gp = grid::gpar(col = col, fill = NA, ...))
}

gridRadial <- function(angle, r = 1, xc = 0, yc = 0, col = "grey90", ...) {
  x <- xc + c(0, r * cos(angle))
  y <- yc + c(0, r * sin(angle))
  grid::segmentsGrob(x[1], y[1], x[2], y[2], default.units = "native", gp = grid::gpar(col = col, ...))
}

gridLabels <- function(angle, r, labels = c("1", "0.99", "0.95", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1", "0"), ...) {
  x <- r * cos(angle)
  y <- r * sin(angle)
  grid::textGrob(labels, x, y, default.units = "native", hjust = "center", vjust = "bottom", rot = angle * 180 / pi, ...)
}


