# theme color help from https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

#cb friendly color palette
cb_colors <- c(
     `grey` = "#999999",
     `orange` = "#E69F00",
     `light blue` = "#56B4E9",
     `green` = "#009E73",
     `yellow` = "#F0E442",
     `dark blue` = "#0072B2",
     `orange red` = "#D55E00",
     `pink` = "#CC79A7",
     `black` = "#000000"
)

#' Function to extract amelia colors as hex codes
#'
#' @param ... Character names of cb_colors
#' @export
amelia_colors <- function(...) {
        cb_colors <- c(
                `grey` = "#999999",
                `orange` = "#E69F00",
                `light blue` = "#56B4E9",
                `green` = "#009E73",
                `yellow` = "#F0E442",
                `dark blue` = "#0072B2",
                `orange red` = "#D55E00",
                `pink` = "#CC79A7",
                `black` = "#000000"
        )
     cols <- c(...)
     if (is.null(cols))
          return (cb_colors)

     cb_colors[cols]
}


amelia_palettes <- list(
     `cb_grey`  = amelia_colors("grey", "orange", "light blue", "green", "yellow", "dark blue",
                                "orange red", "pink", "black"),
     `cb_black`  = amelia_colors("black", "orange", "light blue", "green", "yellow", "dark blue",
                                "orange red", "pink", "grey")
)

#' Function to provide color blind-friendly scales
#'
#' @param palette Name of color palette to use (currently, either "cb_grey" or "cb_black")
#' @export
scale_color_amelia <- function(..., palette = "cb_grey") {
     ggplot2::scale_color_manual(..., values = unname(amelia_palettes[[palette]]))
}


#' Function to provide color blind-friendly fills
#'
#' @param palette Name of color palette to use (currently, either "cb_grey" or "cb_black")
#' @export
scale_fill_amelia <- function(..., palette = "cb_grey") {
     ggplot2::scale_fill_manual(..., values = unname(amelia_palettes[[palette]]))
}

#' Theme from cowplot
#'
#' https://github.com/wilkelab/cowplot
#'
#' @export
#'
theme_halfopen <- function(font_size = 14, font_family = "", line_size = .5,
                          rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
     half_line <- font_size / 2
     small_size <- rel_small * font_size

     # work off of theme_grey just in case some new theme element comes along
     theme_grey(base_size = font_size, base_family = font_family) %+replace%
          theme(
               line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
               rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
               text              = element_text(family = font_family, face = "plain", color = "black",
                                                size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                                margin = margin(), debug = FALSE),

               axis.line         = element_line(color = "black", size = line_size, lineend = "square"),
               axis.line.x       = NULL,
               axis.line.y       = NULL,
               axis.text         = element_text(color = "black", size = small_size),
               axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
               axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
               axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1),
               axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
               axis.ticks        = element_line(color = "black", size = line_size),
               axis.ticks.length = unit(half_line / 2, "pt"),
               axis.title.x      = element_text(
                    margin = margin(t = half_line / 2),
                    vjust = 1
               ),
               axis.title.x.top  = element_text(
                    margin = margin(b = half_line / 2),
                    vjust = 0
               ),
               axis.title.y      = element_text(
                    angle = 90,
                    margin = margin(r = half_line / 2),
                    vjust = 1
               ),
               axis.title.y.right = element_text(
                    angle = -90,
                    margin = margin(l = half_line / 2),
                    vjust = 0
               ),


               legend.background = element_blank(),
               legend.spacing    = unit(font_size, "pt"),
               legend.spacing.x  = NULL,
               legend.spacing.y  = NULL,
               legend.margin     = margin(0, 0, 0, 0),
               legend.key        = element_blank(),
               legend.key.size   = unit(1.1 * font_size, "pt"),
               legend.key.height = NULL,
               legend.key.width  = NULL,
               legend.text       = element_text(size = rel(rel_small)),
               legend.text.align = NULL,
               legend.title      = element_text(hjust = 0),
               legend.title.align = NULL,
               legend.position   = "right",
               legend.direction  = NULL,
               legend.justification = c("left", "center"),
               legend.box        = NULL,
               legend.box.margin =  margin(0, 0, 0, 0),
               legend.box.background = element_blank(),
               legend.box.spacing = unit(font_size, "pt"),

               panel.background  = element_blank(),
               panel.border      = element_blank(),
               panel.grid        = element_blank(),
               panel.grid.major  = NULL,
               panel.grid.minor  = NULL,
               panel.grid.major.x = NULL,
               panel.grid.major.y = NULL,
               panel.grid.minor.x = NULL,
               panel.grid.minor.y = NULL,
               panel.spacing     = unit(half_line, "pt"),
               panel.spacing.x   = NULL,
               panel.spacing.y   = NULL,
               panel.ontop       = FALSE,

               strip.background  = element_rect(fill = "grey80"),
               strip.text        = element_text(
                    size = rel(rel_small),
                    margin = margin(half_line / 2, half_line / 2,
                                    half_line / 2, half_line / 2)
               ),
               strip.text.x      = NULL,
               strip.text.y      = element_text(angle = -90),
               strip.placement   = "inside",
               strip.placement.x =  NULL,
               strip.placement.y =  NULL,
               strip.switch.pad.grid = unit(half_line / 2, "pt"),
               strip.switch.pad.wrap = unit(half_line / 2, "pt"),

               plot.background   = element_blank(),
               plot.title        = element_text(
                    face = "bold",
                    size = rel(rel_large),
                    hjust = 0, vjust = 1,
                    margin = margin(b = half_line)
               ),
               plot.subtitle     = element_text(
                    size = rel(rel_small),
                    hjust = 0, vjust = 1,
                    margin = margin(b = half_line)
               ),
               plot.caption      = element_text(
                    size = rel(rel_tiny),
                    hjust = 1, vjust = 1,
                    margin = margin(t = half_line)
               ),
               plot.tag           = element_text(
                    face = "bold",
                    hjust = 0, vjust = 0.7
               ),
               plot.tag.position = c(0, 1),
               plot.margin       = margin(half_line, half_line, half_line, half_line),

               complete = TRUE
          )
}

#' Minimalistic themes with grids from Cowplot
#' https://github.com/wilkelab/cowplot
#'
#' Three minimalistic themes that provide either a full grid,
#' a horizontal grid, or a vertical grid.
#'
#' `theme_minimal_grid()` provides a minimal grid theme. `theme_minimal_hgrid()` strips down
#' this theme even further and draws only horizontal lines, and `theme_minimal_vgrid()`
#' does the same for vertical lines.
#' @export
theme_minimal_grid <- function(font_size = 14, font_family = "", line_size = .5,
                               rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                               color = "grey85", colour) {
     if (!missing(colour)) {
          color <- colour
     }

     # Starts with theme_halfopen and then modifies some parts
     theme_halfopen(font_size = font_size, font_family = font_family, line_size = line_size,
                   rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) %+replace%
          theme(
               # make grid lines
               panel.grid        = element_line(color = color,
                                                size = line_size),
               panel.grid.minor  = element_blank(),

               # adjust axis tickmarks
               axis.ticks        = element_line(color = color, size = line_size),

               # no x or y axis lines
               axis.line.x       = element_blank(),
               axis.line.y       = element_blank(),

               # no filled background for facted plots
               strip.background = element_blank(),

               complete = TRUE
          )
}
#' @rdname theme_minimal_grid
#' @export
theme_minimal_vgrid <- function(font_size = 14, font_family = "", line_size = .5,
                                rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                                color = "grey85", colour) {
     if (!missing(colour)) {
          color <- colour
     }

     # Starts with theme_grid and then modifies some parts
     theme_minimal_grid(font_size = font_size, font_family = font_family, line_size = line_size,
                        rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large,
                        color = color) %+replace%
          theme (
               # no horizontal grid lines
               panel.grid.major.y = element_blank(),

               # add a y axis line
               axis.line.y        = element_line(color = color, size = line_size),

               complete = TRUE
          )
}

#' @rdname theme_minimal_grid
#' @export
theme_minimal_hgrid <- function(font_size = 14, font_family = "", line_size = .5,
                                rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                                color = "grey85", colour) {
     if (!missing(colour)) {
          color <- colour
     }

     # Starts with theme_grid and then modifies some parts
     theme_minimal_grid(font_size = font_size, font_family = font_family, line_size = line_size,
                        rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large,
                        color = color) %+replace%
          theme (
               # no vertical grid lines
               panel.grid.major.x = element_blank(),

               # add a x axis line
               axis.line.x       = element_line(color = color, size = line_size),

               complete = TRUE
          )
}
