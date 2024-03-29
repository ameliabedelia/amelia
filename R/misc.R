#' Add a logo to your plot or image
#'
#' This function places a logo at one of the corners of your image/plot
#'
#' @export

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){

     # Requires magick R Package

     # Useful error message for logo position
     if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
          stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
     }

     # read in raw images
     plot <- image_read(plot_path)
     logo_raw <- image_read(logo_path)

     # get dimensions of plot for scaling
     plot_height <- image_info(plot)$height
     plot_width <- image_info(plot)$width

     # default scale to 1/10th width of plot
     # can change by altering logo_scale
     logo <- image_scale(logo_raw, as.character(plot_width/logo_scale))

     # Get width of logo
     logo_width <- image_info(logo)$width
     logo_height <- image_info(logo)$height


     # Set position of logo
     # Position starts at 0,0 at top left
     # Using 0.01 for 1% - aesthetic padding

     if (logo_position == "top right") {
          x_pos = plot_width - logo_width - 0.01 * plot_width
          y_pos = 0.01 * plot_height
     } else if (logo_position == "top left") {
          x_pos = 0.01 * plot_width
          y_pos = 0.01 * plot_height
     } else if (logo_position == "bottom right") {
          x_pos = plot_width - logo_width - 0.01 * plot_width
          y_pos = plot_height - logo_height - 0.01 * plot_height
     } else if (logo_position == "bottom left") {
          x_pos = 0.01 * plot_width
          y_pos = plot_height - logo_height - 0.01 * plot_height
     }

     # Compose the actual overlay
     image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))

}
