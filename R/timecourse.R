#' Plot time course data
#' 
#' Visualize measurements across time
#' 
#' @param object Data.frame
#' @param x Column used for y axis
#' @param y Column used for x axis
#' @param col Column name to color measurements by
#' @param group Column used to group measurements
#' 
#' @export
#' 
plot_timecourse <- function(
  object = NULL,
  x = "time",
  y = "count",
  col = "phage",
  group = "well",
  facet_x = "mode",
  facet_y = "host_number",
  facet_scales = "free",
  theme_size = 20,
  pl_title = "Timecourse",
  y_transform = NULL,
  x_transform = NULL,
  xlab = "Time in hours",
  ylab = NULL
) {
  
  stopifnot(
    !is.null(object)
  )
  
  # Create data.frame with generic names
  df <- data.frame(
    x = object[[x]],
    y = object[[y]]
  )
  
  # Color
  if (!is.na(col)) {
    df$col <- object[[col]]
  }
  
  # Grouping
  if (!is.na(group)) {
    df$group <- object[[group]]
  }
  
  # Theme
  theme <- ggplot2::theme_classic(base_size = theme_size) + 
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "grey98"),
      panel.grid.major.y = ggplot2::element_line(),
      strip.text.y     = ggplot2::element_text(angle = 0)
    )
  
  # Faceting
  df$facet_x <- object[[facet_x]]
  df$facet_y <- object[[facet_y]]
  
  # Coordinate ranges and transformations
  #ymax <- ceiling(max(df$y))
  #yrange <- 10^(0:10)
  #yrange <- 10^(0:max(which(ymax > yrange)))
  if (!is.null(y_transform)) {
    y_scale <- ggplot2::scale_y_continuous(trans = y_transform)
  } else {
    y_scale <- NULL
  }
  xmax <- ceiling(max(df$x))
  xrange <- seq(0, xmax, 2)
  if (!is.null(x_transform)) {
    x_scale <- ggplot2::scale_x_continuous(trans = x_transform, breaks = yrange)
  } else {
    x_scale <- ggplot2::scale_x_continuous(breaks = xrange)
  }

  
  # Plot
  plot <- ggplot2::ggplot(df, ggplot2::aes(x, y, col = col)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = group)) +
    ggplot2::facet_grid(facet_x ~ facet_y, scales = facet_scales) +
    theme  +
    ggplot2::labs(title = pl_title,
         x = xlab, y = ylab) + x_scale + y_scale
  
  return(plot)
}