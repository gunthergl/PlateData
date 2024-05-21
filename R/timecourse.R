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
  theme_size = 20
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
      strip.text.y     = ggplot2::element_text(angle = 0)
    )
  
  # Faceting
  df$facet_x <- object[[facet_x]]
  df$facet_y <- object[[facet_y]]
  
  # Plot
  plot <- ggplot2::ggplot(df, ggplot2::aes(x, y, col = col)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = group)) +
    ggplot2::facet_grid(facet_x ~ facet_y, scales = facet_scales) +
    theme
  
  return(plot)
}