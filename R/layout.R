#' Plot the plate layout
#' 
#' @param object Data.frame containing layout
#' @param color Column used to color wells
#' @param row.name Column indicating plate rows
#' @param col.name Column indicating plate columns
#'
plot_plateLayout <- function(
    object = NULL,
    color = NULL,
    row.name = "row",
    col.name = "col",
    pl.title = NULL
) {
  
  # Check input
  stopifnot(
    !is.null(object),
    !is.null(color),
    row.name %in% names(object),
    col.name %in% names(object)
  )
  if (!color %in% names(object)) {
    stop(paste("The column name", color, "does not exist. Please check layout."))
  }
  
  # Wrangle data
  object$x <- object[[col.name]]
  object$y <- object[[row.name]]
  object$col <- object[[color]]
  
  # Title
  if (!is.null(run$name)) {
    pl.title <- paste0("Plate layout of run ", run$name)
  } else {
    pl.title <- "Plate layout (run unknown)"
  }
  
  # Plot
  p <- ggplot(object, aes(x = x, y = y, col = col)) +
    geom_point(size = 10)+
    theme_classic(20) +
    scale_y_discrete(limits = rev(levels(object$y))) +
    scale_x_discrete(limits = levels(object$x)) +
    labs(title = pl.title, x = NULL, y = NULL, col = color) +
    theme(
      axis.title.y = element_text(angle = 0, vjust = 0.5)
    )
  
  return(p)
}