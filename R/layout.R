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
  if (is.null(object)) {
    stop("Please provide a layout sheet to visualize.")
  }
  stopifnot(
    row.name %in% names(object),
    col.name %in% names(object)
  )
  if (is.null(color)) {
    color <- row.name
  }
  if (!color %in% names(object)) {
    stop(paste("The column name", color, "does not exist. Please check layout."))
  }

  # Wrangle data
  object$x <- object[[col.name]]
  object$y <- object[[row.name]]
  object$col <- object[[color]]

  # Create background
  bg <- dummyPlate(last_well = paste0(
    tail(levels(object$y), 1), tail(levels(object$x), 1)
  ))

  # Title
  if (is.null(pl.title)) {
    pl.title <- "Plate layout"
  }

  # Plot
  p <- ggplot2::ggplot(object, ggplot2::aes(x = x, y = y, col = col)) +
    ggplot2::geom_point(data=bg, ggplot2::aes(x=col, y=row),
                        col = "black", fill = "grey", shape = 21, size = 3) + # Background
    ggplot2::geom_point(size = 10)+
    ggplot2::theme_classic(20) +
    ggplot2::scale_y_discrete(limits = rev(levels(object$y))) +
    ggplot2::scale_x_discrete(limits = levels(object$x)) +
    ggplot2::labs(title = pl.title, x = NULL, y = NULL, col = color) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5)
    )

  return(p)
}
