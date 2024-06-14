################################################################################

#' Plot the plate layout
#'
#' @param object PlateData object
#' @param color Column used to color wells
#' @param facet_rows Number of rows to use for facet_wrap
#'
#' @export
#'
plot_plateLayout <- function(
    object = NULL,
    color = NULL,
    facet_rows = 1,
    pt.size = 8,
    theme.size = 20,
    pl.title = "Plate layout",
    xlab = NULL,
    ylab = NULL,
    pl.barwidth=1,
    pl.barheight=10,
    color.pal.type = "RdYlBu",
    color.pal.dir = -1,
    color.values = NULL
) {

  # Check input
  if (is.null(object)) {
    stop("Please provide a layout sheet to visualize.")
  } else if (class(object) != "PlateData") {
    stop("Object must be of type PlateData")
  }

  if (is.null(color)) {
    color <- "row"
  }
  if (!color %in% names(layout(object))) {
    stop(paste("The column name", color, "does not exist. Please check layout."))
  }

  # Wrangle data
  df <- data.frame(
    x = layout(object)$col,
    y = layout(object)$row,
    facet = layout(object)$plate,
    col = layout(object)[[color]]
  )

  # Create background
  bg <- dummyPlate(detect_largest_plate_type(layout(object)), plate_name = "bg")

  # Title
  if (is.null(pl.title)) {
    pl.title <- "Plate layout"
  }

  # Guides
    if (class(df$col) %in% c("numeric", "logical")) {
        guides <- ggplot2::guides(col = ggplot2::guide_colorbar(barwidth = pl.barwidth, barheight = pl.barheight))
        colorscale <- ggplot2::scale_color_distiller(palette = color.pal.type, direction = color.pal.dir)
    } else if (class(df$col) %in% c("character", "factor", "boolean")) {
      if (length(color.values) == length(unique(df$col))) {
        guides <- ggplot2::guides(col = ggplot2::guide_legend())
        colorscale <- ggplot2::scale_color_manual(values=color.values)
      } else {
        guides <- NULL
        colorscale <- NULL
      }
    } else {
        guides <- NULL
        colorscale <- NULL
    }

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, col = col)) +
    ggplot2::geom_point(data=bg, ggplot2::aes(x=col, y=row),
                        col = "black", fill = "grey", shape = 21, size = 3) + # Background
    ggplot2::geom_point(size = pt.size) +
    ggplot2::facet_wrap(~facet, nrow = facet_rows) +
    ggplot2::theme_classic(theme.size) +
    ggplot2::scale_y_discrete(limits = rev(levels(bg$row))) +
    ggplot2::scale_x_discrete(limits = levels(bg$col)) +
    ggplot2::labs(title = pl.title, x = xlab, y = ylab, col = color) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5)
    ) +
    ggplot2::coord_fixed() +
    colorscale +
    guides

  return(p)
}

################################################################################

#' Plot series data
#' 
#' Visualize numeric measurements stored in a PlateData object. 
#' Both x and y axes must be numeric. Lines will be drawn through grouped data points.
#' Can be faceted by categorical vectors. 
#' 
#' @param object PlateData object
#' @param x Column used for y axis
#' @param y Column used for x axis
#' @param col Column name to color measurements by
#' @param group Column used to group measurements
#' @param facet Column used for faceting
#' 
#' @export
plot_series <- function(
  object = NULL,
  x = NULL,
  y = NULL,
  col = NULL,
  group = key(pd),
  facet = "plate",
  facet_scales = "fixed",
  facet_rows = NULL,
  theme_size = 20,
  pl_title = "Series",
  y_transform = NULL,
  x_transform = NULL,
  xlab = NULL,
  ylab = NULL,
  pl.barwidth=1,
  pl.barheight=10,
  color.pal.type = "RdYlBu",
  color.pal.dir = -1,
  color.values = NULL,
  ...
) {
  
  stopifnot(
    !is.null(object),
    !is.null(x),
    !is.null(y)
  )
  
  # Create data.frame with generic names
  dat <- merge(layout(pd), data(pd), by = key(pd))
  df <- data.frame(.key = dat[[key(pd)]])
  
  # Check presence and type of columns
  n <- 1
  cn <- c("x", "y", "group", "facet")
  for (i in c(x, y, group, facet)) {
    j <- cn[[n]]
    if (!i %in% names(dat)) {
      stop(paste("The specified column", i, "for", j, "is not available."))
    } else { 
      df[[j]] <- dat[[i]]
    }
    n <- n+1
  }
  for (i in c(x, y)) {
    if (!class(dat[[i]]) %in% c("integer", "numeric")) {stop(paste0("Column", i, "must be numeric."))}
  }

  # Color
  if (is.null(col)) {
    df[["col"]] <- NaN
  } else if (col %in% names(dat)) {
    df[["col"]] <- dat[[col]]
  } else {
    stop(paste("The specified column", col, "does not exist."))
  }
  
  # Theme
  theme <- ggplot2::theme_bw(base_size = theme_size)
  
  # Coordinate ranges and transformations
  def_scale_trans <- c("asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", 
  "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt", "time")
  if (!is.null(x_transform)) { # Level 1
    if (x_transform %in% def_scale_trans) { # Level 2
      x_scale <- ggplot2::scale_x_continuous(trans = x_transform)
    } else if (x_transform == "minmax") {
      df[[x]] <- minmax(df[[x]])
      x_scale <- NULL
    } else {
      warning(paste("The requested transformation", x_transform, "is not available and will be ignored."))
      x_scale <- NULL
    } # Level 2
  } else {
    x_scale <- NULL
  } # Level 1

  if (!is.null(y_transform)) { # Level 1
    if (y_transform %in% def_scale_trans) { # Level 2
      y_scale <- ggplot2::scale_y_continuous(trans = y_transform)
    } else if (y_transform == "minmax") {
      df <- dplyr::mutate(dplyr::group_by(df, .key), y = minmax(y))
      y_scale <- NULL
    } else {
      warning(paste("The requested transformation", y_transform, "is not available and will be ignored."))
      y_scale <- NULL
    } # Level 2
  } else {
    y_scale <- NULL
  } # Level 1

  # Guides
    if (class(df$col) %in% c("numeric", "logical")) {
        guides <- ggplot2::guides(col = ggplot2::guide_colorbar(barwidth = pl.barwidth, barheight = pl.barheight))
        colorscale <- ggplot2::scale_color_distiller(palette = color.pal.type, direction = color.pal.dir)
    } else if (class(df$col) %in% c("character", "factor", "boolean")) {
      if (length(color.values) == length(unique(df$col))) {
        guides <- ggplot2::guides(col = ggplot2::guide_legend())
        colorscale <- ggplot2::scale_color_manual(values=color.values)
      } else {
        guides <- NULL
        colorscale <- NULL
      }
    } else {
        guides <- NULL
        colorscale <- NULL
    }

  
  # Plot
  plot <- ggplot2::ggplot(df, ggplot2::aes(x, y, col = col)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = group)) +
    ggplot2::facet_wrap(~ facet, scales = facet_scales, nrow=facet_rows) +
    theme  +
    ggplot2::labs(title = pl_title,
         x = xlab, y = ylab) + 
    x_scale + y_scale +
    guides + colorscale
  
  return(plot)
}