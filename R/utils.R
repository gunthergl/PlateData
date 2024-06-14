#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Minmax scaling
#' 
#' Rescales numeric vector to range 0-1
#' 
#' @param x Numeric vector
#' 
#' @export
minmax <- function(x) {
    y <- (x - min(x)) / (max(x) - min(x))
}

#' Mutate well to row and column indices
#' 
#' @param object data.frame with column well
#' 
#' @export
mutate_well_to_row_col_indices <- function(object) {

  stopifnot(
    is.data.frame(object),
    "well" %in% names(object)
  )

  object <- dplyr::mutate(object, 
                          row = factor(as.character(stringr::str_split_fixed(well, "", 2)[,1])), 
                          col = factor(as.numeric(stringr::str_split_fixed(well, "", 2)[,2]))
                          )

  return(object)
}

#' Detect plate type
#' 
#' Detect the plate type (e.g. 6-, 24- or 96-well) from the well names. Assigns
#' one type per plate. Based on sorting the row and column indices and looking 
#' for the closest plate type.
#' 
#' @param object Data.frame with columns 'plate', 'row', and 'col'
#' 
#' @return Named vector of plate type per plate
#' 
#' @export
detect_plate_type <- function(object) {
    
    stopifnot(
      c("plate", "well", "row", "col") %in% names(object)
    )

    ptype <- character(length = length(unique(object$plate)))
    names(ptype) <- unique(object$plate)
    pt <- plate_types
    pt <- dplyr::mutate(pt, row = as.character(stringr::str_split_fixed(last_well, "", 2)[,1]), 
                        col = as.numeric(stringr::str_split_fixed(last_well, "", 2)[,2])
                        )

    for (i in unique(object$plate)) {
        p <- subset(object, plate == i)
        last_well <- paste0(
          max(as.character(p$row)), 
          max(as.numeric(p$col))
          )
        pt$fit <- as.character(stringr::str_split_fixed(last_well, "", 2)[, 1]) <= pt$row & as.numeric(stringr::str_split_fixed(last_well, "", 2)[, 2]) <= pt$col

        ptype[[i]] <- pt$type[pt$last_well == min(subset(pt, fit)$last_well)]
    }
    
    return(ptype)
}

#' Detect largest plate type
#' 
#' @param object  Data.frame with columns 'plate', 'row', and 'col'
#' 
#' @return Character vector with only one plate type
#' 
#' @export 
detect_largest_plate_type <- function(object) {

  # Detect plate types
  ptype <- detect_plate_type(object)

  # Extract largest plate type
  ind <- order(as.numeric(stringr::str_split(ptype, "-", simplify = TRUE)[, 1]), decreasing = TRUE)
  ptype <- ptype[ind][[1]]

  return(ptype)
}

#' Create dummy plate
#'
#' Creates a layout sheet of an empty microtiter well. The size of the plate
#' is determined either by specifying a type (e.g. 96-well) or by the bottom right corner (e.g. H12 of a 96-well plate).
#'
#' @param type Name of layout (e.g. 6-well, also for 12, 24, 48, 96)
#' @param last_well Name of the last well on the plate (e.g. H12)
#'
#' @returns Data.frame with layout of microtiter plate
#' @export
#'
dummyPlate <- function(
    type = NULL,
    last_well = NULL,
    plate_name = "dummy",
    separator = "_"
    ) {

  # Check for type
  if (!is.null(type)) {
    if (!is.null(last_well)) {
      warning("Both type and last_well have been specified. Default to using type.")
    }
    if (type %in% plate_types$type) {
      index <- match(type,plate_types$type)
      last_well <- plate_types$last_well[index]
    } else {
      warning("The plate type you specified is not available. Please use another type or specify a 'last_well'.")
    }
  }

  # Check for last_well
  if (is.null(last_well)) {
    stop("Please specify a layout type or well")
  } else {
    cond <- c("^[:alpha:][:digit:]$", "^[:alpha:][:digit:][:digit:]$")
    if (!any(stringr::str_detect(last_well, cond))) {
      stop("Formatting of 'last_well' does not fit. Please use alphanumeric keys, e.g. H12.")
    }
  }

  stopifnot(
    class(last_well) == "character",
    length(last_well) == 1
  )

  # Separate row and column index
  last_well <- stringr::str_to_upper(last_well)
  ind <- stringr::str_split_fixed(last_well, "", 2)

  # Create all rows until last
  cols <- 1:ind[2]
  rows <- LETTERS[1:match(ind[1], LETTERS)]

  # Create layout
  df <- expand.grid(rows, cols)
  names(df) <- c("row", "col")
  df$well <- stringr::str_c(df$row, df$col)
  df$plate <- plate_name
  df$index <- paste(df$plate, df$well, sep=separator)
  row.names(df) <- df$index
  df <- df[order(df$plate, df$row, df$col), c("index", "plate", "well", "row", "col")]

  # Specify vector types
  df$row <- factor(df$row)
  df$col <- factor(df$col)

  return(df)
}

#' Combine layout and data
#'
#' Combine data.frames with data and layout
#'
#' @param data Data.frame with data
#' @param layout Data.frame with layout
#' @param match_key Column name for matching both data.frames (default: well)
#'
#' @export
#'
combine_layout_and_data <- function(
  data = NULL,
  layout = NULL,
  match_key = "well"
) {

  stopifnot(
    !is.null(data),
    !is.null(layout)
  )

  # Join tables
  index <- match(data[[match_key]], layout[[match_key]])
  cols <- names(layout)[!names(layout) %in% c("well", "row", "col")]
  data <- cbind(data, layout[index, cols])

  return(data)
}
