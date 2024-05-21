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
    last_well = NULL
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
