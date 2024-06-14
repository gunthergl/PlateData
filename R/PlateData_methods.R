### Methods for the PlateData class

################################################################################
### constructor function for PlateData class

#' Create a \code{PlateData} object
#'
#' Create a \code{PlateData} object from layout
#'
#' @param layout Data.frame containing columns 'plate' and 'well' alongside metadata
#' @param data Data.frame containing columns \code{plate} and \code{well} alongside measurements
#' @param misc List of miscellaneous information
#'
#' @return A \code{\link{PlateData}} object
#'
#' @rdname CreatePlateData
#' @export
#'
CreatePlateData <- function(
  layout,
  data = data.frame(),
  misc = list(),
  index = NULL,
  ...
) {
  my_plate_type <- detect_plate_type(layout)

  if (is.null(index)) {
    stop("No index column has been specified.")
  } else if (!index %in% names(layout) & index %in% names(data)) {
    stop("Please specify a valid index column.")
  }

  methods::new("PlateData", layout = layout, data = data, type = my_plate_type, index = index)
}

################################################################################
### Define validity check for PlateData class object

methods::setValidity("PlateData", function(object) {
    if (!all(c("plate", "well", "row", "col") %in% names(object@layout))) {
      "@layout must contain the columns plate, well, row, and col"
    } else {
      TRUE
    }
  })

  methods::setValidity("PlateData", function(object) {
    if (class(layout(object)$row) != "factor") {
      "@layout$row must be a factor"
    } else {
      TRUE
    }
  })

  methods::setValidity("PlateData", function(object) {
    if (class(layout(object)$row) != "factor") {
      "@layout$row must be a factor"
    } else {
      TRUE
    }
  })


################################################################################
### subsetting an SCESet object

# ...

################################################################################
### layout

#' Accessors for the 'layout' element of an PlateData object.
#'
#' @description 
#' The \code{layout} slot in an PlateData object holds
#' a data.frame containing information attributable to
#' individual wells. Contains one row per unique well.
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' layout(pd)
setMethod("layout", "PlateData", function(x) x@layout)

setMethod("layout<-", "PlateData", function(x, value) {
  x@layout <- value
  validObject(x)
  x
})

################################################################################
### data

#' Accessors for the 'data' element of an PlateData object.
#'
#' @description 
#' The \code{data} slot in an PlateData object holds
#' a data.frame containing measurements attributable to
#' individual wells. Can contain multiple rows per well.
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' data(pd)
setMethod("data", "PlateData", function(x) x@data)

setMethod("data<-", "PlateData", function(x, value) {
  x@data <- value
  validObject(x)
  x
})


setMethod("index", "PlateData", function(x) x@index)

setMethod("type", "PlateData", function(x) x@type)

################################################################################
### show

.pd_show <- function(object) {
  cat(is(object), "\n",
    "Total", nrow(layout(object)), "wells (see layout)", "\n",
    "across", length(type(object)), "plates (see type)", "\n",
    "measuring", nrow(data(object)), "data points", "\n", "\n"
   )
  cat("layout", "\n")
  str(layout(object))
  cat("\n")
  cat("data", "\n")
  str(data(object))
  cat("\n")
  cat("index:", index(object), "\n")
  cat("\n")
  cat("type:", type(object))
}

#' @export 
setMethod("show", "PlateData", .pd_show)