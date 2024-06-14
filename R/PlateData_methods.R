### Methods for the PlateData class

#-------------------------------------------------------------------------------
# constructor function for PlateData class
#-------------------------------------------------------------------------------

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
  data = NULL,
  misc = list(),
  key = NULL,
  ...
) {
  my_plate_type <- detect_plate_type(layout)

  if (is.null(key)) {
    stop("No key column has been specified.")
  } else if (!key %in% names(layout) & key %in% names(data)) {
    stop("Please specify a valid key column.")
  }

  # Use key as row.names
  row.names(layout) <- layout[[key]]
  layout <- layout[-which(names(layout) == key)]

  methods::new("PlateData", layout = layout, data = data, type = my_plate_type, key = key)
}

#-------------------------------------------------------------------------------
# Define validity check for PlateData class object
#-------------------------------------------------------------------------------

methods::setValidity("PlateData", function(object) {
    if (!all(c("plate", "well", "row", "col") %in% names(object@layout))) {
      "@layout must contain the columns plate, well, row, and col"
    } else {
      TRUE
    }
  })

  methods::setValidity("PlateData", function(object) {
    if (key(object) %in% names(layout(object))) {
      "key(object) must be registered with data(object) not layout(object)."
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


#-------------------------------------------------------------------------------
# subsetting an SCESet object
#-------------------------------------------------------------------------------

.pd_subset <- function(x) {

}

setMethod("subset", "PlateData", .pd_subset)

#-------------------------------------------------------------------------------
# layout
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
# data
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
# key
#-------------------------------------------------------------------------------
setMethod("key", "PlateData", function(x) x@key)

#-------------------------------------------------------------------------------
# type
#-------------------------------------------------------------------------------
setMethod("type", "PlateData", function(x) x@type)

#-------------------------------------------------------------------------------
# show
#-------------------------------------------------------------------------------

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
  cat("key:", key(object), "\n")
  cat("\n")
  cat("type:", type(object))
}

#' @export 
setMethod("show", "PlateData", .pd_show)