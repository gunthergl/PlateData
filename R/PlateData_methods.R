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
#' @param key Key to match data.frames between layout and data
#' @param remove_unregistered_wells Whether to remove wells from data that do not appear in layout
#' @param add_unregistered_wells Whether to add wells that appear in data to layout
#'
#' @return A \code{\link{PlateData}} object
#'
#' @rdname CreatePlateData
#' @export
#'
CreatePlateData <- function(layout, # nolint: object_name_linter.
                            data = NULL,
                            misc = list(),
                            key = "key",
                            remove_unregistered_wells = FALSE,
                            add_unregistered_wells = FALSE,
                            ...) {
    my_plate_type <- detect_plate_type(layout)

    if (!key %in% names(layout) && key %in% names(data)) {
        stop("Please specify a valid key column.")
    }

    # Use key as row.names
    row.names(layout) <- layout[[key]]
    layout <- layout[-which(names(layout) == key)]

    # Add data
    if (is.null(data)) {
        data <- data.frame()
        fix_unregistered_samples <- FALSE
    } else {
        fix_unregistered_samples <- TRUE
    }

    # Deal with unregistered wells
    if (fix_unregistered_samples) {
        if (remove_unregistered_wells) {
            index <- which(!data[[key]] %in% row.names(layout))
            data <- data[-index, ]
            message(paste(
                "Removing unregistered wells:",
                stringr::str_flatten(na.omit(unique(data[[key]][index])), collapse = ", ")
            ))
        }
        if (add_unregistered_wells) {
            stop("Not implemented yet.")
        }
    }

    methods::new("PlateData", layout = layout, data = data, type = my_plate_type, key = key)
}

#-------------------------------------------------------------------------------
# Define validity check for PlateData class object
#-------------------------------------------------------------------------------

.pd_validity <- function(object) {
    msg <- NULL
    valid <- TRUE

    ## Define necessary columns in layout
    if (!all(c("plate", "well", "row", "col") %in% names(object@layout))) {
        msg <- c(msg, "@layout must contain the columns plate, well, row, and col")
        valid <- FALSE
    }

    ## Check that the key is present in data and used as unique row.names in layout
    if (key(object) %in% names(layout(object))) {
        msg <- c(msg, "key(object) must be registered with data(object) not layout(object).")
        valid <- FALSE
    }

    if (any(duplicated(row.names(layout(object))))) {
        msg <- c(msg, "Duplicated key in layout(object). Must be unique.")
        valid <- FALSE
    }

    ## In case data(object) is not empty
    if (sum(dim(data(object))) != 0) {
        ## Check that data(object) has a key column
        if (!key(object) %in% names(data(object))) {
            msg <- c(msg, "No column corresponding to key(object) found in data(object).")
            valid <- FALSE
        }

        ## Check that all data rows are associated to a registered layout key
        if (!all(data(object)[[key(object)]] %in% row.names(layout(object)))) {
            msg <- c(msg, "Some samples in data(object) are not registered in layout(object).")
            valid <- FALSE
        }
    }

    ## Check that columns are unique across layout and data
    all_cols <- c(names(layout(object)), names(data(object)))
    if (any(duplicated(all_cols))) {
        msg <- c(msg, paste("column '", all_cols[duplicated(all_cols)], "'is duplicated in layout and data."))
        valid <- FALSE
    }

    ## Check that row and col are factors
    if (class(layout(object)$row) != "factor") {
        msg <- c(msg, "@layout$row must be a factor")
        valid <- FALSE
    }
    if (class(layout(object)$col) != "factor") {
        msg <- c(msg, "@layout$col must be a factor")
        valid <- FALSE
    }

    if (valid) TRUE else msg
}

methods::setValidity("PlateData", .pd_validity)

#-------------------------------------------------------------------------------
# subsetting a PlateData object
#-------------------------------------------------------------------------------

.pd_subset <- function(x) {

}

setMethod("subset", "PlateData", .pd_subset)

#-------------------------------------------------------------------------------
# merging PlateData object
#-------------------------------------------------------------------------------

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
#' layout(object)
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
#' data(object)
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
    cat(
        is(object), "\n",
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
