.onLoad <- function(libname, pkgname) {
    plate_types_default <- data.frame(
        type = as.character(c("6-well", "12-well", "24-well", "48-well", "96-well", "384-well")),
        last_well = as.character(c("B3", "C4", "D6", "F8", "H12", "P24"))
    )
    assign("plate_types_default",
        plate_types_default,
        envir = parent.env(environment())
    )
}
