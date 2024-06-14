#' Plate types available for dummyPlate
#' 
#' Stores the 'last_well' for a number of commonly used plate types,
#' indicating the bottom right corner of a plate to distinguish
#' plate dimensions.
#' 
plate_types <- data.frame(
    type = c("6-well", "12-well", "24-well", "48-well", "96-well", "384-well"),
    last_well = c("B3", "C4", "D6", "F8", "H12", "P24")
  )