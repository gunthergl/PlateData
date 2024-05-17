# Functions

# Combine row and column names to create full plate
#
# allrows <- LETTERS[1:match(max(df$row), LETTERS)]
# allcols <- unique(df$col)
#
combine_vectors <- function(x, y, sep = "") {
  rect <- expand.grid(x, y, sep = sep)
  vect <- apply(rect, 1, function(x) {paste0(x, collapse = sep)})
  return(vect)
}
