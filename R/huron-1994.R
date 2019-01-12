huron_1994 <- function(x) {
  UseMethod("huron_1994")
}

huron_1994.default <- function(x) {
  x <- hrep::pc_set(x)
  huron_1994.pc_set(x)
}

huron_1994.pc_set <- function(x) {
  x <- hrep::int_vec(x)
  huron_1994.int_vec(x)
}

huron_1994.int_vec <- function(x) {
  sum(x * huron_1994_weights)
}

huron_1994_weights <- c(- 1.428,
                        - 0.582,
                        0.594,
                        0.386,
                        1.240,
                        -0.453)
