get_model <- function(x) {
  df <- dplyr::filter(incon_models, .data$label == x)
  stopifnot(nrow(df) == 1L)
  y <- as.list(df)
  for (i in seq_along(y)) if (is.list(y[[i]])) y[[i]] <- y[[i]][[1L]]
  class(y) <- c("incon_model", class(y))
  y
}
