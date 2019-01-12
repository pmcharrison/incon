context("test-parameter-sensitivity")

test_sensitivity <- function(param, param_val, chord = c(60, 64, 67)) {
  for (i in seq_len(nrow(incon_models))) {
    res <- purrr::map_dbl(param_val, function(val) {
      args <- list(x = chord, model = incon_models$label[i])
      args[[param]] <- val
      do.call(incon, args)
    })
    info <- paste0("Parameter sensitivity check failed ",
           "(model = ", incon_models$label[i], ", ",
           "parameter = ", param, ")")
    if (incon_models$spectrum_sensitive[i])
      expect_equal(length(unique(res)), length(res),
                   info = info) else
        expect_equal(length(unique(res)), 1L, info = info)
  }
}

test_sensitivity("num_harmonics", c(5, 11))
test_sensitivity("roll_off", c(1, 0.75))
