context("test-cpp")

test_that("get_index", {
  x <- c(0, 1, 2, 3)

  expect_equal(get_index(x, index = 0, offset = 0), 0)
  expect_equal(get_index(x, index = 1, offset = 0), 1)
  expect_equal(get_index(x, index = 2, offset = 0), 2)
  expect_equal(get_index(x, index = 3, offset = 0), 3)

  expect_equal(get_index(x, index = 0, offset = 1), 1)
  expect_equal(get_index(x, index = 1, offset = 1), 2)
  expect_equal(get_index(x, index = 2, offset = 1), 3)
  expect_equal(get_index(x, index = 3, offset = 1), 0)

  expect_equal(get_index(x, index = 0, offset = 2), 2)
  expect_equal(get_index(x, index = 1, offset = 2), 3)
  expect_equal(get_index(x, index = 2, offset = 2), 0)
  expect_equal(get_index(x, index = 3, offset = 2), 1)
})

test_that("cosine_similarity", {
  x <- c(12, 25, -40, 20, 49)
  y <- c(-40, 30, 10, 5, -30)

  expect_equal(
    cosine_similarity_cpp(x, y, offset = 0), # <-- C++ version
    cosine_similarity(x, y), # <-- R version
  )

  expect_equal(
    cosine_similarity_cpp(x, y, offset = 1),
    cosine_similarity(x[c(2:5, 1)], y),
  )

  expect_equal(
    cosine_similarity_cpp(x, y, offset = 2),
    cosine_similarity(x[c(3:5, 1:2)], y),
  )

  expect_equal(
    cosine_similarity_cpp(x, y, offset = 3),
    cosine_similarity(x[c(4:5, 1:3)], y),
  )
})

test_that("sweep_template", {
  x <- c(-5, 20, 15, 35, 40, 20)
  y <- c(20, 15, 27, 40, 10, 15)

  expect_equal(
    sweep_template(x, y, legacy = TRUE),
    sweep_template_cpp(x, y)
  )
})

context("test-harmonicity")

library(hrep)
library(magrittr)

test_that("Legacy comparisons with HarmonyStats package", {
  pc_set_ids <- c(1, 100, 300, 500, 650, 800, 900, 1000, 1200, 1500)
  pc_sets <- pc_set_ids %>% coded_vec("pc_set") %>% decode %>% as.list
  pc_set_sizes <- vapply(pc_sets, length, integer(1))

  # The following commented out code was used to generate the
  # reference vector.

  # library(tidyverse)
  # x <- unclass(readRDS("/Users/peter/Dropbox/Academic/projects/pearce-marcus/harmony/HarmonyStats/inst/extdata/feature_cache.rds"))
  # scale_info <- attr(x, "scale_info")$instantaneous %>% filter(measure == "harmonicity")
  #
  # ref <- tibble(pc_set_id = pc_set_ids,
  #               size = pc_set_sizes,
  #               scaled_harmonicity = x@data$`NA`["harmonicity", pc_set_ids],
  #               center = scale_info$center[size],
  #               scale = scale_info$scale[size],
  #               harmonicity = scaled_harmonicity * scale + center) %>%
  #   pull(harmonicity)
  # dump("ref", file = "")

  ref <-
    c(1.56025992760304, 0.763094086276816, 0.699233982570559, 0.525533347758732,
      0.649556486732275, 0.763094086276816, 0.774127828829956, 0.525533347758732,
      0.699233982570559, 0.552087654180455)

  expect_equal(ref,
               pc_sets %>% vapply(pc_harmonicity, numeric(1)))
})

context("test-kl_div_from_uniform")

library(hrep)
library(magrittr)

test_that("example output", {
  spec <- c(0.2, 0.4, 0.3, 0.1) %>% .milne_pc_spectrum()
  unif <- 1 / length(spec)
  d <- 0
  for (i in seq_along(spec)) {
    d <- d + spec[i] * log(spec[i] / unif, base = 2)
  }
  expect_equal(
    d, kl_div_from_uniform(spec)
  )
})

test_that("invariance to the number of bins", {
  v1 <- c(0.2, 0.4, 0.3, 0.1)
  v2 <- rep(v1, each = 2)
  s1 <- .milne_pc_spectrum(v1)
  s2 <- .milne_pc_spectrum(v2)
  expect_equal(
    kl_div_from_uniform(s1),
    kl_div_from_uniform(s2)
  )
})

test_that("invariance to magnitude", {
  v1 <- c(0.2, 0.4, 0.3, 0.1)
  v2 <- v1 * 2
  s1 <- .milne_pc_spectrum(v1)
  s2 <- .milne_pc_spectrum(v2)
  expect_equal(
    kl_div_from_uniform(s1),
    kl_div_from_uniform(s2)
  )
})

test_that("Peak measure produces different patterns to KL measure", {
  organised_sub_peaks <- c(0, 0, 3, 0, 0, 5, 0, 0, 0, 0) %>% .milne_pc_spectrum()
  disorganised_sub_peaks <- c(1, 1, 1, 0, 0, 5, 0, 0, 0, 0) %>% .milne_pc_spectrum()
  expect_equal(
    peak(organised_sub_peaks),
    peak(disorganised_sub_peaks)
  )
  expect_gt(
    kl_div_from_uniform(organised_sub_peaks),
    kl_div_from_uniform(disorganised_sub_peaks)
  )
})

context("test-peak")

library(hrep)
library(magrittr)

test_that("example", {
  c(0, 0, 3, 4, 1) %>%
    .milne_pc_spectrum() %>%
    peak %>%
    expect_equal(4)
})

test_that("invariance to doubling the number of bins", {
  for (i in 1:10) {
    spec1 <- rnorm(10) %>% .milne_pc_spectrum()
    spec2 <- rep(as.numeric(spec1), each = 2) %>% .milne_pc_spectrum()
    expect_equal(
      peak(spec1),
      peak(spec2)
    )
  }
})

test_that("more pointy distributions get higher peaks", {
  x <- seq(from = -100, to = 100)
  pointy <- dnorm(x, sd = 1)
  less_pointy <- dnorm(x, sd = 4)
  expect_gt(
    peak(.milne_pc_spectrum(pointy)),
    peak(.milne_pc_spectrum(less_pointy))
  )
})
