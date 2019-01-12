context("test-regression-tests")

library(magrittr)

test_that("regression tests", {
  c(60, 64, 67) %>% incon("gill_09_harmonicity") %>%
    expect_equal(0.4666667, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("har_18_harmonicity") %>%
    expect_equal(0.9285886, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("milne_13_harmonicity") %>%
    expect_equal(0.7021341, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("parn_88_root_ambig") %>%
    expect_equal(1.870829, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("parn_94_complex") %>%
    expect_equal(0.309965, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("stolz_15_periodicity") %>%
    expect_equal(2, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("bowl_18_min_freq_dist") %>%
    expect_equal(62.36788, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("huron_94_dyadic") %>%
    expect_equal(2.22, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("hutch_78_roughness") %>%
    expect_equal(0.1202426, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("parn_94_pure") %>%
    expect_equal(0.6157366, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("seth_93_roughness") %>%
    expect_equal(0.7735006, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("vass_01_roughness") %>%
    expect_equal(2.083763, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("wang_13_roughness") %>%
    expect_equal(0.6442906, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("jl_12_tonal") %>%
    expect_equal(1, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("har_19_corpus") %>%
    expect_equal(0.7802433, tolerance = 1e-5, check.names = FALSE)

  c(60, 64, 67) %>% incon("parn_94_mult") %>%
    expect_equal(2.843946, tolerance = 1e-5, check.names = FALSE)
})
