context("test-par")

test_that("Gill & Purves example", {
  a <- bowl18::gill09_harmonicity(c(0, 4, 7))
  b <- bowl18::gill09_harmonicity(c(0, 4, 7), tonic = 3)
  expect_gt(a, b)
  incon(c(0, 4, 7), model = "gill_09_harmonicity") %>% as.numeric %>% expect_equal(a)
  incon(c(0, 4, 7),
        model = "gill_09_harmonicity",
        par = list(gill_09_harmonicity = list(tonic = 3))) %>%
    as.numeric %>% expect_equal(b)
})
