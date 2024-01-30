context("main")

library(magrittr)

# test_that("combined model", {
#   # Test against the original paper
#   df <- c("johnson-laird-2012-data/figure-2.csv",
#           "johnson-laird-2012-data/figure-3.csv") %>%
#     system.file(package = "incon") %>%
#     lapply(function(x) read.csv(x, stringsAsFactors = FALSE)) %>%
#     do.call(rbind, .)
#   df$midi <- I(lapply(strsplit(df$midi, " "), as.numeric))
#   df$pc_set <- I(lapply(df$midi, function(x) {
#     stopifnot(all.equal(x, sort(x)))
#     res <- sort(x %% 12)
#     stopifnot(!anyDuplicated(res))
#     res
#   }))
#   df$rule_1 <- sapply(df$pc_set, jl_rule_1)
#   df$rule_2 <- sapply(df$pc_set, jl_rule_2)
#   df$rule_3 <- sapply(df$pc_set, jl_rule_3)
#   df$dual_process_2 <- sapply(df$pc_set, jl_tonal_dissonance)
#   # expect_equal(df$dual_process, df$dual_process_2)
#   # Seems there are mistakes in the original paper.
#   ## Fig 2:
#   # Here are the implied classifications in the paper:
#   # 1: consonant according to all rules
#   # 2-9: consonant by R1, dissonant by rule 2, consonant by R3
#   # 10-15: consonant by R1, dissononant by R2, dissonant by R3
#   # 16: medium by R1, dissonant by R2, consonant by R3
#   # 17-18: medium by R1, dissonant by R2, consonant by R3 (?) same as previous category
#   # 19: dissonant by all rules
#   ## Here are some example problems with the paper's annotations:
#   # chord 6 cannot be built from stacked thirds, whereas chord 7 can.
#   # => chord 6 should be in a different category to chord 7
#   # chord 11 can be bult by stacking thirds, whereas chord 10 can't
#   # => chord 11 should come in a different category to chord 10
#   # chord 16 is not in the same category as chords 17-18, yet they all
#   # have the same properties:
#   # - from minor scale (R1)
#   # - neither contain a major triad (R2)
#   # - each can be built from thirds
#   ## Similar problems exist for the Figure 3.
#   # Conclusion: the original paper doesn't provide useful
#   # regression tests.
# })

test_that("major scales", {
  expect_equal(major_scales$`2`,
               c(1, 2, 4, 6, 7, 9, 11))
  expect_equal(major_scales$`7`,
               c(0, 2, 4, 6, 7, 9, 11))
  expect_equal(minor_scales$`0`,
               c(0, 2, 3, 5, 7, 8, 11))
  expect_equal(minor_scales$`9`,
               c(0, 2, 4, 5, 8, 9, 11))
})

test_that("rule 1", {
  expect_equal(jl_rule_1(c(0, 4, 7)), 1)
  expect_equal(jl_rule_1(c(0, 3, 7)), 1)
  expect_equal(jl_rule_1(c(3, 7, 10)), 1)
  expect_equal(jl_rule_1(c(0, 4, 9)), 1)
  expect_equal(jl_rule_1(c(1, 2, 6, 9)), 1)
  expect_equal(jl_rule_1(c(0, 2, 5, 9)), 1)
  expect_equal(jl_rule_1(c(2, 5, 7, 11)), 1)
  expect_equal(jl_rule_1(c(2, 5, 9, 11)), 1)
  expect_equal(jl_rule_1(c(0, 4, 8, 9)), 2)
  expect_equal(jl_rule_1(c(0, 4, 8, 11)), 2)
  expect_equal(jl_rule_1(c(2, 5, 8, 11)), 2)
  expect_equal(jl_rule_1(c(3, 5, 9, 11)), 3)
  expect_equal(jl_rule_1(c(0, 1, 2, 3, 4)), 3)
})

test_that("rule 2", {
  expect_false(jl_rule_2(c(2, 6, 9)))
  expect_true(jl_rule_2(c(2, 5, 9)))
  expect_false(jl_rule_2(c(2, 5, 7, 11)))
  expect_true(jl_rule_2(c(2, 3, 7, 11)))
})

test_that("rule 3", {
  expect_false(jl_rule_3(c(0, 4, 9)))
  expect_false(jl_rule_3(c(0, 4, 11)))
  expect_true(jl_rule_3(c(0, 2, 7)))
})
