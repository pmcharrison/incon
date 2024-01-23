context("hutch")

library(magrittr)

test_that(
  "hutch_g", {
    expect_equal(
      hutch_g(
        1, cbw_cut_off = 1.2
      ) %>% round(digits = 4),
      0.0397
    )
    expect_equal(
      hutch_g(
        0.5, cbw_cut_off = 1.2
      ) %>% round(digits = 4),
      0.5413
    )
    expect_equal(
      hutch_g(
        1.5, cbw_cut_off = NULL
      ) %>% round(digits = 4),
      0.0016
    )
    expect_equal(
      hutch_g(
        1.5, cbw_cut_off = 1.2
      ) %>% round(digits = 4),
      0
    )
  }
)

test_that(
  "hutch_cbw", {
    expect_equal(
      hutch_cbw(400, 440) %>% round(digits = 3),
      87.225
    )
    expect_equal(
      hutch_cbw(400, 380) %>% round(digits = 3),
      83.123
    )
  }
)

test_that("get_roughness_hutch", {
  test_midi <- function(midi, expect, num_harmonics, tolerance = 1e-3) {
    midi %>%
      roughness_hutch(num_harmonics = num_harmonics) %>%
      expect_equal(expect, tolerance = tolerance)
  }
  test_midi("60 61", 0.499, num_harmonics = 1)
  test_midi("69 70", 0.491, num_harmonics = 1)
  test_midi("60 61", 0.484, num_harmonics = 11)
  test_midi("60 64 67", 0.120, num_harmonics = 11)
  test_midi("60 63 67", 0.130, num_harmonics = 11)

  expect_equal(
    roughness_hutch(c(60, 61), dissonance_function = function(...) 0),
    0
  )
})

context("test-sethares")

test_that("Regression tests generated from Sethares's implementation", {
  # See below for MATLAB implementation of Sethares's (1993) model,
  # sourced from http://sethares.engr.wisc.edu/comprog.html.
  # To reproduce the exact results, it would be necessary
  # to adjust the parameters slightly: s1 = 0.0207, s2 = 18.96, a = 3.51
  # Note that Sethares's implementation also introduces an arbitrary
  # scaling factor, which we need to compensate for in our testing.
  f <- function(frequency, amplitude, ref = TRUE) {
    x <- roughness_seth(hrep::sparse_fr_spectrum(list(frequency, amplitude)))
    if (ref) {
      x / f(frequency = c(440, 460), amplitude = c(1, 1), ref = FALSE)
    } else x
  }

  # MATLAB:
  # dissmeasure([440, 460, 480], [1, 1, 1]) / dissmeasure([440, 460], [1, 1])
  expect_equal(f(c(440, 460, 480), c(1, 1, 1)), 2.9194, tolerance = 1e-2)

  expect_equal(f(c(440, 460, 480), c(1, 2, 3)), 3.9161, tolerance = 1e-2)
  expect_equal(f(c(440, 460, 480), c(3, 2, 1)), 3.9194, tolerance = 1e-2)
  expect_equal(f(c(300, 250, 275, 425), c(1.5, 2, 9, 4)), 4.8657, tolerance = 1e-2)
})

# Sethares's MATLAB code:
# http://sethares.engr.wisc.edu/comprog.html

# function d=dissmeasure(fvec,amp)
# %
# % given a set of partials in fvec,
# % with amplitudes in amp,
# % this routine calculates the dissonance
# %
# Dstar=0.24; S1=0.0207; S2=18.96; C1=5; C2=-5;
# A1=-3.51; A2=-5.75; firstpass=1;
# N=length(fvec);
# [fvec,ind]=sort(fvec);
# ams=amp(ind);
# D=0;
# for i=2:N
# Fmin=fvec(1:N-i+1);
# S=Dstar./(S1*Fmin+S2);
# Fdif=fvec(i:N)-fvec(1:N-i+1);
# a=min(ams(i:N),ams(1:N-i+1));
# Dnew=a.*(C1*exp(A1*S.*Fdif)+C2*exp(A2*S.*Fdif));
# D=D+Dnew*ones(size(Dnew))';
# end
# d=D;

context("test-vass")

library(magrittr)
library(tibble)

test_that("Comparing model outputs to Vassilakis (2001, p. 210)", {
  # This table comes from p. 208
  .f <- tribble(
    ~ f1, ~ f2, ~ f3, ~ f4, ~ f5, ~ f6,
    262,  526,  790,  1049, 1318, 1573,
    277,  554,  837,  1118, 1398, 1677,
    294,  590,  886,  1180, 1473, 1772,
    311,  624,  932,  1244, 1569, 1873,
    330,  663,  995,  1323, 1654, 1994,
    349,  701,  1053, 1408, 1751, 2107,
    370,  741,  1118, 1482, 1852, 2235,
    392,  783,  1179, 1570, 1973, 2373,
    415,  834,  1250, 1670, 2093, 2499,
    440,  884,  1329, 1768, 2200, 2666,
    466,  937,  1400, 1874, 2345, 2799,
    494,  990,  1484, 1985, 2476, 2973,
    524,  1052, 1573, 2110, 2634, 3154
  )
  .a <- 1 / 1:6
  get_tone <- function(pc) {
    hrep::sparse_fr_spectrum(list(as.numeric(.f[pc + 1, ]), .a))
  }
  get_dyad <- function(pc_1, pc_2) {
    hrep::combine_sparse_spectra(get_tone(pc_1),
                                 get_tone(pc_2))
  }
  get_dyad_roughness <- function(pc_1, pc_2) {
    get_dyad(pc_1, pc_2) %>%
      {roughness_vass(list(hrep::freq(.), hrep::amp(.)))}
  }

  # These results come from p. 210
  res <- tibble(int = 2:12,
                old = 40.383 /
                  c(27.617, 18.117, 16.002,
                    11.446, 12.826, 6.17877,
                    10.103, 5.782, 6.214,
                    6.996, 1.589))
  res$new <- vapply(res$int, function(x) {
    get_dyad_roughness(0, 1) / get_dyad_roughness(0, x)
  }, numeric(1))
  res

  expect_gt(cor(res$old, res$new), 0.998)
})

test_that("zero amplitudes", {
  # The original version sometimes returned NaN when the spectrum contained
  # amplitudes of magnitude zero
  spectrum <- hrep:::.sparse_fr_spectrum(frequency = c(400, 420, 440, 600),
                                         amplitude = c(1, 0, 0, 1))
  expect_false(is.na(roughness_vass(spectrum)))
})
