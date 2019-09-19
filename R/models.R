#' Simultaneous consonance models
#'
#' This \code{\link[tibble]{tibble}} summarises the
#' consonance models available in the \code{incon} package.
#' * \code{label}: Label for the model, used by \code{\link{incon}}.
#' * \code{citation}: Citation for the model.
#' * \code{class}: Organises the models by psychological theory.
#' * \code{package}: Identifies the package in which the model is implemented.
#' * \code{consonance}:
#' \code{TRUE} if the model should positively correlate with consonance,
#' \code{FALSE} if the model should negatively correlate with consonance.
#' * \code{spectrum_sensitive}: Whether the model is sensitive to
#' the spectral characteristics of the input,
#' in particular the number of harmonics
#' and the roll-off rate.
#' * \code{continuous_pitch}: Whether the model can take continuous pitch inputs.
#' * \code{f}: Function to call the model.
#' @docType data
#' @keywords data
#' @md
#' @export
incon_models <- list()

#' List models
#'
#' Lists the consonance/dissonance models that can be selected in
#' \code{\link{incon}},
#' @return A character vector of consonance/dissonance models.
#' @export
list_models <- function() {
  incon_models$label
}

add_model <- function(label,
                      citation,
                      class,
                      package,
                      consonance,
                      spectrum_sensitive,
                      continuous_pitch,
                      f) {
  checkmate::qassert(class, "S1")
  checkmate::qassert(citation, "S1")
  checkmate::qassert(package, "S1")
  checkmate::qassert(spectrum_sensitive, "B1")
  checkmate::qassert(continuous_pitch, "B1")
  checkmate::qassert(consonance, "B1")
  checkmate::qassert(label, "S1")
  stopifnot(is.function(f),
            identical(methods::formalArgs(f),
                      c("x", "num_harmonics", "roll_off", "...")))
  incon_models[[length(incon_models) + 1L]] <<- tibble::tibble(
    label,
    citation,
    class,
    package,
    consonance,
    spectrum_sensitive,
    continuous_pitch,
    f = list(f)
  )
}

add_model("gill_09_harmonicity",
          "Gill & Purves (2009)",
          "Periodicity/harmonicity",
          "bowl18",
          consonance = TRUE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            bowl18::gill09_harmonicity(x, ...))

add_model("har_18_harmonicity",
          "Harrison & Pearce (2018)",
          "Periodicity/harmonicity",
          "har18",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            har18::pc_harmonicity(x,
                                  method = "kl",
                                  num_harmonics = num_harmonics,
                                  rho = roll_off * 0.75,
                                  ...))

add_model("milne_13_harmonicity",
          "Milne (2013)",
          "Periodicity/harmonicity",
          "har18",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            har18::pc_harmonicity(x,
                                  method = "peak",
                                  num_harmonics = num_harmonics,
                                  rho = roll_off * 0.75,
                                  ...))

add_model("parn_88_root_ambig",
          "Parncutt (1988)",
          "Periodicity/harmonicity",
          "parn88",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            parn88::root_ambiguity(x, ...))

add_model("parn_94_complex",
          "Parncutt & Strasburger (1994)",
          "Periodicity/harmonicity",
          "parn94",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            parn94::complex_sonor(x,
                                  num_harmonics = num_harmonics,
                                  roll_off = roll_off,
                                  ...))

add_model("stolz_15_periodicity",
          "Stolzenburg (2015)",
          "Periodicity/harmonicity",
          "stolz15",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            stolz15::smooth_log_periodicity(x, ...))

add_model("bowl_18_min_freq_dist",
          "Bowling et al. (2018)",
          "Interference",
          "bowl18",
          consonance = TRUE,
          spectrum_sensitive = FALSE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            bowl18::bowl18_min_freq_dist(x, ...))

add_model("huron_94_dyadic",
          "Huron (1994)",
          "Interference",
          "incon",
          consonance = TRUE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            huron_1994(x, ...))

add_model("hutch_78_roughness",
          "Hutchinson & Knopoff (1978)",
          "Interference",
          "dycon",
          consonance = FALSE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            dycon::roughness_hutch(x,
                                   num_harmonics = num_harmonics,
                                   roll_off = roll_off,
                                   ...))

add_model("parn_94_pure",
          "Parncutt & Strasburger (1994)",
          "Interference",
          "parn94",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            parn94::pure_sonor(x,
                               num_harmonics = num_harmonics,
                               roll_off = roll_off,
                               ...))

add_model("seth_93_roughness",
          "Sethares (1993)",
          "Interference",
          "dycon",
          consonance = FALSE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            dycon::roughness_seth(x,
                                  num_harmonics = num_harmonics,
                                  roll_off = roll_off,
                                  ...))

add_model("vass_01_roughness",
          "Vassilakis (2001)",
          "Interference",
          "dycon",
          consonance = FALSE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            dycon::roughness_vass(x,
                                  num_harmonics = num_harmonics,
                                  roll_off = roll_off,
                                  ...))

add_model("wang_13_roughness",
          "Wang et al. (2013)",
          "Interference",
          "wang13",
          consonance = FALSE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            wang13::roughness_wang(x,
                                   num_harmonics = num_harmonics,
                                   roll_off = roll_off,
                                   msg = NULL,
                                   ...))

add_model("jl_12_tonal",
          "Johnson-Laird et al. (2012)",
          "Culture",
          "jl12",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            jl12::jl_tonal_dissonance(x, ...))

add_model("har_19_corpus",
          "Harrison & Pearce (2019)",
          "Culture",
          "corpdiss",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            corpdiss::corpus_dissonance(x, ...))

add_model("parn_94_mult",
          "Parncutt & Strasburger (1994)",
          "Numerosity",
          "parn94",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            parn94::multiplicity(x,
                                 num_harmonics = num_harmonics,
                                 roll_off = roll_off,
                                 ...))

add_model("har_19_composite",
          "Harrison & Pearce (2019)",
          "Composite",
          "incon",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            har_19_composite(x,
                             num_harmonics = num_harmonics,
                             roll_off = roll_off,
                             ...))

incon_models <- dplyr::bind_rows(incon_models)
stopifnot(!anyDuplicated(c("any", incon_models$label)))
