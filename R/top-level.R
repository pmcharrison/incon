#' Simultaneous consonance
#'
#' Computes the simultaneous consonance (or dissonance) of a sonority
#' according to various computational models.
#'
#' @param x Chord to analyse.
#' This will be coerced to an object of class \code{\link[hrep]{pi_chord}}.
#' * Numeric vectors will be interpreted as vectors of MIDI note numbers.
#' * Character scalars of the form \code{"60 64 67"} are also accepted;
#' these will likewise be interpreted as MIDI note numbers.
#'
#' @param model (Character vector, default = "hutch_78_roughness")
#' Computational model(s) to apply.
#' See \strong{Details} for available values.
#' Alternatively, \code{"all"} will select all models.
#'
#' @param x_int
#' While some models accept continuous pitch values as inputs,
#' others do not (see \code{\link{incon_models}} for details).
#' Correspondingly, the user may specify one chord (\code{x})
#' to deliver to continuous-pitch models,
#' and another chord (\code{x_int}) to deliver to non-continuous-pitch models.
#' If \code{x_int} is \code{NULL},
#' then non-continuous-pitch models will default to analysing \code{x},
#' and will throw an error if \code{x} is non-integer.
#'
#' @param num_harmonics (Integerish scalar, default = 11)
#' Number of harmonics to which chord tones should be expanded,
#' including the fundamental frequency.
#' This only affects a subset of the consonance models.
#'
#' @param roll_off (Integerish scalar, default = 1)
#' Roll-off rate for the upper harmonics in complex tones.
#' If the roll-off rate is \eqn{x},
#' then the amplitude of the ith harmonic is equal to \eqn{1 ^ -x}.
#'
#' @param par (List, default = \code{list()})
#' Optional list containing additional parameters to pass to specific models.
#' If non-empty, this list should be a named list of lists,
#' with each name identifying the model to which additional parameters
#' should be passed (see \code{\link{list_models}}) for legal names.
#' Each element should itself be a named list of parameters,
#' with the names identifying the arguments,
#' and the values being those to be passed to the model function.
#'
#' @return
#' A named numeric vector comprising the outputs of each computational model
#' in the order that they were specified.
#'
#' @examples
#' incon(c(60, 64, 67))
#' incon("60 64 67")
#' incon("60 64 67", model = "hutch_78_roughness")
#' incon("60 64 67", model = c("hutch_78_roughness", "parn_88_root_ambig"))
#' incon("0 4 7", model = "gill_09_harmonicity",
#'       par = list(gill_09_harmonicity = list(tonic = 3)))
#'
#' @details
#' The following models are available:
#' * `gill_09_harmonicity`:
#' the harmonicity model of \insertCite{Gill2009;textual}{bowl18}
#' (see \code{bowl18::\link[bowl18]{gill09_harmonicity}}).
#' * `har_18_harmonicity`:
#' the harmonicity model of \insertCite{Harrison2018;textual}{har18}
#' (see \code{har18::\link[har18]{pc_harmonicity}}).
#' * `milne_13_harmonicity`:
#' the harmonicity model of \insertCite{Milne2013;textual}{har18}
#' (see \code{har18::\link[har18]{pc_harmonicity}}).
#' * `parn_88_root_ambig`:
#' the root ambiguity model of \insertCite{Parncutt1988;textual}{parn88}
#' (see \code{parn88::\link[parn88]{root_ambiguity}}).
#' * `parn_94_complex`:
#' the complex sonorousness feature of \insertCite{Parncutt1994;textual}{parn94}
#' (see \code{parn94::\link[parn94]{complex_sonor}}).
#' * `stolz_15_periodicity`:
#' smoothed logarithmic periodicity,
#' after \insertCite{Stolzenburg2015;textual}{stolz15}
#' (see \code{stolz15::\link[stolz15]{smooth_log_periodicity}}).
#' * `bowl_18_min_freq_dist`:
#' the minimum frequency distance feature of
#' \insertCite{Bowling2018;textual}{bowl18}
#' (see \code{bowl18::\link[bowl18]{bowl18_min_freq_dist}}).
#' * `huron_94_dyadic`:
#' aggregate dyadic consonance, after \insertCite{Huron1994;textual}{incon}.
#' * `hutch_78_roughness`:
#' the roughness model of \insertCite{Hutchinson1978;textual}{dycon}
#' (see \code{dycon::\link[dycon]{roughness_hutch}}).
#' * `parn_94_pure`:
#' the complex sonorousness feature of \insertCite{Parncutt1994;textual}{parn94}
#' (see \code{parn94::\link[parn94]{pure_sonor}}).
#' * `seth_93_roughness`:
#' the roughness model of \insertCite{Sethares1993;textual}{dycon}
#' (see \code{dycon::\link[dycon]{roughness_seth}}).
#' * `vass_01_roughness`:
#' the roughness model of \insertCite{Vassilakis2001;textual}{dycon}
#' (see \code{dycon::\link[dycon]{roughness_vass}}).
#' * `wang_13_roughness`:
#' the roughness model of \insertCite{Wang2013;textual}{wang13}
#' (see \code{wang13::\link[wang13]{roughness_wang}}).
#' * `jl_12_tonal`:
#' the tonal dissonance model of \insertCite{Johnson-Laird2012;textual}{jl12}
#' (see \code{jl12::\link[jl12]{jl_tonal_dissonance}}).
#' * `har_19_corpus`:
#' a corpus-based model of cultural familiarity
#' \insertCite{Harrison2019}{incon}
#' (see \code{corpdiss::\link[corpdiss]{corpus_dissonance}}).
#' * `parn_94_mult`:
#' the multiplicity feature of \insertCite{Parncutt1994;textual}{parn94}
#' (see \code{parn94::\link[parn94]{multiplicity}}).
#' * `har_19_composite`:
#' a model combining interference \insertCite{Hutchinson1978}{dycon},
#' periodicity/harmonicity \insertCite{Harrison2018}{har18},
#' and cultural familiarity,
#' as introduced by \insertCite{Harrison2019;textual}{incon}.
#' @references
#' \insertAllCited{}
#' @md
#' @rdname incon
#' @export
incon <- function(x,
                  model = "hutch_78_roughness",
                  x_int = NULL,
                  num_harmonics = 11L,
                  roll_off = 1,
                  par = list()) {
  c(x, model, x_int, par) %<-% fix_inputs(x, model, x_int, par)
  purrr::map_dbl(model, apply_model, x, x_int,
                 num_harmonics, roll_off, par) %>%
    magrittr::set_names(model)
}

fix_inputs <- function(x, model, x_int, par) {
  check_inputs_pre(x, model, x_int, par)
  c(x, model, x_int) %<-% rewrite_inputs(x, model, x_int)
  check_inputs_post(x, model, x_int, par)
  list(x, model, x_int, par)
}

rewrite_inputs <- function(x, model, x_int) {
  x <- hrep::pi_chord(x)
  if (!is.null(x_int)) x_int <- hrep::pi_chord(x_int)
  if ("all" %in% model) model <- incon_models$label
  list(x, model, x_int)
}

check_inputs_pre <- function(x, model, x_int, par) {
  checkmate::qassert(model, "S")
  if (anyDuplicated(model)) stop("duplicated models detected")
  if (!all(model %in% c("all", incon_models$label))) stop("invalid model argument")
  if (!is.list(par)) stop("'par' must be a list")
  if (length(par) > 0L && is.null(names(par))) stop("'par' must be named")
  if (anyDuplicated(names(par))) stop("'par' must not possess duplicate names")
  for (elt in par)
    if (!is.named_list(elt)) stop("every element of 'par' should be a named list")
}

is.named_list <- function(x) {
  is.list(x) && (length(x) == 0L || !is.null(names(x)))
}

check_inputs_post <- function(x, model, x_int, par) {
  if (!is.null(x_int)) checkmate::qassert(x_int, "X+")
  if (!all(names(par) %in% model))
    stop("tried to pass parameters to an unselected models")
}

apply_model <- function(model_label, x, x_int, num_harmonics, roll_off, par) {
  model <- get_model(model_label)
  chord <- get_input(model, x, x_int)
  p <- par[[model_label]]
  args <- c(
    list(x = chord, num_harmonics = num_harmonics, roll_off = roll_off),
    p
  )
  do.call(model$f, args)
}

get_input <- function(model, x, x_int) {
  if (model$continuous_pitch) {
    x
  } else if (!is.null(x_int)) {
    x_int
  } else if (hrep::is.equal_tempered(x)) {
    x
  } else {
    stop(model$label, " requires integer inputs ",
         "(see 'x_int' parameter in '?incon')")
  }
}
