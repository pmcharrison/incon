
# incon - Computational Models of Simultaneous Consonance

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/pmcharrison/incon.svg?branch=master)](https://travis-ci.org/pmcharrison/incon)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/incon?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/incon)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2545766.svg)](https://doi.org/10.5281/zenodo.2545766)

`incon` is an R package that implements various computational models of
simultaneous consonance perception.

## Citation

Harrison, P. M. C., & Pearce, M. T. (2019). Instantaneous consonance in
the perception and composition of Western music. *PsyArXiv*.
<https://doi.org/10.31234/osf.io/6jsug>

## Installation

You can install the current version of `incon` from Github by entering
the following commands into R:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("pmcharrison/incon")
```

## Usage

The primary function is `incon`, which applies consonance models to an
input chord. The default model is that of Hutchinson & Knopoff (1978):

``` r
library(incon)

chord <- c(60, 64, 67) # major triad, MIDI note numbers
incon(chord)
#> hutch_78_roughness 
#>          0.1202426
```

You can specify a vector of models and these will applied in turn.

``` r
chord <- c(60, 63, 67) # minor triad
models <- c("hutch_78_roughness", 
            "parn_94_pure",
            "huron_94_dyadic")
incon(c(60, 63, 67), models)
#> hutch_78_roughness       parn_94_pure    huron_94_dyadic 
#>          0.1300830          0.6368813          2.2200000
```

See *Models* for a list of available models. See the package’s inbuilt
documentation, `?incon`, for further details.

## Models

Currently the following models are implemented:

| Label                 | Citation                      | Class                   | Package |
|:----------------------|:------------------------------|:------------------------|:--------|
| gill_09_harmonicity   | Gill & Purves (2009)          | Periodicity/harmonicity | incon   |
| har_18_harmonicity    | Harrison & Pearce (2018)      | Periodicity/harmonicity | incon   |
| milne_13_harmonicity  | Milne (2013)                  | Periodicity/harmonicity | incon   |
| parn_88_root_ambig    | Parncutt (1988)               | Periodicity/harmonicity | incon   |
| parn_94_complex       | Parncutt & Strasburger (1994) | Periodicity/harmonicity | incon   |
| stolz_15_periodicity  | Stolzenburg (2015)            | Periodicity/harmonicity | incon   |
| bowl_18_min_freq_dist | Bowling et al. (2018)         | Interference            | incon   |
| huron_94_dyadic       | Huron (1994)                  | Interference            | incon   |
| hutch_78_roughness    | Hutchinson & Knopoff (1978)   | Interference            | incon   |
| parn_94_pure          | Parncutt & Strasburger (1994) | Interference            | incon   |
| seth_93_roughness     | Sethares (1993)               | Interference            | incon   |
| vass_01_roughness     | Vassilakis (2001)             | Interference            | incon   |
| wang_13_roughness     | Wang et al. (2013)            | Interference            | incon   |
| jl_12_tonal           | Johnson-Laird et al. (2012)   | Culture                 | incon   |
| har_19_corpus         | Harrison & Pearce (2019)      | Culture                 | incon   |
| parn_94_mult          | Parncutt & Strasburger (1994) | Numerosity              | incon   |
| har_19_composite      | Harrison & Pearce (2019)      | Composite               | incon   |

See `?incon` for more details.

## Spectral representations

Certain `incon` models can be applied to full frequency spectra rather
than just symbolically notated chords. One example is the set of
interference models provided in the `dycon` package. In order to run
such models on full frequency spectra one must call lower-level
functions explicitly, as in the following example, which computes the
roughness of a chord using the Hutchinson-Knopoff dissonance model:

``` r
spectrum <- 
  hrep::sparse_fr_spectrum(list(
    frequency = c(400, 800, 1200, 1250),
    amplitude = c(1, 0.7, 0.9, 0.6)
  ))

roughness_hutch(spectrum)
```

## References

Bowling, D. L., Purves, D., & Gill, K. Z. (2018). Vocal similarity
predicts the relative attraction of musical chords. Proceedings of the
National Academy of Sciences, 115(1), 216–221.
<https://doi.org/10.1073/pnas.1713206115>

Gill, K. Z., & Purves, D. (2009). A biological rationale for musical
scales. PLoS ONE, 4(12). <https://doi.org/10.1371/journal.pone.0008144>

Harrison, P. M. C., & Pearce, M. T. (2018). An energy-based generative
sequence model for testing sensory theories of Western harmony. In
Proceedings of the 19th International Society for Music Information
Retrieval Conference (pp. 160–167). Paris, France.

Harrison, P. M. C., & Pearce, M. T. (2019). Instantaneous consonance in
the perception and composition of Western music. *PsyArxiv*.
<https://doi.org/10.31234/osf.io/6jsug>

Huron, D. (1994). Interval-class content in equally tempered pitch-class
sets: Common scales exhibit optimum tonal consonance. Music Perception,
11(3), 289–305. <https://doi.org/10.2307/40285624>

Hutchinson, W., & Knopoff, L. (1978). The acoustic component of Western
consonance. Journal of New Music Research, 7(1), 1–29.
<https://doi.org/10.1080/09298217808570246>

Johnson-Laird, P. N., Kang, O. E., & Leong, Y. C. (2012). On musical
dissonance. Music Perception, 30(1), 19–35.

Milne, A. J. (2013). A computational model of the cognition of tonality.
The Open University, Milton Keynes, England.

Parncutt, R. (1988). Revision of Terhardt’s psychoacoustical model of
the root(s) of a musical chord. Music Perception, 6(1), 65–93.

Parncutt, R., & Strasburger, H. (1994). Applying psychoacoustics in
composition: “Harmonic” progressions of “nonharmonic” sonorities.
Perspectives of New Music, 32(2), 88–129.

Sethares, W. A. (1993). Local consonance and the relationship between
timbre and scale. The Journal of the Acoustical Society of America,
94(3), 1218–1228.

Stolzenburg, F. (2015). Harmony perception by periodicity detection.
Journal of Mathematics and Music, 9(3), 215–238.
<https://doi.org/10.1080/17459737.2015.1033024>

Vassilakis, P. N. (2001). Perceptual and physical properties of
amplitude fluctuation and their musical significance. University of
California, Los Angeles, CA.

Wang, Y. S., Shen, G. Q., Guo, H., Tang, X. L., & Hamade, T. (2013).
Roughness modelling based on human auditory perception for sound quality
evaluation of vehicle interior noise. Journal of Sound and Vibration,
332(16), 3893–3904. <https://doi.org/10.1016/j.jsv.2013.02.030>
