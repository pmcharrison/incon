
# incon - Computational Models of Instantaneous Consonance

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

`incon` is an R package that implements various computational models of
instantaneous consonance perception.

## Authors

This package, and the supporting packages listed in *Packages*, were
created by Peter M. C. Harrison as part of a research project supervised
by Marcus T. Pearce and Emmanouil Benetos.

## Installation

You can install the current version of `incon` from Github by entering
the following commands into R:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("incon")
```

## Usage

The primary function is `incon`, which applies consonance models to an
input chord. The default model is that of Hutchinson & Knopoff (1978):

    #> hutch_78_roughness 
    #>          0.1202426

You can specify a vector of models and these will applied in turn.

    #> hutch_78_roughness       parn_94_pure    huron_94_dyadic 
    #>          0.1300830          0.6368813          2.2200000

See *Models* for a list of available models. See the package’s inbuilt
documentation, `?incon`, for further details.

## Models

Currently the following models are
implemented:

| Label                     | Citation                      | Class                 | Package  |
| :------------------------ | :---------------------------- | :-------------------- | :------- |
| gill\_09\_harmonicity     | Gill & Purves (2009)          | Periodicity           | bowl18   |
| har\_18\_harmonicity      | Harrison & Pearce (2018)      | Periodicity           | har18    |
| milne\_13\_harmonicity    | Milne (2013)                  | Periodicity           | har18    |
| parn\_88\_root\_ambig     | Parncutt (1988)               | Periodicity           | parn88   |
| parn\_94\_complex         | Parncutt & Strasburger (1994) | Periodicity           | parn94   |
| stolz\_15\_periodicity    | Stolzenburg (2015)            | Periodicity           | stolz15  |
| bowl\_18\_min\_freq\_dist | Bowling et al. (2018)         | Spectral interference | bowl18   |
| huron\_94\_dyadic         | Huron (1994)                  | Spectral interference | incon    |
| hutch\_78\_roughness      | Hutchinson & Knopoff (1978)   | Spectral interference | dycon    |
| parn\_94\_pure            | Parncutt & Strasburger (1994) | Spectral interference | parn94   |
| seth\_93\_roughness       | Sethares (1993)               | Spectral interference | dycon    |
| vass\_01\_roughness       | Vassilakis (2001)             | Spectral interference | dycon    |
| wang\_13\_roughness       | Wang et al. (2013)            | Spectral interference | wang13   |
| jl\_12\_tonal             | Johnson-Laird et al. (2012)   | Culture               | jl12     |
| har\_19\_corpus           | Harrison & Pearce (in prep.)  | Culture               | corpdiss |
| parn\_94\_mult            | Parncutt & Strasburger (1994) | Numerosity            | parn94   |
| har\_19\_composite        | Harrison & Pearce (2019)      | Composite             | incon    |

See `?incon` for more details.

## Packages

The functionality of `incon` is split between several low-level R
packages, listed below.

| Package  | DOI | GitHub                                    |
| :------- | :-- | :---------------------------------------- |
| bowl18   | NA  | <https://github.com/pmcharrison/bowl18>   |
| corpdiss | NA  | <https://github.com/pmcharrison/corpdiss> |
| dycon    | NA  | <https://github.com/pmcharrison/dycon>    |
| har18    | NA  | <https://github.com/pmcharrison/har18>    |
| hcorp    | NA  | <https://github.com/pmcharrison/hcorp>    |
| hrep     | NA  | <https://github.com/pmcharrison/hrep>     |
| jl12     | NA  | <https://github.com/pmcharrison/jl12>     |
| parn88   | NA  | <https://github.com/pmcharrison/parn88>   |
| parn94   | NA  | <https://github.com/pmcharrison/parn94>   |
| stolz15  | NA  | <https://github.com/pmcharrison/stolz15>  |
| wang13   | NA  | <https://github.com/pmcharrison/wang13>   |
