# This script compiles data for Bigand et al. (1996),
# which can subsequently be used for regression testing.

# Bigand, E., Parncutt, R., & Lerdahl, F. (1996).
# Perception of musical tension in short chord sequences:
# The influence of harmonic function, sensory dissonance, horizontal motion,
# and musical training. Perception & Psychophysics, 58(1), 124–141.
# https://doi.org/10.3758/BF03205482

library(magrittr)

convert_chord_type_to_pc_set <- function(chord_type) {
  assertthat::assert_that(
    assertthat::is.scalar(chord_type),
    is.character(chord_type) || is.factor(chord_type)
  )
  if (chord_type == "diminished") {
    c(0, 3, 6)
  } else if (chord_type == "major") {
    c(0, 4, 7)
  } else if (chord_type == "minor") {
    c(0, 3, 7)
  } else if (chord_type == "minor_seventh") {
    c(0, 3, 7, 10)
  } else if (chord_type == "revoiced") {
    c(0, 4, 7)
  } else if (chord_type == "seventh") {
    c(0, 4, 7, 10)
  } else stop("Unrecognised chord type")
}

deduce_chord_pitches <- function(
  pc_set,
  bass_interval_size, tenor_interval_size,
  alto_interval_size, soprano_interval_size,
  previous_chord_pitches
) {
  # Known constraints:
  # - bass always rises
  # - soprano always falls
  bass_pitch <- previous_chord_pitches$bass + bass_interval_size
  soprano_pitch <- previous_chord_pitches$soprano - soprano_interval_size
  bass_pc <- bass_pitch %% 12
  soprano_pc <- soprano_pitch %% 12
  # Check all variants of alto/tenor movement and work out which satisfy the constraints
  df <- expand.grid(tenor_ascends = c(FALSE, TRUE),
                    alto_ascends = c(FALSE, TRUE))
  df$tenor_pitch <- ifelse(df$tenor_ascends,
                           previous_chord_pitches$tenor + tenor_interval_size,
                           previous_chord_pitches$tenor - tenor_interval_size)
  df$alto_pitch <- ifelse(df$alto_ascends,
                          previous_chord_pitches$alto + alto_interval_size,
                          previous_chord_pitches$alto - alto_interval_size)
  df$tenor_pc <- df$tenor_pitch %% 12
  df$alto_pc <- df$alto_pitch %% 12
  df$num_repeated_pitches <- mapply(
    function(tenor_pitch, alto_pitch) {
      4 - length(unique(c(bass_pitch, tenor_pitch, alto_pitch, soprano_pitch)))
    }, df$tenor_pitch, df$alto_pitch
  )
  df$num_unique_pcs <- mapply(
    function(tenor_pc, alto_pc) {
      length(unique(c(bass_pc, tenor_pc, alto_pc, soprano_pc)))
    },
    df$tenor_pc, df$alto_pc
  )
  df$valid <- mapply(
    function(tenor_pc, alto_pc, tenor_pitch, alto_pitch) {
      # Must not contain extraneous pitch classes
      all(
        c(bass_pc, tenor_pc, alto_pc, soprano_pc) %in% pc_set
      ) &&
      # Don't allow part crossing
        bass_pitch <= tenor_pitch &&
        tenor_pitch <= alto_pitch &&
        alto_pitch <= soprano_pitch &&
        # Don't allow the third to be doubled.
        # The bass is always the root, so the third is 3 or 4 semitones
        # above the bass
        sum(
          c(bass_pc, tenor_pc, alto_pc, soprano_pc) %in%
            c((bass_pc + 3) %% 12, (bass_pitch + 4) %% 12)
        ) < 2 &&
        # The seventh cannot be omitted in seventh chords
        (
          !((bass_pc + 10) %% 12) %in% pc_set ||
            ((bass_pc + 10) %% 12) %in% c(bass_pc, tenor_pc, alto_pc, soprano_pc)
        )
    },
    df$tenor_pc, df$alto_pc, df$tenor_pitch, df$alto_pitch
  )
  # Solution must be valid
  df_chosen <- df[df$valid,
                  setdiff(names(df), c("tenor_ascends", "alto_ascends"))] %>%
    unique %>%
    # and minimise the number of repeated pitches
    (function(df) df[df$num_repeated_pitches == min(df$num_repeated_pitches), ]) %>%
    # and maximise the number of unique pitch classes
    (function(df) df[df$num_unique_pcs == max(df$num_unique_pcs), ])
  if (nrow(df_chosen) == 0) {
    message("No solutions found")
    browser()
  } else if (nrow(df_chosen) > 1) {
    message("Multiple solutions sound")
    browser()
  }
  c(bass_pitch, df_chosen$tenor_pitch,
    df_chosen$alto_pitch, soprano_pitch)
}

# Actions ####

bigand_1996 <- read.csv("inst/bigand1996/raw/bigand-1996-data.csv", stringsAsFactors = FALSE) %>%
  # Correct mistakes
  ## Incorrect tenor int. size - fixed using Fig. 1
  (function(df) {
    df$tenor_interval_size[df$label == "C#7"] <- 4
    df$tenor_interval_size[df$label == "c#7"] <- 4
    df$tenor_interval_size[df$label == "ab7"] <- 4
    df
  })

bigand_1996_reference_chord <- read.csv("inst/bigand1996/raw/bigand-1996-reference-chord.csv",
                                         stringsAsFactors = FALSE) %>% as.list

bigand_1996$root_pc <- bigand_1996$bass_interval_size # because all chords were root position
bigand_1996$pc_set <- mapply(function(root_pc, chord_type) {
  convert_chord_type_to_pc_set(chord_type) %>%
    add(root_pc) %>%
    mod(., 12) %>%
    sort
}, bigand_1996$root_pc, bigand_1996$chord_type) %>% I
bigand_1996$pitches <- mapply(
  function(pc_set,
           bass_interval_size, tenor_interval_size,
           alto_interval_size, soprano_interval_size) {
    deduce_chord_pitches(
      pc_set = pc_set,
      bass_interval_size = bass_interval_size,
      tenor_interval_size = tenor_interval_size,
      alto_interval_size = alto_interval_size,
      soprano_interval_size = soprano_interval_size,
      previous_chord_pitches = bigand_1996_reference_chord
    )
  }, bigand_1996$pc_set,
  bigand_1996$bass_interval_size, bigand_1996$tenor_interval_size,
  bigand_1996$alto_interval_size, bigand_1996$soprano_interval_size,
  SIMPLIFY = FALSE
) %>% I

saveRDS(bigand_1996, "inst/bigand1996/compiled/bigand_1996.rds")
saveRDS(bigand_1996_reference_chord, "inst/bigand1996/compiled/bigand_1996_reference_chord.rds")
