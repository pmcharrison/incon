approx2 <- function(x, y, new_x) {
  assertthat::assert_that(
    length(x) == length(y),
    length(x) > 1
  )
  y <- y[order(x)]
  x <- x[order(x)]
  n <- length(x)
  ifelse(
    new_x < x[1],
    y[1] + (new_x - x[1]) * (y[2] - y[1]) / (x[2] - x[1]),
    ifelse(
      new_x > x[n],
      y[n] + (new_x - x[n]) * (y[n] - y[n - 1]) / (x[n] - x[n - 1]),
      approx(x, y, xout = new_x)$y
    )
  )
}

compile_detail <- function(x) {
  x$spectrum_input <- data.frame(frequency_Hz = x$frequency_Hz,
                                 level_dB = x$level_dB)
  x$spectrum_after_ear_filtering <- data.frame(frequency_Hz = x$frequency_Hz,
                                               level_dB = x$level_dB_filtered)
  x[c(
    "spectrum_input",
    "spectrum_after_ear_filtering",
    "channel_wave_forms",
    "channel_envelopes",
    "filtered_channel_envelopes",
    "modulation_indices",
    "phase_impact_factors",
    "specific_roughnesses",
    "total_roughness"
  )]
}

get_channel_sound_excitation_levels <- function(frequency_Hz,
                                                level_dB_filtered) {
  frequency_bark <- convert_Hz_to_bark(frequency_Hz)
  lapply(
    seq_len(47),
    function(channel_num) {
      get_channel_sound_excitation_level(
        freq_Hz = frequency_Hz,
        freq_bark = frequency_bark,
        sound_intensity_level = level_dB_filtered,
        channel_num = channel_num)
    }
  )
}

# @param freq_Hz Frequency in Hz (numeric vector)
# @param freq_bark Corresponding frequency in barks (numeric vector)
# @param sound_intensity_level Sound intensity level (numeric vector, matches with freq_Hz)
# @param channel_num Critical band filter number (should be a scalar integer between 1 and 47)
# @return Numeric vector giving the excitation level for the respective channel for each frequency listed in \code{freq_Hz}
get_channel_sound_excitation_level <- function(freq_Hz,
                                               freq_bark,
                                               sound_intensity_level,
                                               channel_num) {
  assertthat::assert_that(
    is.numeric(freq_bark), is.numeric(sound_intensity_level),
    length(freq_bark) == length(sound_intensity_level),
    assertthat::is.scalar(channel_num), is.numeric(channel_num)
  )
  channel_centre_bark <- 0.5 * channel_num
  df <- data.frame(
    freq_Hz = freq_Hz,
    freq_bark = freq_bark,
    sound_intensity_level = sound_intensity_level,
    region = NA
  )
  # Assign components to regions
  df$region[0 <= df$freq_bark & freq_bark < channel_centre_bark - 0.5] <- "upper_slope"
  df$region[channel_centre_bark - 0.5 <= freq_bark &
              freq_bark <= channel_centre_bark + 0.5] <- "centre"
  df$region[channel_centre_bark + 0.5 < freq_bark &
              freq_bark <= 47] <- "lower_slope"
  # Calculate SELs for components within the critical band
  df$sound_excitation_level[df$region == "centre"] <-
    df[df$region == "centre", ] %>%
    (function(df) df$sound_intensity_level)
  # ... and for components below the critical band...
  df$sound_excitation_level[df$region == "upper_slope"] <-
    df[df$region == "upper_slope", ] %>%
    (function(df) {
      df$sound_intensity_level +
        ((channel_centre_bark + 0.5) - df$freq_bark) *
        (- 24 - (230 / df$freq_Hz) + 0.2 * df$sound_intensity_level)
    })
  # ... and for components above the critical band
  df$sound_excitation_level[df$region == "lower_slope"] <-
    df[df$region == "lower_slope", ] %>%
    (function(df) {
      df$sound_intensity_level +
        ((channel_centre_bark - 0.5) - df$freq_bark) * 27
    })
  df$sound_excitation_level
}

get_channel_wave_form <- function(excitation_levels,
                                  frequency_Hz) {
  # Excitation levels are in decibels, so we need to convert back
  # to a linear scale. The units for this linear scale don't matter,
  # because later on we normalise by the RMS of the waveform.
  assertthat::assert_that(
    all(frequency_Hz < 44000),
    all(frequency_Hz >= 0.5)
  )
  # This bit is inefficient for dense spectra
  # (i.e. when frequency_Hz is long)
  amplitudes <- 10 ^ (excitation_levels / 20)
  x <- rep(0, times = 44000)
  for (i in seq_along(frequency_Hz)) {
    tmp_freq <- round(frequency_Hz[i])
    tmp_amp <- amplitudes[i]
    x[tmp_freq] <- hrep::sum_amplitudes(
      x[tmp_freq],
      tmp_amp,
      coherent = FALSE
    )
  }
  Re(fft(x, inverse = TRUE) / length(x))
}

get_channel_envelope <- function(x) {
  hht::HilbertEnvelope(hht::HilbertTransform(x)) %>%
    (function(x) x - mean(x)) # center it
}

filter_channel_envelope <- function(i, channel_envelope) {
  ft <- fft(channel_envelope)
  ft.coef <- Mod(ft)
  ft.freq <- seq(from = 0, to = 44000 - 1)
  ft.freq <- ifelse(ft.freq >= 22000, # Nyquist frequency
                    44000 - ft.freq,
                    ft.freq)
  ft.coef_new <- ft.coef * envelope_weight(freq_Hz = abs(ft.freq),
                                           channel_num = i)
  waveform <- Re(fft(ft.coef_new, inverse = TRUE) / length(ft.coef_new))
  waveform <- waveform - mean(waveform)
  waveform
}

get_modulation_index <- function(filtered_channel_envelope,
                                 channel_wave_form) {
  sqrt(mean(filtered_channel_envelope ^ 2)) /
    sqrt(mean(channel_wave_form ^ 2))
}

get_phase_impact_factor <- function(i, filtered_channel_envelopes) {
  if (i == 1) {
    cor(
      filtered_channel_envelopes[[1]],
      filtered_channel_envelopes[[2]]
    ) ^ 2
  } else if (i == 47) {
    cor(
      filtered_channel_envelopes[[46]],
      filtered_channel_envelopes[[47]]
    ) ^ 2
  } else {
    cor(
      filtered_channel_envelopes[[i - 1]],
      filtered_channel_envelopes[[i]]
    ) *
      cor(
        filtered_channel_envelopes[[i]],
        filtered_channel_envelopes[[i + 1]]
      )
  }
}

get_channel_weight <- function(channel_num) {
  assertthat::assert_that(
    is.numeric(channel_num),
    all(round(channel_num) == channel_num),
    all(channel_num > 0),
    all(channel_num < 48)
  )
  approx(
    x = c(1,    5,    10,   17,   19,   24, 27,  47),
    y = c(0.41, 0.82, 0.83, 1.12, 1.12, 1,  0.8, 0.58),
    xout = channel_num
  )$y
}

# Get ear transmission coefficients
#
# Gets ear transmission coefficients according to Figure 3 of Wang et al. (2013).
# This is a linear approximation to the graph provided in the paper
# (the paper does not provide an equation for the curve, unfortunately).
# @param freq Numeric vector of frequencies
# @return Numeric vector of ear transmission coefficients.
# These coefficients describe a filter that can be applied to incoming spectra
# to simulate the filtering of the ear.
# \insertRef{Wang2013}{incon}
ear_transmission <- function(freq) {
  log_freq <- log(freq, base = 10)
  ifelse(
    log_freq < 3.1,
    0,
    ifelse(
      log_freq > 4.25,
      30 + (log_freq - 4.25) * 100,
      approx(
        x = c(3.1, 3.4, 3.5, 4, 4.25),
        y = c(0, -5, -5, 5, 30),
        method = "linear", rule = 2,
        xout = log_freq
      )$y
    )
  )
}

convert_Hz_to_bark <- function(freq_Hz) {
  freq_kHz <- freq_Hz / 1000
  ifelse(
    freq_kHz <= 1.5,
    11.82 * atan(1.21 * freq_kHz),
    5 * log(freq_kHz / 1.5) + 12.61
  )
}

get_critical_bandwidth <- function(freq_Hz) {
  ifelse(freq_Hz < 500, 100, freq_Hz / 5)
}

# Figure 5 of Wang et al.
envelope_weight <- function(freq_Hz, channel_num) {
  assertthat::assert_that(
    is.numeric(freq_Hz),
    is.numeric(channel_num), assertthat::is.scalar(channel_num),
    channel_num > 0, channel_num < 48,
    round(channel_num) == channel_num
  )
  if (channel_num < 5) {
    approx2(
      x = c(0, 20,  30, 150,   250, 350),
      y = c(0, 0.8, 1,  0.475, 0.2, 0.02),
      new_x = freq_Hz
    )
  } else if (channel_num < 16) {
    approx2(
      x = c(0, 30, 55, 150, 400),
      y = c(0, 0.8, 1, 0.675, 0.1),
      new_x = freq_Hz
    )
  } else if (channel_num < 21) {
    approx2(
      x = c(0, 50,   77, 165,  250,  400),
      y = c(0, 0.85, 1,  0.82, 0.48, 0.225),
      new_x = freq_Hz
    )
  } else if (channel_num < 42) {
    approx2(
      x = c(0, 50,  77, 100,  250,  400),
      y = c(0, 0.9, 1,  0.95, 0.48, 0.225),
      new_x = freq_Hz
    )
  } else {
    approx2(
      x = c(0, 50,   70, 85,    140,  400),
      y = c(0, 0.95, 1,  0.955, 0.69, 0.225),
      new_x = freq_Hz
    )
  }
}

get_specific_roughness <- function(channel_weight,
                                   phase_impact_factor,
                                   modulation_index,
                                   include_phase_impact_factors) {
  if (include_phase_impact_factors) {
    (channel_weight * phase_impact_factor * modulation_index) ^ 2
  } else {
    (channel_weight * modulation_index) ^ 2
  }
}

play_channel_wave_forms <- function(channel_wave_forms,
                                    channel_num,
                                    scale_to_other_channels = TRUE,
                                    ...) {
  if (!requireNamespace("tuneR"))
    stop("tuneR must be installed before continuing")
  peak <- if (scale_to_other_channels) {
    channel_wave_forms %>%
      vapply(function(x) max(abs(x)), numeric(1)) %>%
      max
  } else {
    max(abs(channel_wave_forms[[channel_num]]))
  }
  play_wave_form(x = channel_wave_forms[[channel_num]],
                 peak = peak,
                 ...)
}


play_wave_form <- function(x,
                           sample_rate = 44e3,
                           fade_samples = 1e3,
                           bit = 16,
                           peak = max(abs(x))) {
  if (!requireNamespace("tuneR"))
    stop("tuneR must be installed before continuing")

  if (length(x) > 2 * fade_samples) {
    ind_1 <- seq(from = 1, length.out = fade_samples)
    ind_2 <- seq(to = length(x), length.out = fade_samples)
    x[ind_1] <- x[ind_1] * seq(from = 0, to = 1, length.out = fade_samples)
    x[ind_2] <- x[ind_2] * seq(from = 1, to = 0, length.out = fade_samples)
  }

  u <- x %>%
    magrittr::divide_by(peak * 1.5) %>%
    magrittr::multiply_by(2 ^ (bit - 1) - 1) %>%
    round
  tuneR::play(tuneR::Wave(u, samp.rate = sample_rate, bit = bit), "play")
}


play_sparse_spectrum <- function(
    frequency,
    amplitude,
    seconds = 1,
    sample_rate = 44e3,
    ...
) {
  sparse_spectrum_to_waveform(frequency, amplitude, seconds, sample_rate) %>%
    play_wave_form(sample_rate = sample_rate, ...)
}

# Note: this could be done more efficiently with ifft
sparse_spectrum_to_waveform <- function(
    frequency,
    amplitude,
    seconds,
    sample_rate
) {
  x <- seq(from = 0, to = seconds, length.out = sample_rate * seconds)
  y <- mapply(
    function(freq, amplitude) {
      sin(2 * pi * freq * x) * amplitude
    },
    frequency,
    amplitude
  ) %>% rowSums
}

plot_modulation_indices_wang <- function(modulation_indices,
                                         theme = cowplot::theme_cowplot()) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(modulation_indices),
    length(modulation_indices) == 47
  )
  df <- data.frame(
    x = seq_along(modulation_indices),
    y = modulation_indices
  )
  ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Modulation index") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

plot_phase_impact_factors_wang <- function(phase_impact_factors,
                                           theme = cowplot::theme_cowplot()) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(phase_impact_factors),
    length(phase_impact_factors) == 47
  )
  df <- data.frame(
    x = seq_along(phase_impact_factors),
    y = phase_impact_factors
  )
  ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Phase impact factor") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

plot_specific_roughnesses_wang <- function(specific_roughnesses,
                                           theme = cowplot::theme_cowplot()) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(specific_roughnesses),
    length(specific_roughnesses) == 47
  )
  df <- data.frame(
    x = seq_along(specific_roughnesses),
    y = specific_roughnesses
  )
  ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Specific roughness") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

plot_waveform <- function(
    x,
    sample_rate = 44000,
    range_sec = c(0, 0.2),
    theme = cowplot::theme_cowplot()
) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  df <- data.frame(
    t = seq(from = 0, by = 1 / sample_rate, length.out = length(x)),
    x = x
  ) %>%
    (function(df) df[df$t >= range_sec[1] & df$t <= range_sec[2], ])
  ggplot2::ggplot(df, ggplot2::aes_string(x = "t", y = "x")) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_x_continuous("Time (sec)") +
    ggplot2::scale_y_continuous("Instantaneous amplitude") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

plot_filtered_input_spectrum <- function(analysis) {
  frequency <- analysis$spectrum_after_ear_filtering$frequency_Hz
  amplitude <- hrep::dB_to_amplitude(
    analysis$spectrum_after_ear_filtering$level_dB,
    60
  )
  plot_sparse_spectrum(frequency, amplitude, range_Hz = NULL)
}

plot_sparse_spectrum <- function(
    frequency,
    amplitude,
    resolution_Hz = 1,
    range_Hz = NULL,
    theme = cowplot::theme_cowplot()
) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  if (is.null(range_Hz)) range_Hz <- c(0, max(frequency))
  df <- data.frame(freq = round(frequency),
                   amp = amplitude) %>%
    (function(df) df[df$freq >= range_Hz[1] &
                       df$freq <= range_Hz[2], ]) %>%
    rbind(
      data.frame(freq = seq(from = range_Hz[1],
                            to = range_Hz[2],
                            by = resolution_Hz),
                 amp = 0)
    ) %>%
    (function(df) {
      df[order(df$freq), ]
    })
  ggplot2::ggplot(df, ggplot2::aes_string(x = "freq", y = "amp")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Frequency (Hz)") +
    ggplot2::scale_y_continuous("Amplitude") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

format_chord <- function(x) {
  paste(x, collapse = " ")
}

enter_new_chord <- function(text, state, num_harmonics) {
  tryCatch({
    set_chord(hrep::pc_chord(text),
              state)
  }, error = function(e){
    message("Call: ", capture.output(e$call))
    message("Message: ", e$message)
    shinyjs::alert("Invalid chord")
  })
}

set_chord <- function(chord, state) {
  state$chord <- chord
  state$chord_img_src <- get_chord_url(chord)
}

plot_input_spectrum <- function(analysis) {
  frequency <- analysis$spectrum_input$frequency_Hz
  amplitude <- hrep::dB_to_amplitude(analysis$spectrum_input$level_dB, 60)
  plot_sparse_spectrum(frequency, amplitude, range_Hz = NULL)
}

play_input_spectrum <- function(analysis) {
  frequency <- analysis$spectrum_input$frequency_Hz
  amplitude <- hrep::dB_to_amplitude(analysis$spectrum_input$level_dB, 60)
  play_sparse_spectrum(frequency, amplitude)
}

play_filtered_input_spectrum <- function(analysis) {
  frequency <- analysis$spectrum_after_ear_filtering$frequency_Hz
  amplitude <- hrep::dB_to_amplitude(
    analysis$spectrum_after_ear_filtering$level_dB, 60
  )
  play_sparse_spectrum(frequency, amplitude)
}

get_chord_url <- function(chord, type = "png") {
  assertthat::assert_that(
    assertthat::is.scalar(type),
    is.character(type),
    type %in% c("png", "mp3", "midi")
  )
  pitch <- as.integer(hrep::pi_chord(chord))
  label <- paste(pitch, collapse = "_")
  src <- "http://research.pmcharrison.com/studies/HarmonyDissonance/chords/piano/%s/%s.%s" %>%
    sprintf(label, label, type)
  src
}

analyse_chord <- function(x,
                          include_phase_impact_factors,
                          fundamental_dB,
                          num_harmonics) {
  spectrum <- hrep::sparse_fr_spectrum(hrep::pi_chord(x),
                                       num_harmonics = num_harmonics)

  shiny::withProgress(
    roughness_wang(
      x = spectrum,
      include_phase_impact_factors = include_phase_impact_factors,
      detail = TRUE,
      msg = function(n, N, msg) shiny::setProgress(n, msg)
    ),
    min = 0, max = 7
  )
}

shiny_ui_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    shinyjs::useShinyjs(),
    shiny::h4("Wang et al. (2013)",
              style = "padding-left: 20px"),
    shinydashboard::sidebarMenu(
      lapply(c(
        "About",
        "Input spectrum",
        "Filtered spectrum",
        "Channel waveforms",
        "Channel envelopes",
        "Filtered channel envelopes",
        "Modulation indices",
        "Phase impact factors",
        "Specific roughnesses"
      ), function(title) {
        shinydashboard::menuItem(title,
                                 tabName = gsub(" ", "_", tolower(title)),
                                 icon = NULL)
      }),
      shiny::uiOutput("total_roughness")
    )
  )
}

shiny_ui_body <- function(opt) {
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::column(6, shiny_ui_tabs(opt)),
      shiny::column(6, shiny_ui_input(opt))
    )
  )
}

shiny_ui_tabs <- function(opt) {
  shinydashboard::tabItems(
    shiny_ui_tab_0(),
    shiny_ui_tab_1(opt),
    shiny_ui_tab_2(opt),
    shiny_ui_tab_3(opt),
    shiny_ui_tab_4(),
    shiny_ui_tab_5(),
    shiny_ui_tab_6(),
    shiny_ui_tab_7(),
    shiny_ui_tab_8()
  )
}

shiny_ui_tab_0 <- function() {
  shinydashboard::tabItem(
    tabName = "about",
    shinydashboard::box(
      # title = "Input spectrum",
      title = "About",
      status = "primary",
      width = 12,
      shiny::tags$p("This interactive app analyses the roughness of musical chords",
                    "using Wang et al.'s (2013) algorithm.",
                    "Use the tabs on the left-hand side of the screen",
                    "to navigate through the successive stages of the model."),
      shiny::tags$p("Wang, Y. S., Shen, G. Q., Guo, H., Tang, X. L., & Hamade, T. (2013).",
                    "Roughness modelling based on human auditory perception for",
                    "sound quality evaluation of vehicle interior noise.",
                    shiny::tags$em("Journal of Sound and Vibration"),
                    "332(16), 3893-3904.",
                    shiny::tags$a(href = "https://doi.org/10.1016/j.jsv.2013.02.030")
      )
    )
  )
}

shiny_ui_tab_1 <- function(opt) {
  shinydashboard::tabItem(
    tabName = "input_spectrum",
    shinydashboard::box(
      # title = "Input spectrum",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("The input to the model is an acoustic spectrum."),
      shiny::div(shiny::plotOutput("plot_input_spectrum"),
                 style = "max-width: 100%"),
      if (opt$audio) shiny::actionButton("play_input_spectrum", "Play")
    )
  )
}

shiny_ui_tab_2 <- function(opt) {
  shinydashboard::tabItem(
    tabName = "filtered_spectrum",
    shinydashboard::box(
      # title = "Filtered spectrum",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("The spectrum is filtered by the outer and middle ear."),
      shiny::plotOutput("plot_filtered_input_spectrum"),
      if (opt$audio) shiny::actionButton("play_filtered_input_spectrum", "Play")
    )
  )
}

shiny_ui_tab_3 <- function(opt) {
  shinydashboard::tabItem(
    tabName = "channel_waveforms",
    shinydashboard::box(
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("Different cochlear channels selectively filter",
                    "for different frequency ranges."),
      shiny::plotOutput("plot_channel_wave_form"),
      shiny::sliderInput("channel_wave_forms_channel_num",
                         "Channel number",
                         min = 1, max = 47, value = 25, step = 1),
      if (opt$audio) shiny::fluidRow(
        shiny::column(3, shiny::actionButton("play_channel_wave_form", "Play",
                                             style = "text-align: center")),
        shiny::column(9, shiny::checkboxInput("normalise_volume_across_channels",
                                              "Normalise volume across channels?",
                                              value = TRUE))
      )
    ))
}

shiny_ui_tab_4 <- function() {
  shinydashboard::tabItem(
    tabName = "channel_envelopes",
    shinydashboard::box(
      # title = "Channel envelopes",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("Waveform envelopes are extracted",
                    "for each channel."),
      shiny::plotOutput("plot_channel_envelope"),
      shiny::sliderInput("channel_envelopes_channel_num", "Channel number",
                         min = 1, max = 47, value = 25, step = 1)
    ))
}

shiny_ui_tab_5 <- function() {
  shinydashboard::tabItem(
    tabName = "filtered_channel_envelopes",
    shinydashboard::box(
      # title = "Filtered channel envelopes",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("These amplitude modulations are filtered to prioritise",
                    "the modulation frequencies that contribute most towards roughness."),
      shiny::plotOutput("plot_filtered_channel_envelope"),
      shiny::sliderInput("filtered_channel_envelopes_channel_num", "Channel number",
                         min = 1, max = 47, value = 25, step = 1)
    ))
}

shiny_ui_tab_6 <- function() {
  shinydashboard::tabItem(
    tabName = "modulation_indices",
    shinydashboard::box(
      # title = "Modulation indices",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("The modulation index captures the magnitude of",
                    "amplitude modulation for each channel."),
      shiny::plotOutput("plot_modulation_indices")
    ))
}

shiny_ui_tab_7 <- function() {
  shinydashboard::tabItem(
    tabName = "phase_impact_factors",
    shinydashboard::box(
      # title = "Phase impact factors",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("Phase impact factors capture the correlation between",
                    "the envelopes of adjacent channels.",
                    "According to Wang et al. (2013), higher correlations",
                    "yield greater roughness."),
      shiny::plotOutput("plot_phase_impact_factors")
    ))
}

shiny_ui_tab_8 <- function() {
  shinydashboard::tabItem(
    tabName = "specific_roughnesses",
    shinydashboard::box(
      # title = "Specific roughnesses",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("The roughness contribution of each critical band is",
                    "estimated as a function of modulation index,",
                    "phase impact factor, and pitch height of the critical band.",
                    "These are then summed to give the total roughness value."),
      shiny::plotOutput("plot_specific_roughnesses")
    ))
}


shiny_ui_input <- function(opt) {
  shinydashboard::box(
    shiny::p("Enter a pitch-class set to analyse.",
             "The first pitch class will be taken as the bass note."),
    shiny::textInput("chord", label = NULL, placeholder = "e.g. 4 0 7"),
    shiny::actionButton("enter_new_chord", "Enter"),
    shiny::uiOutput("current_chord_text"),
    shiny::uiOutput("current_chord_image", height = "auto"),
    shiny::checkboxInput("include_phase_impact_factors",
                         "Include phase impact factors",
                         value = opt$default_include_phase_impact_factors),
    shiny::sliderInput("num_harmonics", "Number of harmonics",
                       value = opt$default_num_harmonics,
                       min = 1, max = 15, step = 1),
    style = "text-align: center"
  )
}

#' Wang et al. (2013) interactive demo
#'
#' Launches an interactive demo of Wang et al.'s (2013) roughness model.
#' This function requires a few additional packages to be installed;
#' you will be notified if any of these packages are missing
#' once you run \code{demo_wang()}.
#' @param audio (Scalar logical) Whether to enable playback controls
#' (currently the playback controls don't work when the app is hosted
#' on a remote server).
#' @note The demo takes the form of an Shiny app
#' (\url{https://shiny.rstudio.com/}).
#' @references
#' \insertRef{Wang2013}{wang13}
#' @export
demo_wang <- function(audio = TRUE) {
  pkg_suggest <- c("cowplot", "shiny", "shinyjs", "shinydashboard")
  purrr::map_lgl(pkg_suggest, requireNamespace) %>%
    {
      if (any(!.)) {
        stop("the following packages need to be installed before continuing: ",
             paste(pkg_suggest[!.], collapse = ", "))
      }
    }

  opt <- list(
    default_chord = hrep::pc_chord(c(4, 0, 7)),
    default_num_harmonics = 11,
    default_include_phase_impact_factors = FALSE,
    fundamental_dB = 60,
    audio = audio
  )

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Wang et al. (2013)",
                                    disable = FALSE),
    shiny_ui_sidebar(),
    shiny_ui_body(opt)
  )

  server <- function(input, output) {
    state <- shiny::reactiveValues(
      chord = opt$default_chord,
      analysis = NULL
    )
    output$current_chord_text <- shiny::renderUI(
      shiny::tags$p("Current chord: ",
                    shiny::tags$strong(as.character(state$chord)))
    )
    output$current_chord_image <- shiny::renderUI(
      shiny::img(src = get_chord_url(state$chord),
                 contentType = 'image/png',
                 alt = "Current chord",
                 style = paste("max-width: 150px;",
                               "border-style: solid;",
                               "border-width: 5px;",
                               "border-color: white"))
    )
    shiny::observeEvent(
      input$enter_new_chord,
      enter_new_chord(
        input$chord,
        state)
    )
    shiny::observe({
      state$analysis <- analyse_chord(x = state$chord,
                                      input$include_phase_impact_factors,
                                      opt$fundamental_dB,
                                      input$num_harmonics)
    })
    shiny::observeEvent(input$play_input_spectrum, {
      play_input_spectrum(state$analysis)
    })
    shiny::observeEvent(input$play_filtered_input_spectrum, {
      play_filtered_input_spectrum(state$analysis)
    })
    shiny::observeEvent(input$play_channel_wave_form, {
      play_channel_wave_forms(
        channel_wave_forms = state$analysis$channel_wave_forms,
        channel_num = input$channel_wave_forms_channel_num,
        scale_to_other_channels = input$normalise_volume_across_channels
      )
    })

    output$plot_input_spectrum <- shiny::renderPlot(
      plot_input_spectrum(state$analysis))

    output$plot_filtered_input_spectrum <- shiny::renderPlot(
      plot_filtered_input_spectrum(state$analysis))

    output$plot_channel_wave_form <- shiny::renderPlot(
      plot_waveform(
        state$analysis$channel_wave_forms[[input$channel_wave_forms_channel_num]],
        range_sec = c(0, 0.05)
      ))
    output$plot_channel_envelope <- shiny::renderPlot(
      plot_waveform(
        state$analysis$channel_envelopes[[input$channel_envelopes_channel_num]],
        range_sec = c(0, 0.05)
      ))
    output$plot_filtered_channel_envelope <- shiny::renderPlot(
      plot_waveform(
        state$analysis$filtered_channel_envelopes[[input$filtered_channel_envelopes_channel_num]],
        range_sec = c(0, 0.05)
      ))
    output$plot_modulation_indices <- shiny::renderPlot({
      plot_modulation_indices_wang(state$analysis$modulation_indices)
    })

    output$plot_phase_impact_factors <- shiny::renderPlot(
      plot_phase_impact_factors_wang(state$analysis$phase_impact_factors))

    output$plot_specific_roughnesses <- shiny::renderPlot(
      plot_specific_roughnesses_wang(state$analysis$specific_roughnesses))

    output$total_roughness <- shiny::renderUI({
      x <- round(state$analysis$total_roughness, digits = 5)
      shiny::showNotification(shiny::p("Roughness:", shiny::strong(x)),
                              type = "message",
                              duration = 120)
      shiny::p("Output:",
               shiny::strong(x),
               style = "padding: 15px; font-size: 12pt")
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}

#' Wang et al.'s (2013) roughness model
#'
#' Gets the roughness of a sonority according to the model of Wang et al. (2013).
#' @param x Object to analyse,
#' which will be coerced to an object of class
#' \code{\link[hrep]{sparse_fr_spectrum}}.
#' Various input types are possible:
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' which will be expanded into their implied harmonics.
#' * A two-element list can be used to define a harmonic spectrum.
#' The first element should be a vector of frequencies in Hz,
#' the second a vector of amplitudes.
#' * The function also accepts classes from the \code{hrep} package,
#' such as produced by \code{\link[hrep]{pi_chord}()} and
#' \code{\link[hrep]{sparse_fr_spectrum}()}.
#' @param detail (Logical scalar) Whether to return detailed output information.
#' @param include_phase_impact_factors (Logical scalar)
#' Whether to include phase impact factors in roughness computation.
#' Set to \code{TRUE} to recover the original specifications of Wang et al. (2013).
#' However, disabling this feature (by leaving the parameter at \code{FALSE})
#' seems to result in better estimation of perceptual consonance.
#' @param unit_amplitude_in_dB (Numeric scalar)
#' Determines the decibel level of a partial with amplitude 1.
#' When the input is a musical chord,
#' this will correspond to the decibel level of the fundamental frequencies
#' of each chord tone.
#' @param msg Function to be called to give progress updates.
#' This function should accept three arguments:
#' \code{n}, an integer identifying the current position in the pipeline,
#' \code{N}, an integer identifying the length of the pipeline,
#' and \code{msg}, a string providing a longer-format description
#' of the current position in the pipeline.
#' Pass \code{NULL} to disable progress updates.
#' @param ... Additional parameters to pass to
#' \code{\link[hrep]{sparse_fr_spectrum}}.
#' * \code{num_harmonics}: Number of harmonics to use when expanding
#' chord tones into their implied harmonics.
#' * \code{roll_off}: Rate of amplitude roll-off for the harmonics.
#' @return If \code{detail == FALSE}, a numeric vector of roughnesses,
#' otherwise a list containing detailed algorithm output.
#' @references
#' \insertRef{Wang2013}{wang13}
#' @note
#' This implementation is designed for sparse input spectra, that is,
#' spectra containing only a few (< 100) components.
#' @md
#' @rdname roughness_wang
#' @export
roughness_wang <- function(
    x,
    detail = FALSE,
    include_phase_impact_factors = FALSE,
    unit_amplitude_in_dB = 60,
    msg = function(n, N, msg)
      if (interactive())
        message(n, "/", N, ": ", msg),
    ...
) {
  UseMethod("roughness_wang")
}

#' @rdname roughness_wang
#' @export
roughness_wang.default <- function(
    x,
    detail = FALSE,
    include_phase_impact_factors = FALSE,
    unit_amplitude_in_dB = 60,
    msg = function(n, N, msg)
      if (interactive())
        message(n, "/", N, ": ", msg),
    ...
) {
  x <- hrep::sparse_fr_spectrum(x, ...)
  do.call(roughness_wang, as.list(environment()))
}

#' @rdname roughness_wang
#' @export
roughness_wang.sparse_fr_spectrum <- function(
    x,
    detail = FALSE,
    include_phase_impact_factors = FALSE,
    unit_amplitude_in_dB = 60,
    msg = function(n, N, msg)
      if (interactive())
        message(n, "/", N, ": ", msg),
    ...
) {
  frequency_Hz <- hrep::freq(x)
  level_dB <- hrep::amplitude_to_dB(hrep::amp(x), unit_amplitude_in_dB)
  if (is.null(msg)) msg <- function(...) NULL

  msg(1, 6, "Ear transmission...")
  level_dB_filtered <- level_dB - ear_transmission(frequency_Hz)

  msg(2, 6, "Channel excitation levels...")
  channel_sound_excitation_levels <- get_channel_sound_excitation_levels(
    frequency_Hz = frequency_Hz,
    level_dB_filtered = level_dB_filtered
  )

  # <channel_wave_forms> is a list of numeric vectors corresponding to the
  # y values of the waveforms for each channel for time in [0s, 1s].
  # The units of y is amplitude ratios relative to the reference sound amplitude.
  msg(3, 6, "Channel waveforms...")
  channel_wave_forms <- purrr::map(.x = channel_sound_excitation_levels,
                                   .f = get_channel_wave_form,
                                   frequency_Hz)

  # These are waveforms corresponding to the signal envelopes
  msg(4, 6, "Channel envelopes...")
  channel_envelopes <- purrr::map(.x = channel_wave_forms,
                                  .f = get_channel_envelope)

  # The channel envelopes are filtered to account for the different roughness
  # contributions of different modulation frequencies
  msg(5, 6, "Filtering channel envelopes...")
  filtered_channel_envelopes <- purrr::map2(.x = seq_along(channel_envelopes),
                                            .y = channel_envelopes,
                                            .f = filter_channel_envelope)

  msg(6, 6, "Computing roughness...")
  modulation_indices <- purrr::map2_dbl(.x = filtered_channel_envelopes,
                                        .y = channel_wave_forms,
                                        .f = get_modulation_index)

  phase_impact_factors <- purrr::map_dbl(.x = seq_len(47),
                                         .f = get_phase_impact_factor,
                                         filtered_channel_envelopes)

  specific_roughnesses <- purrr::pmap_dbl(.l = list(get_channel_weight(seq_len(47)),
                                                    phase_impact_factors,
                                                    modulation_indices),
                                          .f = get_specific_roughness,
                                          include_phase_impact_factors)

  total_roughness <- 0.25 * sum(specific_roughnesses)

  if (detail)
    compile_detail(as.list(environment())) else
      total_roughness
}
