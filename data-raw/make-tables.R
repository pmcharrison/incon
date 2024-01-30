# Generates the corpus dissonance tables that are
# stored along with this package.

library(incon)

corpora <- list(popular_1 = hcorp::popular_1)
type <- "pc_chord_type"

unlink("data", recursive = TRUE)
dir.create("data")

for (i in seq_along(corpora)) {
  corpus <- corpora[[i]]
  corpus_label <- names(corpora)[[i]]
  data_label <- paste0(corpus_label, "_", type)

  env <- new.env()
  env[[data_label]] <- corpus_dissonance_table(corpus, type)

  save(list = data_label,
       envir = env,
       file = file.path("data", paste0(data_label, ".rda")))
}
