n <- hrep::alphabet_size("pc_chord")
chords <- hrep::list_chords("pc_chord")
root_by_pc_chord <- integer(n)
pb <- utils::txtProgressBar(max = n, style = 3)

for (i in seq_len(n)) {
  root_by_pc_chord[i] <- parn88::root(chords[[i]])
  utils::setTxtProgressBar(pb, i)
}

close(pb)

use_data(root_by_pc_chord, overwrite = TRUE)
