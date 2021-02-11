# Initial reference: https://towardsdatascience.com/how-to-play-star-wars-and-other-tunes-in-r-5e837e8b71e4

# This file requires the audio package if not is installed, will be installed
if (!require("audio")) install.packages("audio")

library("dplyr")
library("audio")

make_duration <- function(sequence = c(), extra_notes = c()) {
  return(rep(c(rep(sequence, 2),extra_notes),2))
}

# Changed to spanish notes
notes <- c(la = 0, si = 2, do = 3, re = 5, mi = 7, fa = 8, sol = 10)

# Uses full vector to catch easier the notes
song_notes <- c(
   "re",  "re",  "sol", "sol", "fa#", "sol", "la",  "fa#"
  ,"sol", "la",  "si",  "sol", "la",  "si",  "do5", ""
  ,"do5", "do5", "si",  "la",  "sol", "fa#", "la",  "sol"
  ,"fa#", "sol", "la",  "si",  "do5", "la",  "sol", ""
  
  ,"si",  "la",  "fa#", "fa#", "sol", "la",  "si",  "sol"
  ,"si",  "la",  "fa#", "fa#", "sol", "la",  "si",  ""
  ,"sol", "sol", "do5", "do5", "re5", "do5", "si",  "sol"
  ,"fa#", "sol", "la",  "si",  "do5", "la",  "sol", ""
  
  ,"re",  "sol", "fa",  "sol", "sol", "fa#", "sol", "sol"
  ,"sol", "si",  "la",  "sol", "fa#", "sol", "la",  "la", ""
  ,"re",  "la",  "sol", "la",  "la",  "sol", "la",  "la"
  ,"si",  "do5", "si",  "si",  "la",  "fa#", "sol", "sol", ""
  
  ,"la",  "sol", "fa#", "mi",  "fa#", "sol", "la",  "sol"
  ,"la",  "si",  "do5", "si",  "do5", "do5", "si",  "do5"
  ,"si",  "la",  "sol", "fa#", "sol", "la",  "si",  "si"
  ,"si",  "do5", "si",  "si",  "la",  "fa#", "sol", ""
)

song_duration <- c(
  make_duration(c(1.5, 1.5, 1.5, 1.5, 1.5, 1.7, 1.5   ), c(2,1)),
  make_duration(c(1.5, 1.5, 1.3, 1.1, 1.3, 1.5, 1.5   ), c(2,1)),
  make_duration(c(1.5, 1.3, 1.2, 1.3, 1.2, 1.3, 1.5, 2), c(1)  ),
  make_duration(c(1.5, 1.3, 1.2, 1.3, 1.2, 1.3, 1.5, 2)        )
)


song <- data_frame(pitch = song_notes,duration = song_duration)
song <-
  song %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[sub("^([[:alpha:]]*).*", "\\1", pitch)],
         note = note + grepl("#", pitch) -
           grepl("*", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)


tempo <- 280
sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

song_wave <-
  mapply(make_sine, song$freq, song$duration) %>%
  do.call("c", .)

play(song_wave)
