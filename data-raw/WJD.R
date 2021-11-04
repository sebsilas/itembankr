
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(psych)

# custom functions to handle this particular dataset

get.phrase <- function(row, get = "pitch") {
  # get phrases based on segmentation information
  as.vector(unlist(melodies[as.integer(row['start']):as.integer(row['end'])+1, get]))
}


# import the WJD in its original SQLite format

con <- dbConnect(RSQLite::SQLite(), "data-raw/wjazzd.db")

# Show List of Tables
as.data.frame(dbListTables(con))

# get melody table
melodies <- dbReadTable(con, 'melody')

# create ioi column
melodies <- melodies %>% group_by(melid) %>% mutate(ioi = c(NA, diff(onset))) %>% ungroup()


solo_info <- dbReadTable(con, 'solo_info')

# get section information
sections <-dbReadTable(con, 'sections')

# the WJD already has phrase information
phrases <- sections[sections$type=="PHRASE", ]

# list of the melodic phrases
melodic.phrases <- apply(phrases, MARGIN = 1, get.phrase)
melodic.phrases.df <- tibble(melody = melodic.phrases, melid = phrases$melid)
melodic.phrases.dur <- apply(phrases, MARGIN = 1, get.phrase, "duration")
melodic.phrases.ioi <- apply(phrases, MARGIN = 1, get.phrase, "ioi")
melodic.phrases.ioi <- lapply(melodic.phrases.ioi, function(x) x[!is.na(x)])



mean(melodies$duration, na.rm = TRUE)
mean(melodies$ioi, na.rm = TRUE)

# ioi is bigger, so use this as durations

relative.phrases <- lapply(melodic.phrases, diff)

no_phrases <- length(relative.phrases)
# 11,082 phrases


# create string versions of the phrases and collapse into factors to see how many unique phrases there are
relative.phrases.str <- lapply(relative.phrases, paste0, collapse = ",")
relative.phrases.factor <- as.factor(unlist(relative.phrases.str))

no_unique <- length(levels(relative.phrases.factor))
# 8,111 unique phrases

phr.dur <- lapply(melodic.phrases.dur, round, 2)
phr.dur <- lapply(phr.dur, paste0, collapse = ",")


phr.ioi <- lapply(melodic.phrases.ioi, round, 2)
phr.ioi <- lapply(phr.ioi, paste0, collapse = ",")

melodic.phrases <- lapply(melodic.phrases, paste0, collapse = ",")



phrases_dbs <- tibble(orig_abs_melody = unlist(melodic.phrases),
                      melody = unlist(relative.phrases.str),
                      durations = unlist(phr.ioi),
                      melid = phrases$melid)

phrases_dbs$N <- lapply(phrases_dbs$melody, function(x) length(itembankr::str_mel_to_vector(x, sep = ","))+1 )

# remove the empty mel
phrases_dbs <- phrases_dbs[!phrases_dbs$melody=="", ]
# remove the random repeated note melodies
phrases_dbs <- phrases_dbs[!phrases_dbs$melody=="0", ]
phrases_dbs <- phrases_dbs[!phrases_dbs$melody=="0,0", ]
phrases_dbs <- phrases_dbs[!phrases_dbs$melody=="0,0,0", ]
phrases_dbs <- phrases_dbs[!phrases_dbs$melody=="-6,6", ] # hm maybe this should go back in?

# join with tempo
phrases_dbs2 <- phrases_dbs %>% dplyr::inner_join(solo_info, by = "melid") %>%
  select(melody, orig_abs_melody, durations, avgtempo) %>%
  mutate(bpm_120_scale = avgtempo/80,
         durations_original = durations) %>% rowwise() %>%
  mutate(durations = paste0(round(itembankr::str_mel_to_vector(durations_original) * bpm_120_scale, 2), collapse = ",")) %>%
  ungroup() %>%
  select(-c(avgtempo, bpm_120_scale, durations_original))


phrases_dbs2 <- phrases_dbs2 %>% rowwise() %>%
  mutate(mean_duration = mean(itembankr::str_mel_to_vector(durations), na.rm = TRUE)) %>%
  ungroup()

#hist(phrases_dbs2$mean_duration)
#mean(phrases_dbs2$mean_duration, na.rm = TRUE)
#mean(itembankr::WJD("main")$mean_duration, na.rm = TRUE)

# now use the itembankr function
# there are no midi/musicxml files (this the WJD are preprocessed everything for us)
# we can give our dataframe to the corpus_to_item_bank function, which them simply computes features for us
# as well as makes an ngram database


phrases_dbs3 <- phrases_dbs2 %>% slice(sample(1:nrow(phrases_dbs2), 20))

WJD <- corpus_to_item_bank(corpus_name = "WJD", corpus_df = phrases_dbs2,
                           output_type = "ngram", phrases_db = phrases_dbs2, launch_app = FALSE)


t1 <- WJD("files")
t2 <- WJD("ngram")
t3 <- WJD("main")
t4 <- WJD("phrases")

usethis::use_data(WJD, overwrite = TRUE)

# usethis::use_data(ngram_db, main_db, phrases_db, internal = TRUE, overwrite = TRUE)

