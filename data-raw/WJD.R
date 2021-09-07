
# library(DBI)
# library(RSQLite)
# library(tidyverse)
# library(ggplot2)
# library(psych)

# custom functions to handle this particular dataset

get.phrase <- function(row) {
  # get phrases based on segmentation information
  melodies[as.integer(row['start']):as.integer(row['end'])+1, "pitch"]
}

# import the WJD in its original SQLite format

con <- dbConnect(RSQLite::SQLite(), "wjazzd.db")

# Show List of Tables
as.data.frame(dbListTables(con))

# get melody table
melodies <- dbReadTable(con, 'melody')

# get section information
sections <-dbReadTable(con, 'sections')

# the WJD already has phrase information
phrases <- sections[sections$type=="PHRASE", ]

# list of the melodic phrases
melodic.phrases <- apply(phrases, MARGIN = 1, get.phrase)

relative.phrases <- lapply(melodic.phrases, diff)

no_phrases <- length(relative.phrases)
# 11,082 phrases


# create string versions of the phrases and collapse into factors to see how many unique phrases there are
relative.phrases.str <- lapply(relative.phrases, paste0, collapse = ",")
relative.phrases.factor <- as.factor(unlist(relative.phrases.str))
levels(relative.phrases.factor)
no_unique <- length(levels(relative.phrases.factor))
# 8,111 unique phrases

vc <- as.data.frame(base::table(as.vector(unlist(relative.phrases.str))))
names(vc) <- c("melody", "count")
# remove the empty mel
vc <- vc[-which(vc$mel == ""), ]

vc <- arrange(vc, desc(count))

hist(vc$count)

sum_freq <- sum(vc$count)

vc$melody <- as.character(vc$melody)
names(vc) <- c("melody", "freq")

vc$N <- lapply(vc$melody, function(x) length(str.mel.to.vector(x, sep = ","))+1 )

# remove the random repeated note melodies
vc <- vc[!vc$melody=="0", ]
vc <- vc[!vc$melody=="0,0", ]
vc <- vc[!vc$melody=="0,0,0", ]
vc <- vc[!vc$melody=="-6,6", ] # hm maybe this should go back in?


# now use the itembankr function
# there are no midi/musicxml files (this the WJD are preprocessed everything for us)
# we can give our dataframe to the corpus_to_item_bank function, which them simply computes features for us
# as well as makes an ngram database

WJD <- corpus_to_item_bank(corpus_name = "WJD", corpus_df = vc, output_type = "ngram")


usethis::use_data(WJD, overwrite = TRUE)
