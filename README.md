# itembankr

`itembankr` is an R package for creating a useful item bank, to be used in psychological testing, from raw data. 
It is currently focused on musical and particularly monophonic, melodic stimuli.
For convenience, we refer to a "corpus" as some raw data, not in itself particularly useful in psychological testing, as the input, and an item bank as the useful output, which is typically a set of dataframes with the stimuli from the corpus organised in a useful way plus some new calculated representations.

Hence, the main function in `itembankr` is `create_item_bank`:

``` r
Berkowitz <- create_item_bank(corpus_name = 'Berkowitz',
                    midi_file_dir = 'berkowitz_midi_rhythmic_100bpm',
                    musicxml_file_dir = 'berkowitz_musicxml',
                    prefix = 'inst',
                    output_type = 'both')
```

The output of `create_item_bank` is a function which allows you to access whichever of the four possible item bank types you requested are:

``` r
# a fairly limited item bank consisting of just file references
Berkowitz("files")
# a database of the melodic items split up into ngrams (perhaps most useful for arrhythmic usage)
Berkowitz("ngram")
# a database of the melodic items split up into ngrams PLUS computed melodic features
Berkowitz("main")
# rather than splitting up the corpus into ngrams, split it up based on (a rather crude approximation of) phrase boundaries. this is perhaps more useful for rhythmic usage.
Berkowitz("phrases")
```

These various item banks can be used as inputs to tests created with the [musicassessr](https://github.com/syntheso/musicassessr) package.

Please note, these functions can take a long time to run.

By default, `create_item_bank` produces both ngram and phrase databases as output. You can request only some of the database types to be created using the output_type argument:

``` r
create_item_bank(output_type = "ngram") # only ngram
create_item_bank(output_type = "phrases") # only phrases

```

Or 

``` r
files_db <- create_item_bank_from_files(corpus_name = corpus_name,
                                        midi_file_dir = midi_file_dir,
                                        musicxml_file_dir = musicxml_file_dir,
                                        prefix = prefix)
                                        
ngram_db <- split_item_bank_into_ngrams(Berkowitz_files_db) # input should be a files db
freq_db <- count_freqs(Berkowitz_ngram_db) 
main_db <- get_melody_features(Berkowitz_freq_db, mel_sep = ",", durationMeasures = TRUE)
phrases_db <- create_phrases_db(corpus_name = corpus_name, midi_file_dir = add_prefix(paste0('item_banks/', corpus_name, '/', midi_file_dir), prefix), prefix = prefix)

```

## References

Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021). Expert musical improvisations contain sequencing biases seen in language production. Journal of Experimental Psychology. https://doi.org/10.1037/xge0001107

Berkowitz, S., Fontrier, G., Goldstein, P., & Smaldone, E. (2017). A new approach to sight singing (Sixth edition). W. W. Norton & Company

Crayencour, H.-C., Velichkina, O., Frieler, K., Höger, F., Pfleiderer, M., Henry, L., Solis, G., Wolff, D., Weyde, T., Peeters, G., Basaran, D., Smith, J., & Proutskova, P. (2021)

The DTL1000 Jazz Solo Dataset (in prep.). Journal on Computing and Cultural Heritage

Müllensiefen, D. (2009). FANTASTIC: Feature ANalysis Technology Accessing STatistics (In a Corpus; Technical report). 37.

Pfleiderer, M., Frieler, K., Abeßer, J., Zaddach, W.-G., & Burkhart, B. (Hrsg.). (2017). Inside the Jazzomat—New perspectives for jazz research. Schott Campus.

Slonimsky, N. (1947). Thesaurus of scales and melodic patterns. Literary Licensing, LLC.

