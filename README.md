# itembankr

itembankr is an R package for creating a useful item bank, to be used in psychological testing, from raw data. 
It is currently focused on musical and particularly monophonic, melodic stimuli.
For convenience, we refer to a "corpus" as some raw data, not in itself particularly useful in psychological testing, as the input, and an item bank as the useful output, which is typically a set of dataframes with the stimuli from the corpus organised in a useful way plus some new calculated representations.

Hence, the main function itembankr is corpus_to_item_bankr:

``` r
Berkowitz <- corpus_to_item_bank(corpus_name = 'Berkowitz',
                    midi_file_dir = 'berkowitz_midi_rhythmic_100bpm',
                    musicxml_file_dir = 'berkowitz_musicxml',
                    prefix = 'inst',
                    output_type = 'both')
```

The output of corpus_to_item_bank is a function which allows you to access whichever of the four possible item bank types you requested are:

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

These various item banks can be used as inputs to tests created with the [musicassessr](https://github.com/syntheso/musicassessr) library.
