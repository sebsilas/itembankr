# itembankr

`itembankr` is an *R* package for creating useful item banks from raw, relatively unstructured data. These item banks are intended mainly to be used in psychological testing but can also be used for corpus analyses. The package is currently focused on musical, and particularly monophonic, melodic stimuli.

For convenience, we refer to a "corpus" as some raw input data, not in itself particularly useful in psychological testing, and an "item bank" as the useful output, which is typically a set of dataframes with the stimuli from the corpus organised in a useful way (and some new computed melodic representations).

Hence, the main function in `itembankr` is `create_item_bank`:

```r

# Create an item bank from MIDI and/or musicxml files

create_item_bank(name = 'Berkowitz',
                 midi_file_dir = 'berkowitz_midi_rhythmic_100bpm',
                 musicxml_file_dir = 'berkowitz_musicxml')
```

You can also create an item bank simply by inputting MIDI notes and durations directly to the `input_df` argument via an *R* tibble/dataframe. Minimally, you need `abs_melody` and `durations` columns, e.g,:


```r

test_item_bank <- tibble::tribble(
  ~abs_melody,              ~durations,
  "61,62,63,64,65,66",      "1,2,3,4,5,6",
  "72, 73, 75, 78",         "1, 1, 1, 1"
)


create_item_bank(name = "Test", input_df = test_item_bank)

```

The output of `create_item_bank` is one or more `.rda` files, depending on what you requested as `output`.

You can read these into R like this:

```r

# A fairly limited item bank consisting of just file references:

load('Berkowitz_file.rda')


Berkowitz_file <- item_bank

# (we rename the object from "item_bank")

rm(item_bank)


# A database of the melodic items split up into ngrams (perhaps most useful for arrhythmic usage)

load('Berkowitz_ngram.rda')

Berkowitz_ngram <- item_bank

rm(item_bank)


# Rather than splitting up the corpus into ngrams, split it up based on (a rather crude approximation of) phrase boundaries. This is perhaps more useful for rhythmic usage.

Berkowitz_phrase <- item_bank

rm(item_bank)

# Everything combined

load('Berkowitz_combined.rda')

Berkowitz_combined <- item_bank

rm(item_bank)

```


These various item banks can be used as inputs to tests created with the [musicassessr](https://github.com/syntheso/musicassessr) package.

Please note, these functions can take a long time to run.

By default, `create_item_bank` produces all item bank types as output. You can request only some of the database types to be created using the `output` argument.

You can also use the underlying functions directly, e.g., see: 

- `create_item_bank_from_files`
- `split_item_bank_into_ngrams`
- `count_freqs`
- `get_melody_features`
- `create_phrases_db`


## References

Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021). Expert musical improvisations contain sequencing biases seen in language production. Journal of Experimental Psychology. https://doi.org/10.1037/xge0001107

Berkowitz, S., Fontrier, G., Goldstein, P., & Smaldone, E. (2017). A new approach to sight singing (Sixth edition). W. W. Norton & Company

Crayencour, H.-C., Velichkina, O., Frieler, K., Höger, F., Pfleiderer, M., Henry, L., Solis, G., Wolff, D., Weyde, T., Peeters, G., Basaran, D., Smith, J., & Proutskova, P. (2021)

The DTL1000 Jazz Solo Dataset (in prep.). Journal on Computing and Cultural Heritage

Müllensiefen, D. (2009). FANTASTIC: Feature ANalysis Technology Accessing STatistics (In a Corpus; Technical report). 37.

Pfleiderer, M., Frieler, K., Abeßer, J., Zaddach, W.-G., & Burkhart, B. (Hrsg.). (2017). Inside the Jazzomat—New perspectives for jazz research. Schott Campus.

Slonimsky, N. (1947). Thesaurus of scales and melodic patterns. Literary Licensing, LLC.

