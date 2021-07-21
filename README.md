# itembankr

itembankr is an R package for creating a useful item bank, to be used in psychological testing, from raw data. 
It is currently focused on musical and particularly monophonic, melodic stimuli.
For convenience, we refer to a "corpus" as some raw data, not in itself particularly useful in psychological testing, as the input, and an item bank as the useful output, which is typically a dataframe with the stimuli from the corpus organised in a useful way plus some new calculated representations.

Hence, the main function itembankr is corpus_to_item_bankr:

```{r}
corpus_to_item_bank(corpus_name = 'Berkowitz',
                    midi_file_dir = 'berkowitz_midi_rhythmic_100bpm',
                    musicxml_file_dir = 'berkowitz_musicxml',
                    prefix = 'inst',
                    output_type = 'both')
```
