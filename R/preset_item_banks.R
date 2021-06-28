# define some dummy item_banks

berkowitz.rds.abs <- define_item_bank("Berkowitz_RDS_Absolute",
                                      type = "RDS_file_midi_notes",
                                      path = "Berkowitz_Absolute.RDS",
                                      absolute = TRUE
)


berkowitz.rds.rel <- define_item_bank("Berkowitz_RDS_Relative",
                                      type = "RDS_file_midi_notes",
                                      path = "Berkowitz_Relative.RDS",
                                      absolute = FALSE
)


berkowitz.midi <- define_item_bank("Berkowitz_midi",
                                   type = "midi_file",
                                   path = "berkowitz_midi_rhythmic",
                                   absolute = TRUE
)


berkowitz.musicxml <- define_item_bank("Berkowitz_musicxml",
                                       type = "musicxml_file",
                                       path = "berkowitz_musicxml",
                                       absolute = TRUE
)


berkowitz.item.bank.proper.format <- define_item_bank("Berkowitz_item_bank",
                                                      type = "RDS_file_full_format",
                                                      path = "Berkowitz_Item_Bank.RDS",
                                                      absolute = FALSE
)


slonimsky <- define_item_bank("Slonimsky",
                              type = "RDS_file_midi_notes",
                              path = "Slonimsky.RDS",
                              absolute = TRUE)


DTL_1000 <- define_item_bank("DTL_1000",
                              type = "RDS_file_full_format",
                              path = "DTL_1000.RDS",
                              absolute = FALSE)


WJD <- define_item_bank("WJD",
                         type = "RDS_file_full_format",
                         path = "WJD_item_bank.RDS",
                         absolute = FALSE)

rhythm_tapping_item_bank <- define_item_bank("rhythm_tapping_item_bank",
                                   type = "audio_file",
                                   path = "rhythm_tapping_item_bank")

