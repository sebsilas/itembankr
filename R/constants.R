pitch.classes <- c("A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "F#", "G", "Ab")
pitch.classes.flat <- c("A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab")
pitch.classes.sharp <- c("A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")

# the following are used in a different way from the last set
pc_labels <- c("C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "Bb", "B")
pc_labels_flat <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")
pc_labels_sharp <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")

# deal with odd cases:
odd_pitch_classes <- c("B#", "Cb", "E#", "Fb")

B_sharp <- 12*2:9
names(B_sharp) <- paste0("B#", 1:8)

C_flat <- 12*2:9
names(C_flat) <- paste0("Cb", 1:8)

E_sharp <- (12*2:8)+5
names(E_sharp) <- paste0("E#", 1:7)

F_flat <- (12*2:8)+5
names(F_flat) <- paste0("Fb", 1:7)



midi.sci.notation.nos <- c(rep(0,3),rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),rep(7,12), 8)

scientific.pitch.classes <- paste0(pitch.classes, midi.sci.notation.nos)
scientific.pitch.classes.flat <- paste0(pitch.classes.flat, midi.sci.notation.nos)
scientific.pitch.classes.sharp <- paste0(pitch.classes.sharp, midi.sci.notation.nos)

midi.to.pitch.classes.list <- as.list(rep(pitch.classes, 8)[1:88])
names(midi.to.pitch.classes.list) <- 21:108

midi.to.pitch.classes.numeric.list <- as.list(rep(as.integer(1:12), 8)[1:88])
names(midi.to.pitch.classes.numeric.list) <- 21:108

midi.to.sci.notation.list <- scientific.pitch.classes
names(midi.to.sci.notation.list) <- 21:108



sci.notation.to.midi.list <- 21:108
names(sci.notation.to.midi.list) <- scientific.pitch.classes

sci.notation.to.midi.list.flat <- 21:108
names(sci.notation.to.midi.list.flat) <- scientific.pitch.classes.flat
sci.notation.to.midi.list.flat <- c(sci.notation.to.midi.list.flat, C_flat, F_flat)

sci.notation.to.midi.list.sharp <- 21:108
names(sci.notation.to.midi.list.sharp) <- scientific.pitch.classes.sharp
sci.notation.to.midi.list.sharp <- c(sci.notation.to.midi.list.sharp, B_sharp, E_sharp)



pitch.class.to.midi.list <- list(c(21, 33, 45, 57, 69, 81, 93, 105),
                                 c(22, 34, 46, 58, 70, 82, 94, 106),
                                 c(23, 35, 47, 59, 71, 83, 95, 107),
                                 c(24, 36, 48, 60, 72, 84, 96, 108),
                                 c(25, 37, 49, 61, 73, 85, 97),
                                 c(26, 38, 50, 62, 74, 86, 98),
                                 c(27, 39, 51, 63, 75, 87, 99),
                                 c(28, 40, 52, 64, 76, 88, 100),
                                 c(29, 41, 53, 65, 77, 89, 101),
                                 c(30, 42, 54, 66, 78, 90, 102),
                                 c(31, 43, 55, 67, 79, 91, 103),
                                 c(32, 44, 56, 68, 80, 92, 104)
)



names(pitch.class.to.midi.list) <- pitch.classes

# usethis::use_data(scientific.pitch.classes, midi.to.pitch.classes.list, sci.notation.to.midi.list,
# midi.to.pitch.classes.numeric.list, midi.to.sci.notation.list,
# pitch.class.to.midi.list, pc_labels, pc_labels_sharp, pc_labels_flat,
# pitch.classes.flat, pitch.classes.sharp,
# sci.notation.to.midi.list.flat, sci.notation.to.midi.list.sharp, overwrite = TRUE)
#

