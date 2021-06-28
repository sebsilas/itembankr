#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct  2 10:22:29 2019

@author: sebsilas
"""

# for each midi file in a directory, extract note values and remove repetitions

import os
import glob
import mido
from itertools import groupby
import csv

# set wd
os.chdir('/Users/sebsilas/PhD 2021/magma-Gold/magmaGold/R/www/item_banks/berkowitz_midi_rhythmic_100bpm')

# main list for all processed files
main = []

# what is 100 bpm in microseconds?
new_tempo = mido.bpm2tempo(100)
#  600000

test = mido.MidiFile('Berkowitz1.mid')

# test.print_tracks()
a = test.tracks
b = test.tracks[0]


for track in b:
    if track.type == 'set_tempo':
        c = track
        c.tempo = new_tempo
        print(track)
    
#%%

# get all midi files
for file in glob.glob("*.mid"):
        
    mid = mido.MidiFile(file)


    for track in mid.tracks[0]:
        if track.type == 'set_tempo':

            track.tempo = new_tempo
            print(track)
    mid.save('out/100bpm_' + file)
    
    
#%% 

file = open("Max_MSP_Berk.txt","w") 


for line in main:
    idx = main.index(line)
    tidy_line = str(line).replace("[","").replace("]","").replace(",","")
    new = str(idx) + "," + tidy_line + "; \n"
    file.write(new) 

    

file.close() 



#%%
# write list to csv


with open("out.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerows(main2)

#%%
    
# json 

with open('Berkowitz_stim_JSON.json', 'w') as outfile:
    json.dump(main, outfile)

#%%
    
# create midi files of the list
    
    
new_mid = MidiFile()
track = MidiTrack()
new_mid.tracks.append(track)

track.append(Message('program_change', program=12, time=0))
track.append(Message('note_on', note=64, velocity=64, time=32))
track.append(Message('note_off', note=64, velocity=127, time=32))

mid.save('new_song.mid')
    
    
    

#%%
<meta message time_signature numerator=4 denominator=4 clocks_per_click=24 notated_32nd_notes_per_beat=8 time=0>
<meta message key_signature key='C' time=0>
<meta message set_tempo tempo=500000 time=0>
control_change channel=0 control=121 value=0 time=0
control_change channel=0 control=7 value=100 time=0
control_change channel=0 control=10 value=63 time=0
control_change channel=0 control=91 value=0 time=0
control_change channel=0 control=93 value=0 time=0
<meta message midi_port port=0 time=0>
note_on channel=0 note=62 velocity=80 time=0
note_on channel=0 note=62 velocity=0 time=0.7489583333333333
note_on channel=0 note=64 velocity=80 time=0.0010416666666666667
note_on channel=0 note=64 velocity=0 time=0.24895833333333334
note_on channel=0 note=65 velocity=80 time=0.0010416666666666667
note_on channel=0 note=65 velocity=0 time=0.49895833333333334
note_on channel=0 note=61 velocity=80 time=0.0010416666666666667
note_on channel=0 note=61 velocity=0 time=0.49895833333333334
note_on channel=0 note=62 velocity=80 time=0.0010416666666666667
note_on channel=0 note=62 velocity=0 time=0.7489583333333333
note_on channel=0 note=65 velocity=80 time=0.0010416666666666667
note_on channel=0 note=65 velocity=0 time=0.24895833333333334
note_on channel=0 note=68 velocity=80 time=0.0010416666666666667
note_on channel=0 note=68 velocity=0 time=0.9989583333333333
note_on channel=0 note=69 velocity=80 time=0.0010416666666666667
note_on channel=0 note=69 velocity=0 time=0.49895833333333334
note_on channel=0 note=67 velocity=80 time=0.0010416666666666667
note_on channel=0 note=67 velocity=0 time=0.49895833333333334
note_on channel=0 note=66 velocity=80 time=0.0010416666666666667
note_on channel=0 note=66 velocity=0 time=0.49895833333333334
note_on channel=0 note=63 velocity=80 time=0.0010416666666666667
note_on channel=0 note=63 velocity=0 time=0.49895833333333334
note_on channel=0 note=65 velocity=80 time=0.0010416666666666667
note_on channel=0 note=65 velocity=0 time=0.7489583333333333
note_on channel=0 note=64 velocity=80 time=0.0010416666666666667
note_on channel=0 note=64 velocity=0 time=0.24895833333333334
note_on channel=0 note=59 velocity=80 time=0.0010416666666666667
note_on channel=0 note=59 velocity=0 time=0.9989583333333333
note_on channel=0 note=60 velocity=80 time=0.0010416666666666667
note_on channel=0 note=60 velocity=0 time=0.7489583333333333
note_on channel=0 note=64 velocity=80 time=0.0010416666666666667
note_on channel=0 note=64 velocity=0 time=0.24895833333333334
note_on channel=0 note=63 velocity=80 time=0.0010416666666666667
note_on channel=0 note=63 velocity=0 time=0.49895833333333334
note_on channel=0 note=59 velocity=80 time=0.0010416666666666667
note_on channel=0 note=59 velocity=0 time=0.49895833333333334
note_on channel=0 note=60 velocity=80 time=0.0010416666666666667
note_on channel=0 note=60 velocity=0 time=0.7489583333333333
note_on channel=0 note=63 velocity=80 time=0.0010416666666666667
note_on channel=0 note=63 velocity=0 time=0.24895833333333334
note_on channel=0 note=66 velocity=80 time=0.0010416666666666667
note_on channel=0 note=66 velocity=0 time=0.49895833333333334
note_on channel=0 note=65 velocity=80 time=0.0010416666666666667
note_on channel=0 note=65 velocity=0 time=0.49895833333333334
note_on channel=0 note=70 velocity=80 time=0.0010416666666666667
note_on channel=0 note=70 velocity=0 time=0.7489583333333333
note_on channel=0 note=69 velocity=80 time=0.0010416666666666667
note_on channel=0 note=69 velocity=0 time=0.24895833333333334
note_on channel=0 note=66 velocity=80 time=0.0010416666666666667
note_on channel=0 note=66 velocity=0 time=0.9989583333333333
note_on channel=0 note=70 velocity=80 time=0.0010416666666666667
note_on channel=0 note=70 velocity=0 time=0.7489583333333333
note_on channel=0 note=68 velocity=80 time=0.0010416666666666667
note_on channel=0 note=68 velocity=0 time=0.24895833333333334
note_on channel=0 note=64 velocity=80 time=0.0010416666666666667
note_on channel=0 note=64 velocity=0 time=0.9989583333333333
note_on channel=0 note=70 velocity=80 time=0.0010416666666666667
note_on channel=0 note=70 velocity=0 time=0.7489583333333333
note_on channel=0 note=73 velocity=80 time=0.0010416666666666667
note_on channel=0 note=73 velocity=0 time=0.24895833333333334
note_on channel=0 note=71 velocity=80 time=0.0010416666666666667
note_on channel=0 note=71 velocity=0 time=0.33229166666666665
note_on channel=0 note=67 velocity=80 time=0.0010416666666666667
note_on channel=0 note=67 velocity=0 time=0.33229166666666665
note_on channel=0 note=63 velocity=80 time=0.0010416666666666667
note_on channel=0 note=63 velocity=0 time=0.33229166666666665
note_on channel=0 note=66 velocity=80 time=0.0010416666666666667
note_on channel=0 note=66 velocity=0 time=0.7489583333333333
note_on channel=0 note=65 velocity=80 time=0.0010416666666666667
note_on channel=0 note=65 velocity=0 time=0.24895833333333334
note_on channel=0 note=62 velocity=80 time=0.0010416666666666667
note_on channel=0 note=62 velocity=0 time=0.49895833333333334
note_on channel=0 note=61 velocity=80 time=0.5010416666666666
note_on channel=0 note=61 velocity=0 time=0.7489583333333333
note_on channel=0 note=64 velocity=80 time=0.0010416666666666667
note_on channel=0 note=64 velocity=0 time=0.24895833333333334
note_on channel=0 note=67 velocity=80 time=0.0010416666666666667
note_on channel=0 note=67 velocity=0 time=0.49895833333333334
note_on channel=0 note=66 velocity=80 time=0.0010416666666666667
note_on channel=0 note=66 velocity=0 time=0.49895833333333334
note_on channel=0 note=63 velocity=80 time=0.0010416666666666667
note_on channel=0 note=63 velocity=0 time=1.4989583333333334
<meta message end_of_track time=0.0010416666666666667>