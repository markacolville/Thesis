# Thesis
Content related to Masters Thesis and subsequent data analysis

# Description of code

1. Data import

The data import code takes tab deliminated text files from Chart software (AD Instruments). Files contains 5 lines of parameters which are cut. Each row of the main data is formatted as Time (in secnds), followed by a marker value used to identify stimulator triggers,followed by the left eye value in uV, followed by the right eye value in uV. Data is assumed to be at 10,000Hz and an example set is provided. Data name is comprised of "Participant X", "RS/NR" (Responder or Non-Responder), "L/R" (starting hand), "T/C" (test condition or control), "Block 1/2" then sample rate (decpreciated)

First it scans for marker values in excess of 1volt and excludes repeated marker values within 10sec. Then it marks subsequent data based on the comment channel (used for the distance being tested). Data is then split into trial chunks, trimmed to 400ms of data per trial, 100 pre stimuli, 300 post stimuli. 200ms post stimuli is useful, and 100 excess either side to be used when needed (also for smoothing functions that destroy data). Data is then rectified, timepoints are given and distance properly marked.

Following this is for the manual data entry of Age, Gender and assorted statistics. This file and main data file needs to be manually affixed together (Will fix later maybe)


2. Primary analysis

First contains OnsetAlg and OffsetAlgs, functions used later, "ExtractVal" which is basically a variable condition lapply funciton and a function to add titles to tableGrobs (because that isnt an easy thing apparently).

Basic init.

Optional code to upscale data from 1k to 10k (was used for Participant 1)


To be continued....
