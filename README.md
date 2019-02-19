# Thesis
Content related to Masters Thesis and subsequent data analysis

# Description of code

1. Data import

The data import code takes tab deliminated text files from Chart software (AD Instruments). Files contains 5 lines of parameters which are cut. Each row of the main data is formatted as Time (in secnds), followed by a marker value used to identify stimulator triggers,followed by the left eye value in uV, followed by the right eye value in uV. Data is assumed to be at 10,000Hz and an example set is provided. Data name is comprised of "Participant X", "RS/NR" (Responder or Non-Responder), "L/R" (starting hand), "T/C" (test condition or control), "Block 1/2" then sample rate (decpreciated)

The code then imports all selected files and collates them.

Performs recitification, then outputs a large data structure.

2. Primary analysis



To be continued.....
