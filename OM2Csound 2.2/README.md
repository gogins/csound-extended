# OM2Csound in Real Time
## Michael Gogins

This OpenMusic library improves the existing OM2Csound library as follows:

1. Csound will output real-time audio if the output file is "dac."

2. The library uses CFFI to call the Csound library directly, rather than 
running Csound as an external program. This makes it possible for the user to 
interrupt the Csound performance at any time.

These changes were made to speed up the composing workflow in OpenMusic, by 
enabling the user to hear a piece immediately, and to interrupt Csound and 
return to composing immediately.

## Installation

First install the regular OMCHroma 2.2 library. Then, copy the 
`sources/csound-synth.lisp` file here over the regular file.
