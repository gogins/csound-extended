# OM2Csound in Real Time
## Michael Gogins
## July 2018

This version of the OpenMusic OM2Csound library improves the previous OM2Csound 
library as follows:

1. Csound will output real-time audio if the output file is `dac`. In the 
OM2Csound preference dialog, any valid operating system audio output device 
can be set, which will be substituted for `dac` at run time.

2. The improved OM2Csound ibrary uses CFFI to call the Csound shared library 
directly, rather than running Csound as an external program. This makes it 
possible for the user to interrupt the Csound performance at any time.

These changes were made to speed up the composing workflow in OpenMusic, by 
enabling the user to hear a piece immediately, and to interrupt Csound and 
return to editing the piece immediately.

## Installation

First install the regular OM2Csound library. Then, copy the 
`sources/csound-synth.lisp` and `sources\csound-preferences.lisp` files from 
here over the regular files.

The Csound shared library (`libcsound64.so` on Linux) must be loadable from 
the Lisp process, and the Lisp runtime must have the same CPU architecture 
as Csound (the default is now 64 bits).
