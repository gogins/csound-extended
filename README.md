# Csound Goodies

This Web site contains various resources for using Csound. These resources include examples, a playable version of the Csound reference manual, and Lisp code written by Drew Krause that can be used for algorithmic composition for Csound.

## Playable version of the Csound Reference Manual

The following link will take you to an online version of the Csound Reference Manual. It contains examples that will play right in your browser. You can even edit the examples and play them again hear the results of your changes. Currently this is supported only in desktop versions of Google's Chrome browser.

https://gogins.github.io/csound/html/indexframes.html

Not all examples are playable at this time. Only csd based examples (not orc based) that output stereo audio, and do not use file-based resources (such as sample soundfiles) or network-based resources (e.g. OSC does not work), are currently working.

## Sample Pieces

This repository contains samples of integrating HTML5 capabilities with Csound, where the PNaCl build of Csound is used, so that Csound itself does not need to be installed. These examples should play on version 31 or later of 
Google's Chrome browser for OS X, Windows, or Linux. Click on the links below to play the examples.

https://gogins.github.io/csound/PNaCl_Csound_01_Introduction.html

https://gogins.github.io/csound/PNaCl_Csound_02_Instrument_Controls.html

https://gogins.github.io/csound/PNaCl_Csound_03_Style_Sheet.html

https://gogins.github.io/csound/CompileCsdText.html

https://gogins.github.io/csound/Scrims_pnacl.html

## Nudruz

This archive of Lisp code from [Drew Krause](http://www.drew-krause.com/) is hosted with his permission. The code has been lightly edited to make it more usable with Steel Bank Common Lisp and the current version of Csound. Drew's code also is licensed under the terms of the GNU Lesser General Public License.
