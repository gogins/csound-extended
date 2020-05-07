I am pleased to announce new releases of my various projects on GitHub.

## [csound-extended](https://github.com/gogins/csound-extended/releases)

Csound performances can now be stopped and restarted in csound.node, and Csound message printing now works for successive performances.

Some of the examples using csound.node and WebAssembly have been improved.

The `csound_loader.js` script for loading Csound in the same manner across the csound.node, Android, and WebAssembly platforms has been much improved and is much more reliable.

The CsoundAC VoiceleadingNode class now implements conforming notes produced by children of this node to specific instances of the Chord class, by timed segment. This makes composing algorithmically using chords, transformations of chords, and automatic voice-leading much easier and more flexible.

Out of order and incorrect comments in the VoiceleadingNode class have been fixed, leading to corrected Doxygen documentation for this class.

The Python interface for CsoundAC now specifically targets Python2, for compatibility with the csnd6 module in the csound repository.

The ["live" version of the Csound Reference Manual](https://gogins.github.io/csound-extended/html/indexframes.html), using the WebAssembly build of Csound to play most of the examples from the Web browser, has been updated with the latest sources for the manual.

## [csound-android](https://github.com/gogins/csound-android/releases)

A bug preventing writing to the filesystem on some devices has been fixed. Thanks to Karin Daum for helpful advice with this. The Csound for Android app on the Google Play Store has been updated with this fix.

A debuggable version of the Csound for Android app is now available on the GitHub release page. The app has also been updated on the [Google Play Store](https://play.google.com/store/apps/details?id=com.csounds.Csound6&hl=en)

The build system for the Csound for Android app has been considerably simplified and made easier to run. Old and redundant libraries and code have been removed.

## [michael.gogins.studio](https://github.com/gogins/michael.gogins.studio)
    
I am now hosting certain of my pieces in my personal GitHub repository for live performance in Web browsers. The first such piece is "Scrims v2" which updates Scrims, premiered at the New York City Electroacoustic Music Festival in 2016, for the current WebAssembly build of Csound as well as improved Csound instruments and more reliable performance. This is a piece of interactive visual music. Other pieces will follow. 

I have updated my "live talk" on algorithmic composition, originally given at the National University of Quilmes in Buenos Aires in 2018, to use the current version of my build of Csound for WebAssembly, and I have added a new live talk on my implementations of JavaScript interfaces for Csound in csound.node, WebAssembly, and Android.
    
Regards,
Mike