# Computer Music Playpen

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

This computer music playpen is an integrated development environment for 
algorithmic music composition. 

The playpen is designed to provide the simplest possible system with the greatest 
possible power, and to minimize the time involved in moving through the "edit, 
render, listen; edit, render, listen;..." cycle of this style of composition 
without, however, compromising in _any_ way the musical possibilities of the 
software.

Python and the GTK framework are "glue" that binds everything together. The 
playpen provides a simple code editor, an interactive form builder for 
creating graphical user interfaces to control performances, an HTML window for 
presenting visual music, and online help.

It is possible to write a piece in Python using algorithmic composition 
libraries such as Rick Taube's [musx](https://github.com/musx-admin/musx) 
or my [CsoundAC](https://github.com/gogins/csound-extended); to write a Csound 
orchestra to render the piece; to write JavaScript for creating visual music 
in HTML5; to interactively build a fully-featured graphical user interface for 
controlling the audio and video of the piece using Glade; to instantly preview 
the piece in real-time audio and video; and to instantly render the piece to a 
file, normalize it, tag it, and translate it to final publication formats such 
as mp3, FLAC, or mp4.

In addition to Python and Csound, other programming languages can be used, 
either as the primary language or as a supplement, for example by running 
Javascript code in the WebKit browser build into the playpen, or by executing 
an external score-generating program directly from Csound using the <CsScore> 
`bin` attribute or, of course, simply by programming in Python to run whatever 
other composition software is needed to generate a score that is then sent to 
Csound or some other synthesizer for rendering.

The playpen can edit and automatically render pieces written in Python, Csound, 
HTML5, or Javascript. By writing a little extra Python code, pieces can be 
written in any programming language.

## Getting Started

Install the following software requirements, each according to its own 
instructions. Below are links to installation instructions for each 
package. Note that the instructions are often different for different 
operating systems.

1. [Python 3.9](https://www.python.org/downloads/) or, for Ubuntu Linux, 
   follow [these instructions](https://linuxize.com/post/how-to-install-python-3-9-on-ubuntu-20-04/).
3. [GTK 3](https://www.gtk.org/docs/installations/), note that GTK 4 is now 
   the current version but it is the previous version, GTK 3, that is used here.
4. [GStreamer 3](https://gstreamer.freedesktop.org/documentation/installing/index.html?gi-language=c).
5. _WebKit2 4.0 development libraries_; on Ubuntu Linux this is done by 
   executing `sudo apt install libwebkit2gtk-4.0-37` or a compatible version.
6. _WebKit2 GNU introspection libraries_; on Ubuntu Linux this 
   is done by executing `sudo apt install gir1.2-webkit2-4.0`.
6. [Glade](https://wiki.gnome.org/Apps/Glade); on Ubuntu Linux it may be 
   better to execute `sudo apt install glade`.
2. [PyGObject](https://pygobject.readthedocs.io/en/latest/getting_started.html) 
   which automatically generates Python bindings for GTK, GStreamer, and WebKit 
   based on their introspection libraries.
7. [Csound](https://csound.com/download.html), on Linux it is best to build 
   from source code. `Copy the ctcsound.py` file that comes with your version 
   of Csound to a directory where it can be loaded by Python.
8. The playpen itself is part of csound-extended. Clone the 
   [csound-extended](https://github.com/gogins/csound-extended) repository to 
   your local filesystem. Change to the `csound-extended/playpen` directory. 
   Execute `python3 playpen.py` and then open and run the `drone.py` example 
   to check that everything is working.

## User's Guide

## A Few Examples