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

Python and the GTK framework are the "glue" that binds everything together. The 
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
instructions. There are some specific instructions for Ubuntu Linux. However, 
to the best of my knowledge, all dependencies are available on Linux, MacOS, 
and Windows.

1.  [Python 3.9](https://www.python.org/downloads/). For Ubuntu Linux, 
    follow [these instructions](https://linuxize.com/post/how-to-install-python-3-9-on-ubuntu-20-04/).
2.  [GTK 3](https://www.gtk.org/docs/installations/), note that GTK 4 is now 
    the current version but it is the previous version, GTK 3, that is used here.
3.  [GStreamer 3](https://gstreamer.freedesktop.org/documentation/installing/index.html?gi-language=c).
4.  _WebKit2 4.0 development libraries_; on Ubuntu Linux this is done by 
    executing `sudo apt install libwebkit2gtk-4.0-37` or a compatible version.
5.  _WebKit2 GNU introspection libraries_; on Ubuntu Linux this 
    is done by executing `sudo apt install gir1.2-webkit2-4.0`.
6.  [Glade](https://wiki.gnome.org/Apps/Glade). For Ubuntu Linux, execute 
    `sudo apt install glade`.
7.  [PyGObject](https://pygobject.readthedocs.io/en/latest/getting_started.html) 
    which automatically generates Python bindings for GTK, GStreamer, and WebKit 
    based on their introspection libraries.
8.  [Csound](https://csound.com/download.html). For Linux, it is best to build 
    from source code. `Copy the ctcsound.py` file that comes with your version 
    of Csound to a directory where it can be loaded by Python.
9.  [sox](http://sox.sourceforge.net/). For Ubuntu Linux, execute 
    `sudo apt install sox` and `sudo apt install libsox-fmt-all`.
10. [libsndfile](http://www.mega-nerd.com/libsndfile/). For Ubuntu Linux, all 
    you need is to execute `sudo apt install sndfile-programs`. For other 
    operating systems, you may need to install libsndfile itself which should 
    come with these utilities.
11. [Audacity](https://www.audacityteam.org/). Please note, for Ubuntu Linux, 
    install the regular Unbutu Packages Search result for Audacity, currently 
    `sudo apt install audacity`.
12. [markdown](https://pypi.org/project/Markdown/) to make the `README.md` file 
    more readable.
12. The playpen itself is part of my csound-extended repository. Clone the 
    [csound-extended](https://github.com/gogins/csound-extended) repository to 
    your local filesystem. Change to the `csound-extended/playpen` directory. 
    Execute `python3 playpen.py` and then open and run the `drone.py` example 
    to check that everything is working.

## User's Guide

## A Few Examples