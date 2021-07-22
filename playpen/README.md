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

Everything should be installed for the same version of Python.

1.  _An up to date C/C++ compiler_; on Ubuntu, execute 
    `sudo apt install build-essentials`.
1.  [SWIG wrapper and Interface Generator](http://swig.org/); on Ubuntu execute 
    `sudo apt install swig`.
2.  [Python 3.7](https://www.python.org/downloads/) or higher. For Ubuntu Linux, 
    follow [these instructions](https://linuxize.com/post/how-to-install-python-3-9-on-ubuntu-20-04/).
2.  [GTK 3](https://www.gtk.org/docs/installations/), note that GTK 4 is now 
    the current version but it is the previous version, GTK 3, that is used here.
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
    from source code.
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
    Execute `python3 playpen.py` and then open and run the `xanadu.csd` example 
    to check that everything is working.
    
You must download or build two shared libraries in this repository. On Linux, 
running `build.sh` should do the job. These shared libraries are:

1.  `CsoundThreaded`, a native Python module generated from `csound.i` by 
    the SWIG program. This provides a simplified implementation of the Csound 
    API that can be used from Python.
2.  `libjsc_csound.so`, a native loadable module (providing the same 
    simplified Csound API) that can be used by the JavaScriptCore engine in 
    the embedded WebKit2 browser.

## A Few Examples

### xanadu.csd

This is a high-resolution version of Kung's "Xanadu" piece often used as an 
introduction to Csound. It's the simplest possible test that your installation 
of the playpen is working.

### sierpinski-csound.py

For this example, install Rick Taube's Python port of the Common Music 
algorithmic composition system, [musx](https://github.com/musx-admin/musx).

## User Guide

The philosophy of the playpen is to keep it simple, stupid and yet to have 
access to all the power of Csound, Python, and HTML5 in one editor, along 
with a built-in visual user interface designer.

The playpen expects each computer music piece to be one single source code 
file. This can be a Csound CSD file, a Python file, or an HTML file.

1.  __CSD__ pieces can have an associated user interface definition (.glade 
    file).
2.  __Python__ pieces can also have an associated user interface definition.
    It is recommended to embed the complete Csound CSD file into the Python 
    file as a multi-line string variable. Python pieces have direct access 
    to native Csound through the CsoundThreaded.CsoundThread class.
3.  __HTML__ pieces run in a separate process in the WebKit2 browser and thus 
    cannot communicate with any user-defined user interface in the playpen 
    __Controls__ pane. HTML pieces also should embed the complete Csound CSD 
    file in a multi-line string or in an invisible TextView. HTML pieces 
    also have direct access to native Csound (not the WebAssembly build of 
    Csound) through the Csound class.

The playpen has the following panes or tabs:

1.  __Code__, a GtkSourceView code editor with customizable scheme, syntax 
    coloring, and search and replace widgets.
2.  __Controls__, containing user-defined controls for controlling the 
    Csound performance.
3.  __HTML__, a WebKit2 browser for displaying a Web page defined by an 
    HTML piece, or programmatically through Python code. The Web page 
    can also contain user-defined controls for controlling Csound.
4.  __Help__, displaying this README file.

The buttons on the top toolbar have the following functions, from left to 
right:

1.  Create a new piece, you will be prompted for the file location and name.
2.  Open an existing piece, any associated user interface definition will 
    be opened along with the piece.
3.  Save the piece to its file.
4.  Save the piece to a new file, you will be prompted for the new file 
    location and name.
5.  Open the user interface designer, which is actually the Glade program 
    for designing Gtk user interfaces. Remember to save your work before 
    exiting Glade. The changes you have saved will immediately show up in 
    the Controls pane of the playpen.
6.  Play the piece to real-time audio
7.  Render the piece to a soundfile. When the rendering is complete, the 
    soundfile will be normalized, tagged with metadata from `setting.ini`,
    translated to MP3, FLAC, and MP4 (suitable for YouTube) formats; 
    finally the normalized soundfile will be opened in a soundfile editor.

When using the user interface builder, there are some things that must be 
understood:

1.  The user interface shown in Glade must have a top widget that is a layout.
    The default is a Grid layout. You can delete this and replace it with any 
    other layout, but you must give it the id "user_controls_layout."
2.  The Scale widget is the usual choice for controlling variables in Csound.
    Each Scale widget must have its own associated Adjustment. The Adjustment 
    can be given a minimum and maximum value that match the range needed 
    in Csound. Both the name, and the id, of the Scale must be set to the same 
    value as the name of the global Csound variable that will be controlled by 
    that Scale.
3.  In the header of your Csound orchestra, create global variables with the 
    same names and types as the Gtk widgets you have created to control them. 
    Then use the `chnexport` opcode to create the global variables and 
    associate them with control channels having the same names. This greatly 
    simplifies writing Csound orchestras. Example:
    <pre>
    gk_Harpsichord_level init 0
    gk_Harpsichord_pan init .3
    gi_Harpsichord_release init .3
    gk_Harpsichord_pick init .275
    gk_Harpsichord_reflection init .75
    gi_Harpsichord_pluck init .5
    gk_Harpsichord_level chnexport "gk_Harpsichord_level", 3
    gk_Harpsichord_pan chnexport "gk_Harpsichord_pan", 3
    gi_Harpsichord_release chnexport "gi_Harpsichord_release", 3
    gk_Harpsichord_pick chnexport "gk_Harpsichord_pick", 3
    gk_Harpsichord_reflection chnexport "gk_Harpsichord_reflection", 3
    gk_Harpsichord_pluck chnexport "gi_Harpsichord_pluck", 3
    </pre>



