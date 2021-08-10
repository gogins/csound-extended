# Computer Music Playpen

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

This computer music playpen is an integrated development environment 
__specifically designed to support my particular style of algorithmic music 
composition__.

I write music by writing code. I almost always use Csound to render my pieces 
as fixed media, that is, soundfiles. I may use C++, Python, Common Lisp, or 
JavaScript in addition to writing Csound code. I sometimes write interactive 
visual music. I generally find that the Csound instruments that I use need 
some pretty fine-tuned tweaking while the music is playing. What I need is to 
minimize the time spent in the "edit code, render piece, listen to piece; edit 
code again, render piece again, listen to piece again;..." cycle without, 
however, in any way limiting the power of Csound, the power of Python, or the 
power of HTML5.

In addition, long experience has taught me that keeping a finished piece as 
one file of source code, with a minimum of external dependencies, and those 
dependencies as standardized and widely used as possible (such as Csound, 
Python, and HTML5), is essential to ensure that pieces are maintainable and 
will play into the indefinite future. After all, I have been making computer 
music now for 38 years. Of course, this means that I often use a finished 
piece as a template for a new piece.

Experience has also taught me that all-in-one systems such as CsoundQt never 
actually do everything that I need, and may have bugs that I am unwilling or 
unable to fix. I find it is better to use a toolkit of widely available and 
well maintained components that I can tie together with shell scripts or 
Python code.

After exhaustively trying many alternatives I have ended up with the system 
that I maintain in this repository and, more specifically, in this directory.

1.  SciTE is the source code editor.
2.  I have added many custom commands for rendering Csound pieces, Python 
    pieces, Common Lisp pieces, C++ pieces, and HTML pieces.
3.  I have added custom commands for compiling C++ code.
4.  I have added custom commands for running a visual form builder for Python 
    pieces.
5.  It is easier to add custom commands in SciTE than it is in more widely 
    used source code editors such as Visual Studio Code.

If you compose in the same way that I do, by writing code for synthesis by 
Csound, you may well find this system useful. If you do not compose by writing 
code, you will probably _not_ find this system useful.

## Getting Started

Install the following software requirements, each according to its own 
instructions. I give some some specific instructions for Ubuntu Linux. However, 
to the best of my knowledge, all dependencies are available on Linux, MacOS, 
and Windows.

Core requirements for composing with Csound, including SciTE extensions for 
rendering Csound pieces and automatically post-processing them:

1.  [Csound](https://csound.com/download.html). For Linux, it is best to build 
    from source code according to [these instructions](https://github.com/csound/csound/blob/develop/BUILD.md).
2.  The SciTE text editor. On Ubuntu, execute `sudo apt install scite`.
3.  The SciTE user properties from this repository from [here]. Download this
    to your home directory or create in your home directory a symbolic link 
    to this file.
4.  [sox](http://sox.sourceforge.net/). For Ubuntu, execute 
    `sudo apt install sox` and `sudo apt install libsox-fmt-all`.
5.  The [libsndfile](http://www.mega-nerd.com/libsndfile/) utilities. For Ubuntu, all 
    you need is to execute `sudo apt install sndfile-programs`. For other 
    operating systems, you may need to install libsndfile itself which should 
    come with these utilities.
6.  [Audacity](https://www.audacityteam.org/). Please note, for Ubuntu, 
    install the regular Unbutu Packages Search result for Audacity, currently 
    `sudo apt install audacity`.
    
Additional requirements for composing with Python. Pleae note that everything 
should be installed for the __same version__ of Python. These instructions are for 
a global installation, but you can also install in your home directory or in a 
virtual environment.


7.  [Python 3.8](https://www.python.org/downloads/) or higher. For Ubuntu Linux, 
    follow [these instructions](https://linuxize.com/post/how-to-install-python-3-9-on-ubuntu-20-04/).
8.  Make sure that the `csound/interfaces/ctcsound.py` file 
    is the one that comes with your version of Csound and that it can be found 
    in your Python site-packages directory, e.g. 
    `/usr/local/lib/python3.9/dist-packages/ctcsound.py`. To use ctcsound you 
    also need numpy, which can be installed with 
    `sudo python3.8 -m pip install numpy`.
9.  [PySide6](https://www.gtk.org/docs/installations/). Execute 
    `sudo python3.8 -m pip PySide6`.
    
Additional requirements for composing with CsoundAC (usable from C++, Python, or 
JavaScript). These requirements also apply to composing with C++:

10. CsoundAC is part of my [csound-extended](https://github.com/gogins/csound-extended)
    repository. CsoundAC has advanced features for algorithmic composition, 
    including support for tonal harmony and counterpoint. Install if you can 
    or build if you must, according to instructions 
    [here](https://github.com/gogins/csound-extended).
    
Additional requirements for composing with HTML5:

11. Install if you can or build if you must Csound for NW.js according to 
    instructions [here](https://github.com/gogins/csound-extended-node).
12. Install the SDK version of [NW.js](https://nwjs.io/).
13. Add the directory containing the `csound.node` binary to your `NODE_PATH` 
    environment variable.
    
Additional useful things:

14. [csound-examples](https://github.com/gogins/csound-vst3-opcodes) is my 
    extensive compilation of Csound examples and pieces.
15. [michael.gogins.studio] contains the source code for my own compositions,
    and includes Csound instruments that could be used in your own work.
16. [csound-vst3-opcodes](https://github.com/gogins/csound-vst3-opcodes) 
    enables Csound to use VST3 plugins.
17. [csound-extended-nudruz](https://github.com/gogins/csound-extended-nudruz) 
    provides Csound suppport for composing in 
    Common Lisp using Drew Krause's extensions to Common Music.
18. [musx](https://github.com/musx-admin/musx) is Rick Taube's translation of 
    his original Common Lisp system for algorithmic composition to Python, now 
    adapted to work seamlessly with Csound.
19. [MuseScore](https://musescore.org/en) is a open source, cross-platform 
    score editor.
20. [lilypond](http://lilypond.org/) is an open source, cross-platform, very 
    powerful music typesetting system.
    
## A Few Examples

### xanadu.csd

This is a high-resolution version of Joseph Kung's "Xanadu" piece, often used 
as an introduction to Csound. It's the simplest possible test that your 
installation of the playpen is working. Just load the file and click on the 
__Play__ button.

### sierpinski-csound.py

For this example, install Rick Taube's Python port of the Common Music 
algorithmic composition system, [musx](https://github.com/musx-admin/musx).

### message.html

This is a basic HTML5 example that runs native Csound using JavaScript and 
has a custom user interface implemented with JQuery. In this case, the __Play__
button is part of the HTML5 user interface.

## User Guide

The philosophy of the playpen is to keep it simple, stupid and yet to have 
access to all the power of Csound, Python, and HTML5 in one editor, along 
with a built-in visual user interface designer.

The playpen expects each computer music piece to be one single source code 
file. This can be a Csound CSD file, a Python file, or an HTML file. If the 
user defines a user interface for the piece, that is stored in a `piece.ui` 
file, and the values of the widgets for Csound control channels are stored 
in a `piece.ui.channels` file.

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
    
### Panes

The playpen has the following panes or tabs:

1.  __Code__, a GtkSourceView code editor with customizable scheme, syntax 
    coloring, and search and replace widgets.
2.  __Controls__, containing user-defined controls for controlling the 
    Csound performance.
3.  __HTML__, a WebKit2 browser for displaying a Web page defined by an 
    HTML piece, or programmatically through Python code. The Web page 
    can also contain user-defined controls for controlling Csound.
4.  __Help__, displaying this README file.
4.  __Csound help__, displaying the current online version of the **__Csound 
    Reference Manual.__**

### Toolbar

The buttons on the top toolbar have the following functions, from left to 
right:

1.  Create a new piece, you will be prompted for the file location and name.
2.  Open an existing piece, any associated user interface definition will 
    be opened along with the piece. Any control widgets will be restored 
    with their last saved values.
3.  Save the piece to its file. The current control widget *values* will also  
    be saved in the piece's ui file.
4.  Save the _current state_ of any user-defined controls to the ui file.
5.  Save the piece to a new file, you will be prompted for the new file 
    location and name. The current control widget *values* will also be 
    saved in the new piece's ui file.
6.  Open the user interface designer, which is actually the Glade program 
    for designing Gtk user interfaces. Remember to save your work before 
    exiting Glade. The changes you have saved will immediately show up in 
    the __**Controls**__ pane of the playpen.
7.  Save the current values of all widgets associated with Csound control 
    channels.
8.  Play the piece to real-time audio (applies only to CSD and Python pieces,
    HTML pieces must supply their own play button or buttons).
9.  Render the piece to a soundfile (applies only to CSD pieces). 
    When the rendering is complete, the soundfile will be normalized, 
    tagged with metadata from `setting.ini`, translated to MP3, FLAC, and MP4 
    (suitable for YouTube) formats; finally the normalized soundfile will be 
    opened in a soundfile editor.
10. Stop the Csound performance (applies to CSD and Python pieces, not to 
    HTML pieces, which must provide their stop button or other means of 
    stopping the performance).
12. Control the verbosity of the runtime messages printed by Python; enabled 
    means print all messages, disabled means print only messages at the 
    Warning or higher levels. Note that the Csound options must be used to 
    control the verbosity of Csound's runtime messages.
    
### User Interface Builder

When using the user interface builder (which is actually the Glade 
application), there are some things that must be understood:

1.  The user interface shown in Glade must have a top widget that is a layout.
    The default is a Grid layout. You can delete this and replace it with any 
    other layout, but you must give it the id "user_controls_layout."
2.  The Scale widget is the usual choice for controlling variables in Csound.
    Each Scale widget must have its own associated Adjustment. The Adjustment 
    can be given minimum and maximum values to match the range needed 
    in Csound. Both the name, and the id, of the Scale must be set to the same 
    value as the name of the global Csound variable that will be controlled by 
    that Scale.
3.  In the header of your Csound orchestra, create global variables with the 
    same names and types as the Gtk widgets you have created to control them. 
    Then just use the `chnexport` opcode to create the global variables and 
    associate them with control channels having the same names. This greatly 
    simplifies writing controllable Csound orchestras. Example:
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
    
### User-Defined Controls
    
The playpen can automatically connect, update, save, and restore the following 
types of user-defined controls:

1.  `GtkButton`
2.  `GtkCheckButton`
3.  `GtkEditable`
4.  `GtkRadioButton`
5.  `GtkScale`
6.  `GtkScaleButton`
7.  `GtkSpinButton`
8.  `GtkSwitch`
9.  `GtkToggleButton`
10. `GtkVolumeButton`

The current values of these widgets will also be saved in a 
`piece.ui.channels` file for the piece when the user clicks on the "Save 
control values" button in the toolbar, and those values will be restored 
when the piece is reopened.

How this works is that signals from these wigets are automatically connected 
to a handler that sends an appropriate control channel value update to 
Csound, as long as the user-defined name of that widget is the same as the 
name of a global variable that is defined by and exported from the Csound 
orchestra using the `chnexport` opcode.

This is not a limitation. Any _other_ widgets and signals can also be used to 
control the Csound performance, but the user must then define custom code for 
handling these widgets and signals.

For a Python piece, the user must write a little code to restore the Csound 
control channels from the saved values, after starting Csound but before 
actually performing, like this:

<pre>
global values_for_channels

csound.Start()
for channel, value in values_for_channels.items():
    csound.SetControlChannel(channel, value)
csound.Perform()
</pre>





