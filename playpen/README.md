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
music now for 38 years. Of course, this means that I prefer to use a finished 
piece as a template for a new piece, rather than to create and use a library, 
because the template approach ensures the self-contained nature of the pieces.

Experience has also taught me that all-in-one systems such as CsoundQt never 
actually do everything that I need, and may have bugs that I am unwilling or 
unable to fix. I find it is better to use a toolkit of widely available and 
well maintained components that I can tie together with shell scripts or 
Python code.

After exhaustively trying many alternatives I have ended up with the system 
that I maintain in this repository and, more specifically, in this directory.

1.  SciTE is the source code editor, mainly because it is easier to add custom 
    commands in SciTE than it is in more widely used source code editors such 
    as Visual Studio Code.
2.  I have added many custom commands for rendering Csound pieces, Python 
    pieces, Common Lisp pieces, C++ pieces, and HTML pieces.
3.  I have added custom commands for compiling C++ code.
4.  I have added custom commands for running a visual form builder for Python 
    pieces.

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
    from source code according to [these  
    instructions](https://github.com/csound/csound/blob/develop/BUILD.md).
2.  The SciTE text editor. On Ubuntu, execute `sudo apt install scite`.
3.  Configuration files and helper scripts for SciTE in the 
    `csound-extended/playpen` directory. If you have cloned the 
    csound-extended repository, create symbolic links in your home directory 
    to the following files in the `csound-extended/playpen` directory. 
    Alternatively, download the files to your home directory. You need to 
    customize at least `playpen.ini`, and perhaps others, for your system.
    
    [.SciTEUser.properties](https://github.com/gogins/csound-extended/blob/develop/playpen/.SciTEUser.properties)
    [patch_calibrator.py](https://github.com/gogins/csound-extended/blob/develop/playpen/patch_calibrator.py)
    [playpen.ini](https://github.com/gogins/csound-extended/blob/develop/playpen/playpen.ini)
    [post-process.py](https://github.com/gogins/csound-extended/blob/develop/playpen/post-process.py)
    [run_nwjs_application.sh](https://github.com/gogins/csound-extended/blob/develop/playpen/run_nwjs_application.sh)

4.  [sox](http://sox.sourceforge.net/). For Ubuntu, execute 
    `sudo apt install sox` and `sudo apt install libsox-fmt-all`.
5.  The [libsndfile](http://www.mega-nerd.com/libsndfile/) utilities. For Ubuntu, all 
    you need is to execute `sudo apt install sndfile-programs`. For other 
    operating systems, you may need to install libsndfile itself, which should 
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
9.  [GTK 3](https://www.gtk.org/docs/installations/), note that GTK 4 is now 
    the current version but it is the previous version, GTK 3, that is used here.
10. [PyGObject](https://pygobject.readthedocs.io/en/latest/getting_started.html) 
    which automatically generates Python bindings for GTK based on introspection 
    libraries.
11. [Glade](https://wiki.gnome.org/Apps/Glade). For Ubuntu Linux, execute 
    `sudo apt install glade`.

Additional requirements for composing with CsoundAC (usable from C++, Python, or 
JavaScript). These requirements also apply to composing with C++:

12. CsoundAC is part of my [csound-extended](https://github.com/gogins/csound-extended)
    repository. CsoundAC has advanced features for algorithmic composition, 
    including support for tonal harmony and counterpoint. Install if you can 
    or build if you must, according to instructions 
    [here](https://github.com/gogins/csound-extended).
    
Additional requirements for composing with HTML5:

13. Install if you can or build if you must Csound for NW.js according to 
    instructions [here](https://github.com/gogins/csound-extended-node).
14. Install the SDK version of [NW.js](https://nwjs.io/).
15. Add the directory containing the `csound.node` binary to your `NODE_PATH` 
    environment variable.
    
Additional useful things:

16. [csound-examples](https://github.com/gogins/csound-vst3-opcodes) is my 
    extensive compilation of Csound examples and pieces.
17. [michael.gogins.studio](https://github.com/gogins/michael.gogins.studio) 
    contains the source code for my own compositions, and includes Csound 
    instruments that could be used in your own work.
16. [csound-vst3-opcodes](https://github.com/gogins/csound-vst3-opcodes) 
    enables Csound to use VST3 plugins.
18. [csound-extended-nudruz](https://github.com/gogins/csound-extended-nudruz) 
    provides Csound suppport for composing in 
    Common Lisp using Drew Krause's extensions to Common Music.
19. [musx](https://github.com/musx-admin/musx) is Rick Taube's translation of 
    his original Common Lisp system for algorithmic composition to Python, now 
    adapted to work seamlessly with Csound.
20. [MuseScore](https://musescore.org/en) is a open source, cross-platform 
    score editor.
21. [lilypond](http://lilypond.org/) is an open source, cross-platform, very 
    powerful music typesetting system.
    
## A Few Examples

### xanadu.csd

This is a high-resolution version of Joseph Kung's "Xanadu" piece, often used 
as an introduction to Csound. It's the simplest possible test that your 
installation of the playpen is working. Just load the piece into SciTE and use 
the _Tools_ menu _Render csd piece to audio_ item.

To produce a soundfile, normalize it, and translate it to different formats, 
just use the _Tools_ menu _Render csd piece to soundfile and post-process and 
play_ item.

### sierpinski-csound-gtk.py

For this example, install Rick Taube's Python port of the Common Music 
algorithmic composition system, [musx](https://github.com/musx-admin/musx).
Just load the piece into SciTE and use the _Tools_ menu _go_ item.

### message.html

This is a basic HTML5 example that runs native Csound using JavaScript and 
has a custom user interface implemented with JQuery. Just load the piece 
into SciTE and use the _Tools_ menu _go_ item.

## Native Csound for GTK

These libraries are not needed for the purposes outlined above, but you may 
find them useful. They provide a simplified version of the native Csound API 
for GTK. They include the `CsoundThreaded` Python extension module, which is 
an alternative to ctcsound, and the `libjsc_csound` shared library, which 
injects native Csound into the GTK WebView2, which is an alternative to NW.js. 
To use these libraries, change to the playpen directory and execute:
```
cmake .
make
sudo make install
```
