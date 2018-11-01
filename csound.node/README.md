# csound.node

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

[NW.js][nwjs], which used to be called Node-Webkit, is a system for building applications
for personal computers and workstations based on Web browser technology. Typically
the user interface is defined in HTML and the program logic is defined in JavaScript.
C++ addons, which expose JavaScript functions that call into C++ libraries, also can
be used. The application executes in an embedded Web browser based on Google Chrome.

`csound.node` is a C++ addon for NW.js that embeds [Csound][csound] into the JavaScript context
of Web pages running in NW.js. Such pages can call core methods of the Csound API
as member functions of a `csound` object that belongs to the window. The
`csound/examples/html` directory contains a number of examples that run in NW.js with
`csound.node` (the CSD examples will also run on Csound for Android or in CsoundQt).

Therefore, NW.js with `csound.node` can be used not only for composing and performing
Csound pieces, but also for developing standalone applications that incorporate Csound.
It can be used, for example, to develop sound art installations, visual music, or kiosk-type
applications.

The `NW_Csound_Demo.html` piece is an example of
an HTML file that embeds not only Csound, but also a Csound orchestra and score. The
`GameOfLife3D.csd` piece is an example of a CSD file that embeds a Web page in the
`<html>` element of the CSD file. See below for how to run these.

The motivation for `csound.node` should be obvious. It works on all personal computer
platforms, the build steps are simple, and the
end product makes all of the myriad capabilities of HTML5 available to Csound pieces,
including JavaScript, user-defined HTML user interfaces, 3-dimensional animated computer
graphics, and much more. For a full list of capabilities currently implemented in HTML5, see
[this HTML5 test page][html5test].

Please log any bug reports or requests for enhancements at https://github.com/gogins/csound-extended/issues.

## Usage

From the command line, execute `nw <directory>`, where the directory contains the
JSON-formatted manifest of a NW.js application based on an HTML file that embeds a
Csound piece. See the NW.js documention for more information on the manifest and
packaging NW.js applications. An example manifest (must be named package.json) for
running "Scrims_node.html" with nw and csound.node is:

<pre>
{
  "main": "Scrims_node.html",
  "name": "Scrims_node",
  "description": "Visual music for Csound and HTML5",
  "version": "0.1.0",
  "keywords": [ "Csound", "node-webkit" ],
  "nodejs": true,
  "node-remote": "http://<all-urls>/*",
  "window": {
    "title": "Scrims_node",
    "icon": "link.png",
    "toolbar": false,
    "frame": false,
    "maximized": true,
    "position": "mouse",
    "fullscreen": true
  },
  "webkit": {
    "plugin": true
  }
}
</pre>

To run your Csound pieces easily in nw, you can use a menu shortcut and script in
your text editor to automate the construction and deployment
of the manifest file, or you can use a template package.json and make a copy of your
piece with the name given in the manifest every time you want to run the piece.

# Installation

See the installation instructions in the csound-extended main README.md.

You may need to ensure that the csound.node shared libary is in a directory included 
in the NODE_PATH environment variable.

## Building

See the build instructions in the csound-extended main README.md.

[csound]: http://csound.github.io/
[nwjs]: http://nwjs.io/
[iojs]: https://iojs.org/en/index.html/
[msvs]: https://www.visualstudio.com/
[html5test]: https://html5test.com/
[gcc]: https://gcc.gnu.org/
[python]: http://www.python.org/
[nw-gyp]: https://github.com/nwjs/nw-gyp/
