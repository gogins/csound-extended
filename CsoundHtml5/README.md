# Csound with HTML5

## License

This software is licensed under the terms of the GNU Lesser General Public
License, which is the same license as used for Csound.

## Introduction


CsoundHtml5 is a simple, efficient "front end" for editing and performing 
Csound pieces: standard .csd files, .csd files embedded <html> element, or 
.html pages in which Csound is embedded.

HTML files in CsoundHtml5 run in a Qt WebView browser in which Csound has 
been embedded in the form of a Csound object with many of the methods of the 
regular C++ Csound API. The JavaScript API methods and names are otherwise 
roughly the same as found in CsoundQt, Csound for Android, csound.node, and 
Csound for WebAssembly.

This page can be used to provide a highly customizable user interface for 
Csound pieces, to generate Csound scores using JavaScript, and to interface 
with all the myriad capabilities of HTML5, including for example animated 
3-dimensional graphics (WebGL). 

The CSD file also can contain a standard Web page, which appears as an <html>
element in the file. When this page is loaded in the browser tab of the
application, the running instance of Csound is accessible as a "csound"
object in the JavaScript context of the page. This object implements many of
the important methods of the Csound API. Although all of these methods are
asynchronous, methods that normally return values accept an additional
completion callback that will be called with the return value of the method
when it becomes available.

One major difference is that the Csound API for CsoundHtml5, which uses the 
QWebChannel for creating a proxy for the Csound shared library, is completely 
asynchronous. If a return value for an API call is required, it must be 
obtained by passing a callback function as the last parameter of the API call.

In practice, at least for most of my own pieces, even complex ones, a return 
value is simply not required, and with a few minor changes in coding practice 
all the Csound API calls can be made asynchronously. For example, see 
Scrims_qt.html in the csound directory, and compare it with Scrims_node.html. 

This project requires a recent version of the Qt SDK with QtWebEngine support.
The only external dependency is a recent version of Csound. CsoundHtml5 should
build and run on Windows, Linux, and OS X.
