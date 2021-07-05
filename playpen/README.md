# Computer Music Playpen

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

This computer music playpen is an integrated development environment for 
algorithmic music composition. 

The objective is provide the simplest possible system with the maximum 
possible power, and to minimize the time involved in moving through the "edit, 
render, listen; edit, render, listen;..." cycle of this style of composition 
without, however, compromising the musical possibilities of the software.

Python and the GTK framework are "glue" that binds everything together. The 
playpen provides a simple code editor, an interactive form builder for 
creating graphical user interfaces to control pieces, an HTML window for 
presenting visual music, and online help.

It is possible to write a piece in Python using algorithmic composition 
libraries such as Rick Taube's musx or my CsoundAC; to write a Csound 
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

## Getting Started

## User's Guide

## A Few Examples