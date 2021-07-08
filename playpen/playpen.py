#!/usr/bin/python3
# -*- coding: utf-8 -*-
import math
import random
import sys
import time
import traceback

'''
A few strict conventions greatly simplify the code.

Each piece is always a single text file of code; it could be piece.csd, 
piece.py, or piece.html. The piece should specify a real-time audio output 
device and, if needed, an audio input and MIDI input and output. For rendering 
to a soundfile, the playpen program will write to piece.wav and post-process 
that file to produce piece.normalized.wav, piece.mp3, piece.flac, and piece.mp4;
piece.normalized.wav will then be opened for inspection in Audacity.

All user-defined Python graphical user interfaces are stored in a glade file 
with the same filename, i.e. my_piece.py goes with my_piece.glade, and the 
widget tree thus defined is rooted in the controls_layout widget. The user 
need write no code for handling UI events; the convention is that all UI 
events are handled in a single function that dispatches all widget event 
values to a Csound control channel with the same name as the widget.

'''

import gi
from gi.repository import GObject
from gi.repository import GLib
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk 
## gi.require_version("Gst", "1.0")
## from gi.repository import Gst 
gi.require_version("WebKit2", "4.0")
from gi.repository import WebKit2
gi.require_version("GtkSource", "3.0")
from gi.repository import GtkSource
import ctcsound

csound = ctcsound.Csound()

## Gst.init(sys.argv)

title = "Playpen"
print("title:", title)

builder = Gtk.Builder()
builder.add_from_file("playpen.glade")

filename = ""
piece = ""

def piece_is_csound():
    if filename.lower().endswith('.csd'):
        return True
    else:
        return False

def piece_is_python():
    if filename.lower().endswith('.py'):
        return True
    else:
        return False
    
def piece_is_html():
    if filename.lower().endswith('.html'):
        return True
    else:
        return False
    
def on_new_button_clicked(button):
    print(button.get_label())
    file_chooser_dialog = Gtk.FileChooserDialog(title="Please enter a filename", 
        parent=None, 
        action=Gtk.FileChooserAction.SAVE)
    file_chooser_dialog.add_buttons(
    Gtk.STOCK_CANCEL,
    Gtk.ResponseType.CANCEL,
    Gtk.STOCK_SAVE,
    Gtk.ResponseType.OK)
    file_chooser_dialog.run()
    filename = file_chooser_dialog.get_filename()
    print(filename)
    
def load(filename):
    print("loading:", filename)
    with open(filename, "r") as file:
        text = file.read()
        code_editor.get_buffer().set_text(text)

def on_open_button_clicked(button):
    print(button.get_label())
    file_chooser_dialog = Gtk.FileChooserDialog(title="Please enter a filename", 
        parent=None, 
        action=Gtk.FileChooserAction.OPEN)
    file_chooser_dialog.add_buttons(
    Gtk.STOCK_CANCEL,
    Gtk.ResponseType.CANCEL,
    Gtk.STOCK_SAVE,
    Gtk.ResponseType.OK)
    file_chooser_dialog.run()
    filename = file_chooser_dialog.get_filename()
    load(filename)
    file_chooser_dialog.close()
        
def on_save_button_clicked(button):
    print(button.get_label())
    buffer = code_editor.get_buffer()
    piece = buffer.get_text(buffer.get_start_iter(), buffer.get_end_iter(), True)
    print("piece:")
    print(piece)
    with open(filename, "w") as file:
        file.write(piece)
    
def on_save_as_button_clicked(button):
    print(button.get_label())
    file_chooser_dialog = Gtk.FileChooserDialog(title="Please enter a filename", 
        parent=None, 
        action=Gtk.FileChooserAction.SAVE)
    file_chooser_dialog.add_buttons(
    Gtk.STOCK_CANCEL,
    Gtk.ResponseType.CANCEL,
    Gtk.STOCK_SAVE,
    Gtk.ResponseType.OK)
    file_chooser_dialog.run()
    print(file_chooser_dialog.get_filename())
    buffer = code_editor.get_buffer()
    piece = buffer.get_text(buffer.get_start_iter(), buffer.get_end_iter(), True)
    print("piece:")
    print(piece)
    with open(filename, "w") as file:
        file.write(piece)

def on_play_audio_button_clicked(button):
    print(button.get_label())
    buffer = code_editor.get_buffer()
    piece = buffer.get_text(buffer.get_start_iter(), buffer.get_end_iter(), True)
    print("piece:")
    print(piece)
    if piece_is_csound():
        csound.compileCsdText(piece)
        csound.start()
        # Try to keep the UI responsive during performance.
        while csound.performBuffer() == 0:
            Gtk.main_iteration_do(False)
            message_count = csound.messageCnt()
            for message_index in range(message_count):
                message = csound.firstMessage()
                csound.popFirstMessage()
                sys.stdout.write(message)
        csound.stop()
        csound.cleanup()
        csound.reset()
    
def on_render_soundfile_button_clicked(button):
    print(button.get_label())
    buffer = code_editor.get_buffer()
    piece = buffer.get_text(buffer.get_start_iter(), buffer.get_end_iter(), True)
    print("piece:")
    print(piece)
    glade_file = "xanadu.glade"
    with open(glade_file, "r") as file:
        glade_xml = file.read()
        print("glade:", glade_xml)
        controls_window = builder.get_object("controls_window")
        print("controls_window:", controls_window)
        controls_layout.add(controls_window)
    if piece_is_csound():
        # Change output target here.
        csound.createMessageBuffer(False)
        csound.compileCsdText(piece)
        csound.start()
        # Try to keep the UI responsive during performance.
        while csound.performBuffer() == 0:
            Gtk.main_iteration_do(False)
            message_count = csound.messageCnt()
            for message_index in range(message_count):
                message = csound.firstMessage()
                csound.popFirstMessage()
                sys.stdout.write(message)
                messages_text_buffer.insert(messages_text_buffer.get_end_iter(), message, -1)
                #Gtk.main_iteration_do(False)
                end_iter = messages_text_buffer.get_end_iter()
                messages_text_view.scroll_to_iter(end_iter, 0, False, .5, .5)
                Gtk.main_iteration_do(False)
        csound.stop()
        csound.cleanup()
        csound.reset()
        # Post-process and edit here.
     
def on_stop_button_clicked(button):
    print(button.get_label())
    if piece_is_csound():
        print("Stopping csound...")
        csound.stop()
        csound.cleanup()
        csound.reset()
        print("Csound has been stopped and reset.")

def on_control_change(control):
    print("on_control_change", control)
    
def on_destroy(source):
    print("on_destroy: source:", source)
    csound.stop()
    csound.cleanup()
    csound.reset()
    Gtk.main_quit()
    
main_window = builder.get_object("main_window")
main_window.connect("destroy", on_destroy)
html_window = builder.get_object("html_window")
code_editor = builder.get_object("code_editor")
controls_layout = builder.get_object("controls_layout")
messages_text_view = builder.get_object("messages_text_view")
messages_text_buffer = messages_text_view.get_buffer()
webview = WebKit2.WebView() 
webview.load_uri("http://csound.com") 
html_window.add(webview);
help_window = builder.get_object("help_window")
helpview = WebKit2.WebView() 
helpview.load_uri("https://github.com/gogins/csound-extended/tree/develop/playpen") 
help_window.add(helpview)
main_window.resize(1200, 800)
new_button = builder.get_object("new_button")
new_button.connect("clicked", on_new_button_clicked)
open_button = builder.get_object("open_button")
open_button.connect("clicked", on_open_button_clicked)
save_button = builder.get_object("save_button")
save_button.connect("clicked", on_save_button_clicked)
save_as_button = builder.get_object("save_as_button")
save_as_button.connect("clicked", on_save_as_button_clicked)
play_audio_button = builder.get_object("play_audio_button")
play_audio_button.connect("clicked", on_play_audio_button_clicked)
render_soundfile_button = builder.get_object("render_soundfile_button")
render_soundfile_button.connect("clicked", on_render_soundfile_button_clicked)
stop_button = builder.get_object("stop_button")
stop_button.connect("clicked", on_stop_button_clicked)
main_window.show_all() 

if len(sys.argv) > 1:
    filename = sys.argv[1]
    load(filename)
Gtk.main()