#!/usr/bin/python3
# -*- coding: utf-8 -*-
import sys
import random
import time
import math

import gi
from gi.repository import GObject
from gi.repository import GLib
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk 
gi.require_version("Gst", "1.0")
from gi.repository import Gst 
gi.require_version("WebKit2", "4.0")
from gi.repository import WebKit2
import ctcsound

Gst.init(sys.argv)

title = "Playpen"
print("title:", title)

builder = Gtk.Builder()
builder.add_from_file("playpen.glade")

def on_new_button_clicked(button):
    print(button.get_label())
    
def on_open_button_clicked(button):
    print(button)
    
def on_save_button_clicked(button):
    print(button)
    
def on_save_as_button_clicked(button):
    print(button)
    
def on_play_audio_button_clicked(button):
    print(button)
    
def on_render_soundfile_button_clicked(button):
    print(button)
    
def on_stop_button_clicked(button):
    print(button)
    

main_window = builder.get_object("main_window")
main_window.connect("destroy", Gtk.main_quit)
html_window = builder.get_object("html_window")
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
main_window.show_all() 

Gtk.main()