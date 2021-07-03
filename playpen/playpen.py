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

Gst.init(sys.argv)

title = sys.argv[0].split(".")[0]
print("title:", title)
glade_file = title + ".glade"

builder = Gtk.Builder()
builder.add_from_file(glade_file)


webview = WebKit2.WebView() 
# print(help(webview))

scrolled_window = Gtk.ScrolledWindow() 
scrolled_window.add(webview) 

main_window = builder.get_object("main_window")
main_window.add(scrolled_window) 
main_window.connect("destroy", Gtk.main_quit)
main_window.show_all() 

webview.load_uri("http://w3.org/") 
Gtk.main()