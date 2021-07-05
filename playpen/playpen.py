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



main_window = builder.get_object("main_window")
main_window.connect("destroy", Gtk.main_quit)
html_window = builder.get_object("html_window")
webview = WebKit2.WebView() 
webview.load_uri("http://csound.com/") 
html_window.add(webview);
help_window = builder.get_object("help_window")
helpview = WebKit2.WebView() 
helpview.load_uri("https://github.com/gogins/csound-extended/tree/develop/playpen") 
help_window.add(helpview);
main_window.show_all() 

Gtk.main()