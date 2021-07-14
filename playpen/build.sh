#!/bin/bash
echo Building Csound interface for JavaScriptCore...
swig -c++ -javascript -I/usr/local/include -I/usr/local/include/csound -I/usr/include/webkitgtk-4.0 -v8 -v jsc_csound.i
g++ -Wno-format-security -fpermissive -fPIC -shared jsc_csound_wrap.cxx -I/usr/include/atk-1.0 -I/usr/include/gdk-pixbuf-2.0 -I/usr/include/cairo -I/usr/include/harfbuzz -I/usr/include/pango-1.0 -I/usr/include/libsoup-2.4 -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/gtk-3.0 -I/usr/include/webkitgtk-4.0 -I/usr/local/include/csound -lcsound64 -ljavascriptcoregtk-4.0 -lpthread -olibjsc_csound.so
echo Finished building Csound interface for JavaScriptCore.