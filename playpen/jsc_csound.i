/*
* C S O U N D A C
*
* Copyright (c) 2001-2003 by Michael Gogins. All rights reserved.
*
* CsoundAC is a Python extension module for doing algorithmic
* composition, in one which one writes music by programming in
* Python. Musical events are points in music space with dimensions
* {time, duration, event type, instrument, pitch as MIDI key,
* loudness as MIDI velocity, phase, pan, depth, height, pitch-class
* set, 1}, and pieces are composed by assembling a hierarchical tree
* of nodes in music space. Each node has its own local transformation
* of coordinates in music space. Nodes can be empty, contain scores
* or fragments of scores, generate scores, or transform
* scores. CsoundAC also contains a Python interface to the Csound
* API, making it easy to render CsoundAC compositions using Csound.
*
* L I C E N S E
*
* This software is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This software is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this software; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*
* swig -c++ -javascript -I/usr/local/include -I/usr/local/include/csound -I/usr/include/webkitgtk-4.0 -jsc -v jsc_csound.i
* 
* g++ -fPIC -shared jsc_csound_wrap.cxx -I/usr/include/webkitgtk-4.0 -I/usr/local/include/csound -lcsound64 -ljavascriptcoregtk-4.0 -lpthread
*/
#if defined(SWIGJAVASCRIPT)

%begin %{
#ifdef _MSC_VER
#define SWIG_PYTHON_INTERPRETER_NO_DEBUG
#endif
%}

%module csound
%{
    #include <string>
    #include <csound/csound.h>
    #include <csound/csound.hpp>
    #include <csound/csound_threaded.hpp>
    #include <glib-2.0/glib.h>
    #include <glib-2.0/gmodule.h>
    #include <libsoup-2.4/libsoup/soup.h>
    #include <libsoup-2.4/libsoup/soup-address.h>
    #include <gtk/gtk.h>
    #include <gtk/gtk.h>
    #include <webkit2/webkit-web-extension.h>
%}
%inline 
%{
    /** 
     * There isn't much code so we just stick it all in here.
     */

    #if (defined(WIN32) || defined(_WIN32)) && !defined(SWIG) && !defined(_MSC_VER)
    #  define SILENCE_PUBLIC __declspec(dllexport)
    #elif defined(__GNUC__) && (__GNUC__ >= 4) /* && !defined(__MACH__) */
    #  define SILENCE_PUBLIC        __attribute__ ( (visibility("default")) )
    #else
    #  define SILENCE_PUBLIC
    #endif

    /**
     * Wrap a subset of CsoundThreaded.
     */
    class JscCsound {
    protected:
        CsoundThreaded csound;
    public:
        JscCsound(){};
        JscCsound(CSOUND *other) {
            csound.SetCsound(other);
        };
        virtual ~JscCsound(){};
        virtual int Cleanup() {
            return csound.Cleanup();
        };
        virtual int CompileCsdText(const char *text) {
            return csound.CompileCsdText(text);
        };
    };
    
    extern "C" {

        void SILENCE_PUBLIC web_page_created_callback(WebKitWebExtension *extension,
                                   WebKitWebPage *web_page,
                                   const GVariant *user_data)
        {
            printf("web_page_created_callback: page %ld created for %s\n", 
                     webkit_web_page_get_id (web_page),
                     webkit_web_page_get_uri (web_page));
        }

        /**
         * Loads this extension when the page is created.
         */
        void SILENCE_PUBLIC webkit_web_extension_initialize_with_user_data(WebKitWebExtension *extension, GVariant *user_data)
        {
            printf("webkit_web_extension_initialize: %p %ld\n", extension, g_variant_get_int64(user_data));
            g_signal_connect (extension, "page-created", 
                              G_CALLBACK (web_page_created_callback), 
                              NULL);
        }    
    };
%}

#endif
