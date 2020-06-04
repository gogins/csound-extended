/*
 * C S O U N D
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
 */
#ifndef SILENCE_H
#define SILENCE_H

 /**
 * \page csoundac CsoundAC
 *
 * CsoundAC is a C++ class library for algorithmic music
 * composition with Csound.
 *
 * CsoundAC has C++, Java, Python, and Lua interfaces.
 *
 * CsoundAC implements the idea of a "music graph," similar to a scene
 * graph in 3-dimensional computer graphics. A composition is constructed
 * by assembling a directed acyclic graph of Node objects, which may generate
 * or transform Events that are collected in a Score. Each Node may have its
 * own local transformation of coordinagte system. The music graph
 * represents a model of a musical score. The Composition class provides
 * an abstract base class for managing a music graph. The MusicModel class
 * subclasses the Composition class to include the ability to store and
 * run a Csound orchestra.
 *
 * See the <a href="namespacecsound.html">csound</a>
 * namespace for information about the classes in CsoundAC.
 */
 /**
 * \namespace csound
 *
 * The csound namespace contains classes for doing algorithmic composition,
 * and for rendering audio from algorithmically generated scores,
 * especially using Csound.
 *
 * All classes declared for CsoundAC should be #included in Silence.hpp.
 *
 * SWIG is run on Silence.hpp to generate wrappers for all CsoundAC classes
 * in other languages, especially scripting languages such as Python,
 * Therefore, all framework headers must be included in this header,
 * and all framework headers must use #ifdef SWIG to declare
 * the module and make other SWIG declarations (see Node.h for an extensive example).
 * The order of declaration is important to SWIG!
 *
 * It is also expected that doxygen will be used to generate documentation
 * from comments in the framework header files.
 */
 /** \page csound Csound
 *
 * Csound is a sound and music computing system. It was originally written
 * by Barry Vercoe at the Massachusetts Institute of Technology in
 * 1984 as the first C language version of this type of
 * software. Since then Csound has received numerous contributions
 * from researchers, programmers, and musicians from around the world.
 *
 * \section silence_section_api_outline Outline of the API
 *
 * \subsection silence_section_api_apilist The Csound Application Programming Interfaces
 *
 * The Csound Application Programming Interface (API) reference is contained
 * herein.
 * The Csound API actually consists of several APIs:
 *
 * - The basic Csound C API. Include csound.h and link with libcsound.a.
 *   This also includes the Cscore API (see below).
 * - The basic Csound C++ API. Include csound.hpp and link with libcsound.a.
 * - The interfaces API, includes a number of auxiliary C++ classes, which
 *   add functionality and support the wrapping of the Csound API by various
 *   languages (e.g. Python, Java, Lua).
 *
 * \b Purposes
 *
 * The purposes of the Csound API are as follows:
 *
 * \li Declare a stable public application programming interface (API)
 *     for Csound in csound.h. This is the only header file that needs
 *     to be \#included by users of the Csound API.
 *
 * \li Hide the internal implementation details of Csound from users of
 *     the API, so that development of Csound can proceed without affecting
 *     code that uses the API.
 *
 * \b Users
 *
 * Users of the Csound API fall into two main categories: hosts and plugins.
 *
 * \li Hosts are applications that use Csound as a software synthesis engine.
 *     Hosts can link with the Csound API either statically or dynamically.
 *
 * \li Plugins are shared libraries loaded by Csound at run time to implement
 *     external opcodes and/or drivers for audio or MIDI input and output.
 *     Plugin opcodes need only include the csdl.h header which brings all
 *     necessary functions and data structures.
 *     Plugins can be written in C or C++. For C++, OOP support is given through
 *     `include/plugin.h` (using the Csound allocator, for opcodes
 *     that do not involve standard C++ library collections) or
 *     `include/OpcodeBase.hpp` (using the standard ++ allocator, for opcodes
 *     that do use standard C++ library collections).
 *
 * \section silence_section_api_c_example Examples Using the Csound (host) API
 *
 * The Csound command--line program is itself built using the Csound API.
 * Its code reads (in outline) as follows:
 *
 * \code
 * #include "csound.h"
 *
 * int main(int argc, char **argv)
 * {
 *   void *csound = csoundCreate(0);
 *   int result = csoundCompile(csound, argc, argv);
 *   if(!result) {
 *     while(csoundPerformKsmps(csound) == 0){}
 *     csoundCleanup(csound);
 *   }
 *   csoundDestroy(csound);
 *   return result;
 * }
 * \endcode
 *
 * Csound code can also be supplied directly using strings, either as
 * a multi-section CSD (with the same format as CSD files) or
 * directly as a string. It can be compiled any number of times
 * before or during performance.
 *
 * \subsection silence_s1 Using a CSD text
 *
 * System options can be passed via the CSD text before the engine
 * is started. These are ignored in subsequent compilations.
 *
 * \code
 * #include "csound.h"
 *
 * const char *csd_text =
 *  "<CsoundSynthesizer> \n"
 *  "<CsOptions> -odac </CsOptions> \n"
 *  "<CsInstruments> \n"
 *  "instr 1 \n"
 *  " out(linen(oscili(p4,p5),0.1,p3,0.1)) \n"
 *  "endin \n"
 *  "</CsInstruments> \n"
 *  "<CsScore> \n"
 *  "i1 0 5 1000 440 \n"
 *  "</CsScore> \n"
 *  "</CsoundSynthesizer> \n";
 *
 * int main(int argc, char **argv)
 * {
 *   void *csound = csoundCreate(0);
 *   int result = csoundCompileCsdText(csound, csd_text);
 *   result = csoundStart(csound);
 *   while (1) {
 *      result = csoundPerformKsmps(csound);
 *      if (result != 0) {
 *         break;
 *      }
 *   }
 *   result = csoundCleanup(csound);
 *   csoundReset(csound);
 *   csoundDestroy(csound);
 *   return result;
 * }
 * \endcode
 *
 * \subsection silence_s2 Using Csound code directly.
 *
 * Options can be passed via csoundSetOption() before the engine starts.
 *
 * \code
 * #include "csound.h"
 *
 * const char *orc_text =
 *  "instr 1 \n"
 *  " out(linen(oscili(p4,p5),0.1,p3,0.1)) \n"
 *  "endin \n";
 *
 * const char *sco_text = "i1 0 5 1000 440 \n";
 *
 * int main(int argc, char **argv)
 * {
 *   void *csound = csoundCreate(0);
 *   int result = csoundSetOption(csound, "-d");
 *   result = csoundSetOption(csound, "-odac");
 *   result = csoundStart(csound);
 *   result = csoundCompileOrc(csound, orc_text);
 *   result = csoundReadScore(csound, sco_text);
 *   while (1) {
 *      result = csoundPerformKsmps(csound);
 *      if (result != 0) {
 *         break;
 *      }
 *   }
 *   result = csoundCleanup(csound);
 *   csoundReset(csound);
 *   csoundDestroy(csound);
 *   return result;
 * }
 * \endcode

 *
 * Everything that can be done using C as in the above examples can also be done
 * in a similar manner in Python or any of the other Csound API languages.
 *
 * \file csound.h
 *
 * \brief Declares the public Csound application programming interface (API).
 * \author John P. ffitch, Michael Gogins, Matt Ingalls, John D. Ramsdell,
 *         Istvan Varga, Victor Lazzarini, Andres Cabrera and Steven Yi.
 *
 * Hosts using the Csound API must \#include <csound.h>, and link with the
 * Csound API library. Plugin libraries should \#include <csdl.h> to get
 * access to the API function pointers in the CSOUND structure, and do not
 * need to link with the Csound API library.
 * Only one of csound.h and csdl.h may be included by a compilation unit.
 *
 * Hosts must first create an instance of Csound using the \c csoundCreate
 * API function. When hosts are finished using Csound, they must destroy the
 * instance of csound using the \c csoundDestroy API function.
 * Most of the other Csound API functions take the Csound instance as their
 * first argument.
 * Hosts can only call the standalone API functions declared in csound.h.
 *
 * Here is the complete code for the simplest possible Csound API host,
 * a command-line Csound application:
 *
 * \code
 *
 * #include <csound.h>
 *
 * int main(int argc, char **argv)
 * {
 *     CSOUND *csound = csoundCreate(NULL);
 *     int result = csoundCompile(csound, argc, argv);
 *     if (!result)
 *       result = csoundPerform(csound);
 *     csoundDestroy(csound);
 *     return (result >= 0 ? 0 : result);
 * }
 *
 * \endcode
 *
 * All opcodes, including plugins, receive a pointer to their host
 * instance of Csound as the first argument. Therefore, plugins MUST NOT
 * compile, perform, or destroy the host instance of Csound, and MUST call
 * the Csound API function pointers off the Csound instance pointer.
 *
 * \code
 * MYFLT sr = csound->GetSr(csound);
 * \endcode
 *
 * In general, plugins should ONLY access Csound functionality through the
 * API function pointers and public members of the #CSOUND_ structure.
 *
 * \section silence_section_licenses License
 *
 * \subsection silence_section_csound_license Csound
 *
 * Copyright (C) 2001-2013 John ffitch, Michael Gogins, Victor Lazzarini,
 *                         Steven Yi, Istvan Varga, Andres Cabrera
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
 */

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%begin %{
#include <cmath>
%}
%feature("autodoc", "1");
%include "typemaps.i"
%include "std_vector.i"
%{
#include <csound.h>
#include <string>
#include <vector>
#include <map>
#include <eigen3/Eigen/Dense>
%}
#else
#include <string>
#include <vector>
#include <map>
#include <eigen3/Eigen/Dense>
#endif

#include "Conversions.hpp"
#include "System.hpp"
#include "Event.hpp"
#include "Midifile.hpp"
#include "Score.hpp"
#include "ChordSpace.hpp"
#include "Composition.hpp"
#include "Node.hpp"
#include "Counterpoint.hpp"
#include "CounterpointNode.hpp"
#include "ScoreNode.hpp"
#include "Cell.hpp"
#include "Rescale.hpp"
#include "ScoreModel.hpp"
#if !defined(EMSCRIPTEN)
#include "MusicModel.hpp"
#endif
#include "Sequence.hpp"
#include "Random.hpp"
#if !defined(EMSCRIPTEN)
#include "ImageToScore.hpp"
#endif
#include "StrangeAttractor.hpp"
#include "Lindenmayer.hpp"
#include "MCRM.hpp"
#include "Soundfile.hpp"
#include "Voicelead.hpp"
#include "VoiceleadingNode.hpp"
#include "ChordLindenmayer.hpp"

#endif
