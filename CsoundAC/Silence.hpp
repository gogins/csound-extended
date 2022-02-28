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
#pragma once
/**
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
#include <Eigen/Dense>
%}
#else
#include <string>
#include <vector>
#include <map>
#include <Eigen/Dense>
#endif
#include "Conversions.hpp"
#include "System.hpp"
#include "Event.hpp"
#include "Midifile.hpp"
#include "Score.hpp"
#include "ChordSpaceBase.hpp"
#include "ChordSpace.hpp"
#include "Composition.hpp"
#include "Node.hpp"
#include "Counterpoint.hpp"
#include "CounterpointNode.hpp"
#include "ScoreNode.hpp"
#include "Cell.hpp"
#include "HarmonyIFS.hpp"
#include "HarmonyIFS2.hpp"
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
#include "HarmonyIFS.hpp"
#if !defined(SWIGMAC) || !defined(__APPLE__)
#include "ExternalNode.hpp"
#endif
