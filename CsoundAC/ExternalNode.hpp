#ifndef EXTERNAL_NODE_HPP_INCLUDED
#define EXTERNAL_NODE_HPP_INCLUDED
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

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%{
#include "Node.hpp"
#include "Score.hpp"
#include "ScoreNode.hpp"
#include <Eigen/Dense>
%}
#else
#include "Node.hpp"
#include "Score.hpp"
#include "ScoreNode.hpp"
#include <Eigen/Dense>
#endif

namespace csound
{
/**
 * ExternalNode runs a stored script with a specified command line, and 
 * imports Csound "i" statements printed by the script to stdout as CsoundAC 
 * Event objects in a CsoundAC Score. The format of the "i" statements must 
 * be the same as used in CsoundAC's Event::toCsoundIStatement method:
 * <pre>
 * p1  Csound instrument number.
 * p2  Time in seconds from the beginning of the score.
 * p3  Duration of the note in seconds.
 * p4  MIDI key number as a real number, may have a fractional part.
 * p5  MIDI velocity number as a real number.
 * p6  Spatial location depth (Ambisonic X axis).
 * p7  Spatial location width (Ambisonic Y axis, stereo pan).
 * p8  Spatial location height (Ambisonic Z axis).
 * p9  Audio phase in radians.
 * p10 Mason number, i.e. a pitch-class set as a sum of powers of 2.
 * </pre>
 * Lines of text read from stdout that do not begin with "i " are ignored.
 */
class SILENCE_PUBLIC ExternalNode :
    public ScoreNode
{
protected:
    std::string command;
    std::string script;
public:
    virtual void generate();
    virtual void generate(Score &collectingScore);
    virtual void setCommand(std::string command_);
    virtual std::string getCommand() const;
    virtual void setScript(std::string script_);
    virtual std::string getScript() const;
};

}

#endif
