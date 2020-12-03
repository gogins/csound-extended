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

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%{
#include <limits>
#include <map>
#include "Node.hpp"
#include <eigen3/Eigen/Dense>
%}
#else
#include <limits>
#include <map>
#include "Node.hpp"
#include <ecl/ecl.h>
#include <eigen3/Eigen/Dense>
#endif

namespace csound
{
    
#if !defined(SWIG)
    
template<typename T> struct is_cl_object { constexpr static bool p = false; };
template<> struct is_cl_object<cl_object> { constexpr static bool p = true; };
template<typename...> struct are_cl_objects { constexpr static bool p = true; };
template<typename Head, typename... Tail> struct are_cl_objects<Head, Tail...> {
constexpr static bool p = is_cl_object<Head>::p && are_cl_objects<Tail...>::p; };
    
/**
 * This function must be called with the arc and argv from main() before any 
 * Lisp code is executed.
 */
void initialize_ecl(int argc, char **argv);

/**
 * Evaluates a _SINGLE_ Lisp form. Please note, in Embeddable Common Lisp, 
 * `(require :xxx)` and some other forms work only if they are at the top 
 * level. That may necessitate repeated calls to this function from the 
 * embedding system.
 */
cl_object evaluate_form(const std::string &form);
    
/**
 * Translate a Lisp string to a C++ string.
 */
std::string to_std_string(cl_object lisp_string);
    
/**
 * Creates a DEFUN abstraction in C++.
 */
template <typename... Params>
void defun(const std::string &name, cl_object fun(Params... params)) {
    static_assert(are_cl_objects<Params...>::p, "ECL Functions may only take cl_object as argument.");
    ecl_def_c_function(c_string_to_object(name.c_str()), (cl_objectfn_fixed)fun, sizeof...(Params));
}

/**
 * Translates a Common Music seq to a Silence Score. All MIDI events 
 * in the seq are translated to Silence note on Events in the Score.
 */
void seqToScore(cl_object &seq_, Score &score);

/**
 * Translates a Silence Score to Common Music seq. All Silence note on Events 
 * in the Score to Common Music MIDI events in the seq. MIDI channel 0 is 
 * Csound insno 1.
 */
cl_object scoreToSeq(Score &score, std::string seq_name);

#endif
    
/**
 * Base class for Nodes that can use embedded Lisp code to generate or 
 * transform Events. In order to use the Common Music or nudruz packages to do 
 * this, first execute the following sequence of calls:
 * ```
 * csound::initialize_ecl(argc, (char **)argv);
 * csound::evaluate_form("(require :asdf)");
 * csound::evaluate_form("(require :nudruz)");
 * csound::evaluate_form("(in-package :cm)");
 * ```
 */
class SILENCE_PUBLIC LispNode :
    public Node
{
protected:
    std::vector<std::string> top_level_forms;
public:
    LispNode();
    virtual ~LispNode();
     /**
     * Sets the Lisp code that will generate or transform a Silence score.
     * Please note, each top-level form must be appended in sequence, e.g. 
     * `require` and `in-package` should be added before a `progn` containing 
     * score generating forms.
     */
    virtual void appendTopLevelForm(const std::string code);
    virtual std::vector<std::string> &getTopLevelForms();
    virtual double getNumberFromForm(const std::string &form);
    virtual std::string getStringFromForm(const std::string &form);
};

/**
 * Node that uses Lisp code to generate Events.
 */
class SILENCE_PUBLIC LispGenerator :
    public LispNode
{
public:
    LispGenerator();
    virtual ~LispGenerator();
    /**
     * To generate a score_from_this, the Lisp code should end by returning a 
     * Common Music seq object, which will be translated to score_from_this. 
     */
    virtual void generate(Score &score_from_this);
};

/**
 * Node that uses Lisp code to transform Events produced by child Nodes.
 */
class SILENCE_PUBLIC LispTransformer :
    public LispNode
{
public:
    LispTransformer();
    virtual ~LispTransformer();
    /**
     * To transform score_from_children, the Lisp code should operate on a 
     * Common Music seq instance named "score-from-children", which will be 
     * translated from score_from_children before transformation, and then 
     * translated back to score_from_children after transformation. Please 
     * note, before doing any of this, the asdf, nudruz, and cm packages 
     * will be loaded.
     */
    virtual void transform(Score &score_from_children);
};

}


