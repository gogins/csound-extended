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
#ifndef LISP_H
#define LISP_H

#include "Platform.hpp"
#include <limits>
#include <map>
#include <sstream>
#include "Lisp.hpp"
#include "Score.hpp"
#include "System.hpp"
#include <ecl/ecl.h>
#include <eigen3/Eigen/Dense>
#endif

namespace csound
{

void initialize_ecl(int argc, char **argv)
{
    static bool initialized_ecl = false;
    if (initialized_ecl) {
        return;
    } 
    initialized_ecl = true;
    cl_boot(argc, argv);
    atexit(cl_shutdown);
    System::inform("LispNode: initialized Embeddable Common Lisp.\n");
}
cl_object evaluate_form(const std::string &form) {
    return cl_eval(c_string_to_object(form.c_str()));
}

std::string to_std_string(cl_object lisp_object){
    std::string result;
    size_t length = 0;
    bool is_unicode = ECL_EXTENDED_STRING_P(lisp_object);
    if (is_unicode) {
        length = lisp_object->string.dim;
        for (size_t i = 0; i < length; ++i) {
            auto c = lisp_object->string.self[i];
            result += (char) c;
        }
    } else {
        length = lisp_object->base_string.dim;
        for (size_t i = 0; i < length; ++i) {
            auto c = lisp_object->base_string.self[i];
            result += (char) c;
        }
    }
    return result;
}
void seqToScore(cl_object &seq, Score &score)
{
    cl_object subobjects = ecl_slot_value(seq, "subobjects");
    cl_object size = cl_list_length(subobjects);
    for (long int i = 0, n = ecl_fixnum(size); i < n; ++i) {
        cl_object event = cl_nth(ecl_make_fixnum(i), subobjects);
        auto time_= ecl_to_double(ecl_slot_value(event, "time"));
        auto channel = ecl_to_double(ecl_slot_value(event, "channel"));
        auto keynum = ecl_to_double(ecl_slot_value(event, "keynum"));
        auto duration = ecl_to_double(ecl_slot_value(event, "duration"));
        auto amplitude = ecl_to_double(ecl_slot_value(event, "amplitude"));
        score.append(time_,
            duration,
            144.,
            1. + channel,
            keynum,
            127. * amplitude);
    }
}

cl_object scoreToSeq(Score &score, std::string seq_name)
{
    cl_object seq = evaluate_form(R"qqq(
(progn
    (defparameter score-from-children (new seq :name "score-from-children"))
    (list-objects score-from-children)
    score-from-children
)
)qqq");
    char event_form_buffer[0x200];
    // Common Music uses a plain Lisp list for a sequence. This makes it awkward to 
    // simply append envets from C++ to Lisp. So, we build up code that will 
    // create the list of events all in one go.
    std::stringstream stream;
    stream << "(setf (slot-value score-from-children 'subobjects) (list ";
    for (size_t i = 0, n = score.size(); i < n; ++i) {
        const Event &event = score[i];
        auto time_ = event.getTime();
        auto channel = event.getInstrument();
        auto keynum = event.getKey();
        auto duration = event.getDuration();
        auto amplitude = (event.getVelocity() / double(127));
        std::snprintf(event_form_buffer, 0x200, "(new midi :time  %f :channel %f :keynum %f :duration %f :amplitude %f)", time_, channel, keynum, duration, amplitude);
        stream << event_form_buffer;
    }
    stream << "))";
    auto code = stream.str();
    // std::cerr << code << std::endl;
    cl_object result = evaluate_form(code);
    return seq;
}

LispNode::LispNode()
{
}

LispNode::~LispNode()
{
}

void LispNode::appendTopLevelForm(const std::string top_level_form)
{
    top_level_forms.push_back(top_level_form);
}

std::vector<std::string> &LispNode::getTopLevelForms()
{
    return top_level_forms;
}

double LispNode::getNumberFromForm(const std::string &form)
{
    cl_object result = evaluate_form(form);
    double number = ecl_double_float(result);
    return number;
}

std::string LispNode::getStringFromForm(const std::string &form)
{
    cl_object result = evaluate_form(form);
    std::string string_ = to_std_string(result);
    return string_;
}

LispGenerator::LispGenerator()
{
}

LispGenerator::~LispGenerator()
{
}

void LispGenerator::generate(Score &score_from_this)
{
    System::inform("LispGenerator::generate...\n");
    cl_object result;
    for (auto it = top_level_forms.begin(); it != top_level_forms.end(); ++it) {
        // The final form must return the seq to be translated.
        result = evaluate_form(*it);
        System::debug("result type: %d\n", ecl_t_of(result));
    }
    seqToScore(result, score_from_this);
    System::inform("LispGenerator::generate.\n");
}

LispTransformer::LispTransformer()
{
}

LispTransformer::~LispTransformer()
{
}

void LispTransformer::transform(Score &score_from_children)
{
    System::inform("LispGenerator::transform...\n");
    System::inform("score_from_children size: %d\n", score_from_children.size());
    cl_object result;    
    // These forms must be evaluated to set up the environment 
    // before score_from_children can be translated to the  cm:seq
    // score-from-children.
    //result = evaluate_form("(require :asdf)");
    //result = evaluate_form("(require :nudruz)");
    //result = evaluate_form("(in-package :cm)");
    cl_object seq = scoreToSeq(score_from_children, "score-from-children");
    for (auto it = top_level_forms.begin(); it != top_level_forms.end(); ++it) {
        // The final form must return the seq to be translated back to the score.
        result = evaluate_form(*it);
    }
    // Replace the original score_from_children with the transformed version.
    score_from_children.clear();
    seqToScore(result, score_from_children);
    System::inform("LispGenerator::transform.\n");
}

}

