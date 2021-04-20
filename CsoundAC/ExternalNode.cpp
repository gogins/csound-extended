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
#include "Event.hpp"
#include "ExternalNode.hpp"
#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <boost/process.hpp>
#include <boost/tokenizer.hpp>

namespace csound
{
    
static void parse_line(std::string line, Score &score) {
    std::vector<std::string> pfields;
    boost::tokenizer<> tokenizer(line);
    for(boost::tokenizer<>::iterator it = tokenizer.begin(); it != tokenizer.end(); ++it) {
        pfields.push_back(*it);
    }
    if (pfields.front() != "i") {
        return;
    }
    Event event;
    event.setStatus(144);
    /** 
     * Csound dimensions are assumed to be the same as for Event::toCsoundIStatement.
     */
    for (int i = 1, n = pfields.size(); i < n; ++i) {
        std::string pfield = pfields[i];
        double value = std::atof(pfield.c_str());
        switch(i) {
            case 1:
                event.setInstrument(value);
                break;
            case 2:
                event.setTime(value);
                break;
            case 3:
                event.setDuration(value);
                break;
            case 4:
                event.setKey(value);
                break;
            case 5:
                event.setVelocity(value);
                break;
            case 6:
                event.setDepth(value);
                break;
            case 7:
                event.setPan(value);
                break;
            case 8:
                event.setHeight(value);
                break;
            case 9:
                event.setPhase(value);
                break;
            case 10:
                event.setPitches(value);
                break;
        }
    }   
    score.push_back(event);
}

void ExternalNode::generate()
{
    score.clear();
    auto script_filename = std::tmpnam(nullptr);
    std::fstream stream;
    stream.open(script_filename, std::ios_base::in | std::ios_base::out | std::ios_base::binary);
    stream.write(script.data(), script.length());
    stream.close();
    auto program_path = boost::process::search_path(program);
    boost::process::ipstream stdout_stream;
    boost::process::child child_process(program, script_filename, boost::process::std_out > stdout_stream);
    std::string line;
    while (stdout_stream && std::getline(stdout_stream, line) && !line.empty()) {
        std::cerr << line << std::endl;
        parse_line(line, score);
    }
    child_process.wait();
    score.sort();
    if (duration != 0.0) {
        score.setDuration(duration);
    }
}

void ExternalNode::generate(Score &collectingScore)
{
    generate();
    for (int i = 0, n = score.size(); i < n; ++i) {
        collectingScore.push_back(score[i]);
        // TODO fix this hack... much work!
        collectingScore.back().process = score[i].process;
    }
}


}
