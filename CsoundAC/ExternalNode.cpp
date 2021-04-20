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
#include <stdio.h>
#include <cstring>
#include <iostream>
#include <fstream>
#include <boost/process.hpp>
#include <boost/tokenizer.hpp>
#include "System.hpp"

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

// TODO: This is POSIX-specific.

void ExternalNode::generate()
{
    score.clear();
    char script_filename[] = "/tmp/externalXXXXXX.py";
    auto file_descriptor = mkstemps(script_filename, 3);
    if (file_descriptor == -1) {
        System::error("ExternalNode::generate: Error: Could not create remporary file.\n");
    }
    System::inform("ExternalNode::generate: Created temporary script file: %s\n", script_filename);
    auto script_file = fdopen(file_descriptor, "w+");
    rewind(script_file);
    auto bytes_written = fwrite(getScript().c_str(), sizeof(getScript().front()), getScript().length(), script_file);
    System::inform("ExternalNode::generate: Wrote %d bytes.\n", bytes_written);
    boost::process::ipstream stdout_stream;
    char command_line[0x500];
    std::sprintf(command_line, "%s %s", getCommand().c_str(), script_filename);
    System::inform("ExternalNode::generate: Executing: %s\n", command_line);
    boost::process::child child_process(const_cast<const char *>(command_line), boost::process::std_out > stdout_stream);
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

void ExternalNode::setCommand(std::string command_) {
    command = command_;
}

std::string ExternalNode::getCommand() const {
    return command;
}

void ExternalNode::setScript(std::string script_) {
    script = script_;
}

std::string ExternalNode::getScript() const {
    return script;
}



}
