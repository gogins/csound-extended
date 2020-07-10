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
#include "CppSound.hpp"
#include "ChordLindenmayer.hpp"
#include "Event.hpp"
#include "Score.hpp"
#include "Voicelead.hpp"
#include <cstring>
#include <iostream>
#include <iomanip>
#include <sstream>
#if defined(HAVE_IO_H)
#ifdef __linux__
#include <sys/io.h>
#else
#include <io.h>
#endif
#endif
#include <stdio.h>

namespace csound
{
ChordLindenmayer::ChordLindenmayer() :
    iterationCount(0),
    angle(1.0),
    beganAt(uintmax_t(0)),
    endedAt(uintmax_t(0)),
    elapsed(uintmax_t(0))
{
}

ChordLindenmayer::~ChordLindenmayer()
{
}

std::string ChordLindenmayer::getReplacement(std::string word)
{
    if(rules.find(word) == rules.end())
    {
        return word;
    }
    else
    {
        return rules[word];
    }
}

void ChordLindenmayer::generate()
{
    System::inform("ChordLindenmayer::generate...\n");
    System::inform("ChordLindenmayer::initialize...\n");
    initialize();
    System::inform("ChordLindenmayer::generateLindenmayerSystem...\n");
    generateLindenmayerSystem();
    System::inform("ChordLindenmayer::writeScore...\n");
    writeScore();
    System::inform("ChordLindenmayer::writeScore() wrote: %d events.\n", score.size());
    System::inform("ChordLindenmayer::tieOverlappingNotes...\n");
    tieOverlappingNotes();
    System::inform("ChordLindenmayer::applyVoiceleadingOperations...\n");
    applyVoiceleadingOperations();
    System::inform("ChordLindenmayer::tieOverlappingNotes...\n");
    tieOverlappingNotes();
    System::inform("ChordLindenmayer::fixStatus...\n");
    fixStatus();
    System::inform("ChordLindenmayer::generate ended with: %d events.\n", score.size());
}

void ChordLindenmayer::initialize()
{
    turtle.initialize();
    while(!turtleStack.empty()) {
        turtleStack.pop();
    }
    score.clear();
}

void ChordLindenmayer::generateLindenmayerSystem()
{
    std::stringstream source;
    std::stringstream target;
    std::string word;
    std::string rewrittenWord;
    target.str(axiom);
    for (int i = 0; i < iterationCount; i++) {
        source.str(target.str());
        source.clear();
        source.seekg(0);
        target.str("");
        target.clear();
        target.seekp(0);
        char c;
        while (source.get(c)) {
            if (c == '(') {
                word.clear();
            } else if (c == ')') {
                if(rules.find(word) == rules.end()) {
                    rewrittenWord = word;
                } else {
                    rewrittenWord = rules[word];
                }
                target << rewrittenWord;
            } else {
                word.append(1, c);
            }
        }
    }
    production = target.str();
}

void ChordLindenmayer::writeScore()
{
    std::string token;
    std::vector<std::string> command;
    std::stringstream stream(production);
    char c;
    while (stream.get(c)){
        if (c == '(') {
            command.clear;
        } else if (c == ')' {
            interpret(command);
        } else if (c == ' ') {
            command.push_back(token);
            token.clear();
        } else {
            token.push_back(c);
        }
    }
}

void ChordLindenmayer::fixStatus()
{
    for(std::vector<Event>::iterator it = score.begin(); it != score.end(); ++it) {
        if (it->getStatusNumber() == 0.0) {
            it->setStatus(144.0);
        }
    }
}

void ChordLindenmayer::tieOverlappingNotes()
{
    score.tieOverlappingNotes();
}

void ChordLindenmayer::applyVoiceleadingOperations()
{
    transform(score);
}

double ChordLindenmayer::equivalence(double &value, const std::string &equivalenceClass) const
{
    switch(equivalenceClass)
    {
    case "0":
    {
        value = Conversions::modulus(value, 12.0);
    }
    break;
    case "R":
    {
        value = Conversions::modulus(value, turtle.rangeSize);
    }
    break;
    }
    return value;
}

void ChordLindenmayer::turtleOperation(const std::string &operation, 
    const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "[") {
    } else if (operation == "]") {
    }
}

void ChordLindenmayer::noteOperation(const std::string &operation, 
    const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "[") {
    } else if (operation == "]") {
    }
}

void ChordLindenmayer::noteStepOperation(c(const std::string &operation, 
    const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "[") {
    } else if (operation == "]") {
    }
}

void ChordLindenmayer::noteOrientationOperation((const std::string &operation, 
    const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "[") {
    } else if (operation == "]") {
    }
}

void ChordLindenmayer::chordOperation((const std::string &operation, 
    const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "[") {
    } else if (operation == "]") {
    }
}

void ChordLindenmayer::scaleOperation((const std::string &operation, 
    const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "[") {
    } else if (operation == "]") {
    }
}

void ChordLindenmayer::scaleStepOperation((const std::string &operation, 
    const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "[") {
    } else if (operation == "]") {
    }
}

void ChordLindenmayer::scoreOperation((const std::string &operation, 
    const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "=") {
    }
}

/**
 * The first element of the command is always the operation, 
 * the second element is always the target. Not all operations
 * are defined for all targets, and not all operations have the same 
 * operands. The logic switches first on target, then on 
 * operation, then on the remaining elements of the command.
 */
void ChordLindenmayer::interpret(std::vector<std::string> command) {
    const std::string &operation = command[0];
    const std::string &target = command[1];
    if (target == "T") {
        turtleOperation(command);
    } else if target == "N") {
        noteOperation(command);
    } else if target == "S") {
        noteStepOperation(command);
    } else if target == "O") {
        noteOrientationOperation(command);
    } else if target == "C") {
        chordOperation(command);
    } else if target == "M") {
        modalityOperation(command);
    } else if target == "V") {
        voicingOperation(command);
    } else if target == "Sc") {
        scaleOperation(command);
    } else if target == "Sd") {
        scaleDegreeOperation(command);
    } else if target == "P") {
        scoreOperation(command);
    }
}

int ChordLindenmayer::getDimension (const std::string &dimension) const {
    switch(dimension) {
    case "i":
        return Event::INSTRUMENT;
    case "t":
        return Event::TIME;
    case "d":
        return Event::DURATION;
    case "k":
        return Event::KEY;
    case "v":
        return Event::VELOCITY;
    case "p":
        return Event::PHASE;
    case "x":
        return Event::PAN;
    case "y":
        return Event::HEIGHT;
    case "z":
        return Event::DEPTH;
    case "s":
        return Event::PITCHES;
    }
    return -1;
}

Eigen::MatrixXd ChordLindenmayer::createRotation (int dimension1, int dimension2, double angle) const
{
    Eigen::MatrixXd rotation_ = Eigen::MatrixXd::Identity(Event::ELEMENT_COUNT, Event::ELEMENT_COUNT);
    rotation_(dimension1,dimension1) =  std::cos(angle);
    rotation_(dimension1,dimension2) = -std::sin(angle);
    rotation_(dimension2,dimension1) =  std::sin(angle);
    rotation_(dimension2,dimension2) =  std::cos(angle);
    return rotation_;
}

double ChordLindenmayer::getAngle() const
{
    return angle;
}

void ChordLindenmayer::setAngle(double angle)
{
    this->angle = angle;
}

std::string ChordLindenmayer::getAxiom() const
{
    return axiom;
}

void ChordLindenmayer::setAxiom(std::string axiom)
{
    this->axiom = axiom;
}

void ChordLindenmayer::addRule(std::string command, std::string replacement)
{
    rules[command] = replacement;
}

int ChordLindenmayer::getIterationCount() const
{
    return iterationCount;
}

void ChordLindenmayer::setIterationCount(int count)
{
    iterationCount = count;
}

void ChordLindenmayer::clear()
{
    rules.clear();
    while(!turtleStack.empty()) {
        turtleStack.pop();
    }
    score.clear();
}

void ChordLindenmayer::generate(Score &collectingScore)
{
    generate();
    collectingScore.insert(collectingScore.end(), score.begin(), score.end());
}

}

