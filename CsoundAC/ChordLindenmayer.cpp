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
#include "ChordLindenmayer.hpp"
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
    while (stream.get(c)) {
        if (c == '(') {
            command.clear();
        } else if (c == ')') {
            interpret(command);
        } else if (c == ' ') {
            command.push_back(token);
            token.clear();
        } else {
            token.push_back(c);
        }
    }
}

void ChordLindenmayer::fixStatus() {
    for(std::vector<Event>::iterator it = score.begin(); it != score.end(); ++it) {
        if (it->getStatusNumber() == 0.0) {
            it->setStatus(144.0);
        }
    }
}

void ChordLindenmayer::tieOverlappingNotes() {
    score.tieOverlappingNotes();
}

void ChordLindenmayer::applyVoiceleadingOperations() {
    transform(score);
}

double ChordLindenmayer::equivalence(double &value, const std::string &equivalenceClass) const {
    if (equivalenceClass == "O") {
        value = Conversions::modulus(value, 12.0);
    } else if (equivalenceClass == "R") {
        value = Conversions::modulus(value, turtle.rangeSize);
    }
    return value;
}

/**
 * Returns a zero-based numerical index for a string
 * dimension name (for Events) or voice number (for Chords).
 */
static int getIndex (const std::string &dimension) {
    static std::map<std::string, int> indexesForDimensions = {
        {"t", Event::TIME},
        {"d", Event::DURATION},
        {"s", Event::STATUS},
        {"i", Event::INSTRUMENT},
        {"k", Event::KEY},
        {"v", Event::VELOCITY},
        {"p", Event::PHASE},
        {"x", Event::PAN},
        {"y", Event::DEPTH},
        {"z", Event::HEIGHT},
        {"m", Event::PITCHES},
        {"0", 0},
        {"1", 1},
        {"2", 2},
        {"3", 3},
        {"4", 4},
        {"5", 5},
        {"6", 6},
        {"7", 7},
        {"8", 8},
        {"9", 9},
        {"10", 10},
        {"11", 11},
        {"12", 12}
    };
    if (indexesForDimensions.find(dimension) != indexesForDimensions.end()) {
        return indexesForDimensions[dimension];
    } else {
        return -1;
    }
}

static void arithmetic(double &target, const std::string &operation, const std::string &x, const std::string equivalence = "0") {
}

static void sample(double &target, const std::string &distribution, const std::string &p1, const std::string &p2, const std::string &p3, const std::string &p4) {
}

static double real(const std::string &number) {
    return std::atof(number.c_str());
}

static double &element(Event &event, const std::string &stringIndex) {
    double value;
    int index = getIndex(stringIndex);
    if (index != -1) {
        value = event[index];
    }
    return value;
}

static double &voice(Chord &chord, const std::string &stringIndex) {
    double pitch;
    int voice = getIndex(stringIndex);
    if (voice != -1) {
        pitch = chord.getPitch(voice);
    }
    return pitch;
}

std::vector<double> parseVector(std::string text) {
    std::vector<double> result;
    std::string token;
    for (int i = 0; i < text.length(); ++i) {
        char c = text[i];
        if (c == '{') {
            continue;
        } else if (c == ',' || c == '}') {
            result.push_back(real(token));
        } else {
            token.push_back(c);
        }
    }
    return result;
}

void ChordLindenmayer::turtleOperation(const std::string &operation,
                                       const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "[") {
    } else if (operation == "]") {
    }
}

void ChordLindenmayer::noteOperation(const std::string &operation,
                                     const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "W") {
    } else if (operation == "F") {
    } else if (operation == "=") {
    } else if (operation == "+") {
    } else if (operation == "-") {
    } else if (operation == "*") {
    } else if (operation == "/") {
    } else if (operation == "uni") {
    } else if (operation == "nor") {
    } else if (operation == "bin") {
    } else if (operation == "nbi") {
    } else if (operation == "poi") {
    } else if (operation == "exp") {
    } else if (operation == "gam") {
    } else if (operation == "wei") {
    } else if (operation == "ext") {
    } else if (operation == "log") {
    } else if (operation == "chi") {
    } else if (operation == "cau") {
    } else if (operation == "fis") {
    } else if (operation == "stu") {
    }
}

void ChordLindenmayer::noteStepOperation(const std::string &operation,
                                         const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "=") {
    } else if (operation == "+") {
    } else if (operation == "-") {
    } else if (operation == "*") {
    } else if (operation == "/") {
    } else if (operation == "uni") {
    } else if (operation == "nor") {
    } else if (operation == "bin") {
    } else if (operation == "nbi") {
    } else if (operation == "poi") {
    } else if (operation == "exp") {
    } else if (operation == "gam") {
    } else if (operation == "wei") {
    } else if (operation == "ext") {
    } else if (operation == "log") {
    } else if (operation == "chi") {
    } else if (operation == "cau") {
    } else if (operation == "fis") {
    } else if (operation == "stu") {
    }
}

void ChordLindenmayer::noteOrientationOperation(const std::string &operation,
        const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "R") {
    }
}

void ChordLindenmayer::chordOperation(const std::string &operation,
                                      const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "W") {
    } else if (operation == "T") {
    } else if (operation == "I") {
    } else if (operation == "K") {
    } else if (operation == "Q") {
    } else if (operation == "+") {
    } else if (operation == "-") {
    } else if (operation == "*") {
    } else if (operation == "/") {
    } else if (operation == "++") {
    } else if (operation == "--") {
    } else if (operation == "uni") {
    } else if (operation == "nor") {
    } else if (operation == "bin") {
    } else if (operation == "nbi") {
    } else if (operation == "poi") {
    } else if (operation == "exp") {
    } else if (operation == "gam") {
    } else if (operation == "wei") {
    } else if (operation == "ext") {
    } else if (operation == "log") {
    } else if (operation == "chi") {
    } else if (operation == "cau") {
    } else if (operation == "fis") {
    } else if (operation == "stu") {
    }
}

void ChordLindenmayer::voicingOperation(const std::string &operation,
                                        const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "+") {
    } else if (operation == "-") {
    } else if (operation == "*") {
    } else if (operation == "/") {
    } else if (operation == "uni") {
    } else if (operation == "nor") {
    } else if (operation == "bin") {
    } else if (operation == "nbi") {
    } else if (operation == "poi") {
    } else if (operation == "exp") {
    } else if (operation == "gam") {
    } else if (operation == "wei") {
    } else if (operation == "ext") {
    } else if (operation == "log") {
    } else if (operation == "chi") {
    } else if (operation == "cau") {
    } else if (operation == "fis") {
    } else if (operation == "stu") {
    }
}

void ChordLindenmayer::modalityOperation(const std::string &operation,
        const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "T") {
    } else if (operation == "I") {
    } else if (operation == "K") {
    } else if (operation == "Q") {
    } else if (operation == "+") {
    } else if (operation == "-") {
    } else if (operation == "*") {
    } else if (operation == "/") {
    } else if (operation == "++") {
    } else if (operation == "--") {
    } else if (operation == "uni") {
    } else if (operation == "nor") {
    } else if (operation == "bin") {
    } else if (operation == "nbi") {
    } else if (operation == "poi") {
    } else if (operation == "exp") {
    } else if (operation == "gam") {
    } else if (operation == "wei") {
    } else if (operation == "ext") {
    } else if (operation == "log") {
    } else if (operation == "chi") {
    } else if (operation == "cau") {
    } else if (operation == "fis") {
    } else if (operation == "stu") {
    }
}

void ChordLindenmayer::scaleOperation(const std::string &operation,
                                      const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "+") {
    } else if (operation == "-") {
    } else if (operation == "*") {
    } else if (operation == "/") {
    } else if (operation == "C") {
    } else if (operation == "M") {
    }
}

void ChordLindenmayer::scaleDegreeOperation(const std::string &operation,
        const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "+") {
    } else if (operation == "-") {
    } else if (operation == "*") {
    } else if (operation == "/") {
    } else if (operation == "uni") {
    } else if (operation == "nor") {
    } else if (operation == "bin") {
    } else if (operation == "nbi") {
    } else if (operation == "poi") {
    } else if (operation == "exp") {
    } else if (operation == "gam") {
    } else if (operation == "wei") {
    } else if (operation == "ext") {
    } else if (operation == "log") {
    } else if (operation == "chi") {
    } else if (operation == "cau") {
    } else if (operation == "fis") {
    } else if (operation == "stu") {
    } else if (operation == "C") {
    }
}

void ChordLindenmayer::scoreOperation(const std::string &operation,
                                      const std::string &target, const std::vector<std::string> &command) {
    if        (operation == "=") {
    } else if (operation == "0") {
    } else if (operation == "C") {
    } else if (operation == "Cl") {
    } else if (operation == "S") {
    }
}

/**
 * The first element of the command is always the operation,
 * the second element is always the target. Not all operations
 * are defined for all targets, and not all operations have the same
 * parameters. The logic switches first on target, then on
 * operation, then on the remaining parameters of the command.
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

