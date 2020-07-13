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
#include <random>
#if defined(HAVE_IO_H)
#ifdef __linux__
#include <sys/io.h>
#else
#include <io.h>
#endif
#endif
#include <stdio.h>

#define DEBUGGING 1
#define INDEX_DEBUGGING 0

namespace csound
{
    
    
static std::mt19937_64 twister;

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
    production.clear();
    for (int i = 0; i < iterationCount; i++) {
        std::stringstream input;
#if DEBUGGING
        System::inform("ChordLindenmayer::generateLindenmayerSystem: iteration: %3d of %d\n", i + 1, iterationCount);
#endif
        if (i == 0) {
            input.str(axiom);
        } else {
            input.str(production);
        }
        char c;
#if DEBUGGING
        System::inform("  source:      %s\n", input.str().c_str());
#endif
        std::string word;
        std::string rewrittenWord;
        std::stringstream output;
        while (input.get(c)) {
            if (c == '(') {
                word.clear();
                word.append(1, c);
            } else if (c == ')') {
                word.append(1, c);
                if(rules.find(word) == rules.end()) {
                    rewrittenWord = word;
                } else {
                    rewrittenWord = rules[word];
                }
#if DEBUGGING
        System::inform("    replaced:  %s\n", word.c_str());
        System::inform("    with:      %s\n", rewrittenWord.c_str());
#endif
                output << rewrittenWord;
            } else {
                word.append(1, c);
            }
        }
        production = output.str();
#if DEBUGGING
        System::inform("  production:  %s\n", production.c_str());
#endif
    }
}

void ChordLindenmayer::writeScore()
{
    std::string token;
    std::vector<std::string> command;
    std::stringstream stream(production);
    // Turtle commands have the syntax of a Lisp function:
    // `(operation target parameter1 ...)`.
    char c;
    while (stream.get(c)) {
        if (c == '(') {
            token.clear();
            command.resize(0);
        } else if (c == ')') {
            command.push_back(token);
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

static bool getIndex(int &index, const std::string &dimension) {
    index = getIndex(dimension);
    if (index == -1) {
        return false;
    } else {
        return true;
    }
}

static double real(const std::string &number) {
    return std::atof(number.c_str());
}

static bool parseIndex(int &index, const std::string &target) {
    bool result = false;
    size_t openBrace = target.find('[');
    // The target should be treated as a vector.
    if (openBrace == std::string::npos) {
        index = -1;
    // The target should be treated as one element of a vector.
    } else {
        size_t start = openBrace + 1;
        size_t closeBrace = target.find(']');
        size_t length = closeBrace - openBrace - 1;
        std::string indexString = target.substr(start, length);
        index = getIndex(indexString);
        result = true;
    }
#if INDEX_DEBUGGING
    System::inform("parseIndex: %d %d %s\n", result, index, target.c_str());
#endif
    return result;
}

bool parseVector(std::vector<double> &elements, std::string text) {
    if (text.find('{') == std::string::npos) {
        return false;
    }
    elements.clear();
    std::string token;
    for (int i = 0; i < text.length(); ++i) {
        char c = text[i];
        if (c == '{') {
            continue;
        } else if (c == ',' || c == '}') {
            elements.push_back(real(token));
        } else {
            token.push_back(c);
        }
    }
    return true;
}

static void addVoice(Chord &chord) {
    chord = chord.eOP();
    chord.resize(chord.voices() + 1);
    chord.setPitch(chord.voices() - 1, chord.getPitch(0));
    chord = chord.eOP();
    
}

static void removeVoice(Chord &chord) {
    chord = chord.eOP();
    chord.resize(chord.voices() - 1);
    chord = chord.eOP();
}

void ChordLindenmayer::arithmetic(Chord &target, const std::string &operation, const std::string &targetString, const std::vector<std::string> &command) {
    int index;
    if (parseIndex(index, targetString)) {
        // Apply the operation to the indexed element of the target.
        arithmetic(target.getPitchReference(index), operation, targetString, command);
        equivalence(target.getPitchReference(index), command.back());
    } else {
        std::vector<double> elements;
        if (parseVector(elements, command[2])) {
            // Apply vector-vector arithmetic to the target.
            for (int i = 0, n = target.voices(); i < n; ++i) {
                arithmetic(target.getPitchReference(i), operation, targetString, elements[i], 0, 0, 0, 0);
                equivalence(target.getPitchReference(i), command.back());
            }
        } else {
            // Apply vector-scalar arithmetic to the target.
            for (int i = 0, n = target.voices(); i < n; ++i) {
                arithmetic(target.getPitchReference(i), operation, targetString, real(command[3]), 0, 0, 0, 0);
                equivalence(target.getPitchReference(i), command.back());
            }
        }
    }
}

void ChordLindenmayer::arithmetic(Event &target, const std::string &operation, const std::string &targetString, const std::vector<std::string> &command) {
    int index;
    double result;
    if (parseIndex(index, targetString)) {
#if DEBUGGING
        System::inform("ChordLindenmayer::arithmetic: index: %3d Event: %s\n", index, target.toString().c_str());
#endif
        // Apply the operation to the indexed element of the target.
        result = arithmetic(target[index], operation, targetString, command);
        if (index == 4) {
#if DEBUGGING
            target[index] = result;
            System::inform("  after arithmetic:           Event: %s\n", target.toString().c_str());
#endif
            equivalence(result, command.back());
        }
        target[index] = result;
#if DEBUGGING
        System::inform("  after equivalence:            Event: %s\n", target.toString().c_str());
#endif
    } else {
        std::vector<double> elements;
        if (parseVector(elements, command[2])) {
            // Apply vector-vector arithmetic to the target.
            for (int i = 0, n = target.size() - 1; i < n; ++i) {
                result = arithmetic(target[i], operation, targetString, elements[i], 0, 0, 0, 0);
                if (i == 4) {
                    equivalence(result, command.back());
                }
                target[i] = result;
            }
        } else {
            // Apply vector-scalar arithmetic to the target.
            for (int i = 0, n = target.size() - 1; i < n; ++i) {
                result = arithmetic(target[i], operation, targetString, command);
                if (i == 4) {
                    equivalence(result, command.back());
                }
                target[i] = result;
            }
        }
    }
}

double ChordLindenmayer::arithmetic(const double &target, const std::string &operation, const std::string &targetString, double p1, double p2, double p3, double p4, double p5) {
    double result = target;
    if        (operation == "=") {
        result = p1;
    } else if (operation == "+") {
        result = target + p1;
    } else if (operation == "-") {
        result = target - p1;
    } else if (operation == "*") {
        result = target * p1;
    } else if (operation == "/") {
        result = target / p1;
    } else if (operation == "uni") {
        std::uniform_real_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "nor") {
        std::normal_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "bin") {
        std::binomial_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "nbi") {
        std::negative_binomial_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "poi") {
        std::poisson_distribution<> distribution(p1);
        result = distribution(twister);
    } else if (operation == "exp") {
        std::exponential_distribution<> distribution(p1);
        result = distribution(twister);
    } else if (operation == "gam") {
        std::gamma_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "wei") {
        std::weibull_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "ext") {
        std::extreme_value_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "log") {
        std::lognormal_distribution<> distribution(p1, p2);
        result = distribution(twister);
     } else if (operation == "chi") {
        std::chi_squared_distribution<> distribution(p1);
        result = distribution(twister);
    } else if (operation == "cau") {
        std::cauchy_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "fis") {
        std::fisher_f_distribution<> distribution(p1, p2);
        result = distribution(twister);
    } else if (operation == "stu") {
        std::student_t_distribution<> distribution(p1);
        result = distribution(twister);
    }
#if DEBUGGING
        System::inform("ChordLindenmayer::arithmetic: %9.4f = %9.4f %s %9.4f %9.4f\n", result, target, operation.c_str(), p1, p2);
#endif
    return result;
}

double ChordLindenmayer::arithmetic(const double &target, const std::string &operation, const std::string &targetString, const std::vector<std::string> &command) {
    double p1 = real(command[2]);
    double p2 = 0;
    if (command.size() > 3) {
        p2 = real(command[3]);
    }
    double p3 = 0;
    if (command.size() > 4) {
        p3 = real(command[4]);
    }
    double p4 = 0;
    if (command.size() > 5) {
        p4 = real(command[5]);
    }
    double p5 = 0;
    if (command.size() > 6) {
        p5 = real(command[6]);
    }
    return arithmetic(target, operation, targetString, p1, p2, p3, p4, p5);
}

void ChordLindenmayer::turtleOperation(const std::string &operation,
                                       const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::turtleOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    if        (operation == "[") {
        turtleStack.push(turtle);
    } else if (operation == "]") {
        turtle = turtleStack.top();
        turtleStack.pop();
    }
}

void ChordLindenmayer::noteOperation(const std::string &operation,
                                     const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::noteOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    if        (operation == "W") {
        score.append(turtle.note);
    } else if (operation == "F") {
        auto x = real(command[2]);
        for (int i = 0, n = turtle.note.size() - 1; i <  n; ++i) {
            turtle.note[i] = turtle.note[i] + (x * turtle.step[i] * turtle.orientation[i]);
        }        
    } else {
        arithmetic(turtle.note, operation, target, command);
    }
}

void ChordLindenmayer::noteStepOperation(const std::string &operation,
                                         const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::noteStepOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    arithmetic(turtle.step, operation, target, command);
}

void ChordLindenmayer::noteOrientationOperation(const std::string &operation,
        const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::noteOrientationOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    if        (operation == "R") {
        int dimension1;
        if (getIndex(dimension1, command[2]) == false) {
            return;
        }
        int dimension2;
        if (getIndex(dimension2, command[3]) == false) {
            return;
        }
        double angle = real(command[4]);
        Eigen::MatrixXd rotation = createRotation(dimension1, dimension2, angle);	
        turtle.orientation = rotation * turtle.orientation;	
    }
}

void ChordLindenmayer::chordOperation(const std::string &operation,
                                      const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::chordOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    if        (operation == "W") {
        std::vector<double> ptv = Voicelead::chordToPTV(turtle.chord,	
                                  0,	
                                  turtle.rangeSize);	
        turtle.chord = Voicelead::ptvToChord(ptv[0],	
                                             ptv[1],	
                                             turtle.voicing,	
                                             0,	
                                             turtle.rangeSize);	
        for (size_t i = 0, n = turtle.chord.size(); i < n; ++i) {	
            Event event = turtle.note;	
            event.setKey(turtle.chord.getPitch(i));	
            score.append(event);	
        }        
    } else if (operation == "T") {
        double x = real(command[2]);
        turtle.chord = turtle.chord.T(x).eOP();
    } else if (operation == "I") {
        double x = real(command[2]);
        turtle.chord = turtle.chord.I(x).eOP();
    } else if (operation == "K") {
        turtle.chord = turtle.chord.K().eOP();
    } else if (operation == "Q") {
        double x = real(command[2]);
        turtle.chord = turtle.chord.Q(x, turtle.modality).eOP();
    } else if (operation == "++") {
        addVoice(turtle.chord);
    } else if (operation == "--") {
        removeVoice(turtle.chord);
    } else {
        arithmetic(turtle.chord, operation, target, command);
    }
}

void ChordLindenmayer::voicingOperation(const std::string &operation,
                                        const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::voicingOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    arithmetic(turtle.voicing, operation, target, command); 
}

void ChordLindenmayer::modalityOperation(const std::string &operation,
        const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::modalityOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    if        (operation == "T") {
        double x = real(command[2]);
        turtle.modality = turtle.modality.T(x).eOP();
    } else if (operation == "I") {
        double x = real(command[2]);
        turtle.modality = turtle.modality.I(x).eOP();
    } else if (operation == "K") {
        turtle.modality = turtle.modality.K().eOP();
    } else if (operation == "Q") {
        turtle.modality = turtle.modality.K().eOP();
    } else if (operation == "++") {
        addVoice(turtle.modality);
    } else if (operation == "--") {
        removeVoice(turtle.modality);
    } else {
        arithmetic(turtle.modality, operation, target, command);
    }
}

void ChordLindenmayer::scaleOperation(const std::string &operation,
                                      const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::scaleOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    if        (operation == "=") {
        std::vector<double> scale_tones;
        if (parseVector(scale_tones, command[3])) {
            turtle.scale = Scale(command[2], scale_tones);
        }
    } else if (operation == "C") {
        int degree = real(command[2]);
        int voices = real(command[3]);
        // Under implicit scale degree equivalence.
        while (degree < 1) {
            degree = degree + turtle.scale.voices();
        }
        while (degree > turtle.scale.size()) {
            degree = degree - turtle.scale.voices();
        }
        turtle.chord = turtle.scale.chord(degree, voices);
    } else if (operation == "M") {
        int scaleDegree = turtle.scale.degree(turtle.chord);
        int choice = real(command[3]);
        if (scaleDegree > 0) {
            int voices = real(command[2]);
            Chord chord = turtle.scale.chord(scaleDegree, voices);
            auto modulations = turtle.scale.modulations(chord, voices);
            if (modulations.size() > 0) {
                while (choice < 0) {
                    choice = choice + modulations.size();
                }
                while (choice >= modulations.size()) {
                    choice = choice - modulations.size();
                }
                turtle.scale = modulations[choice];
            }                
        }
    }
}

void ChordLindenmayer::scaleDegreeOperation(const std::string &operation,
        const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::scaleDegreeOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    double temporary = turtle.scaleDegree;
    arithmetic(temporary, operation, target, command);
    turtle.scaleDegree = temporary;
}

void ChordLindenmayer::scoreOperation(const std::string &operation,
                                      const std::string &target, const std::vector<std::string> &command) {
#if DEBUGGING
        System::inform("ChordLindenmayer::scoreOperation: %s %s\n", operation.c_str(), target.c_str());
#endif
    if        (operation == "0") {
        operations[turtle.note.getTime()].beginTime = turtle.note.getTime();	
    } else if (operation == "C") {
        chord(turtle.chord, turtle.note.getTime());
    } else if (operation == "Cl") {
        chordVoiceleading(turtle.chord, turtle.note.getTime(), true);
    } else if (operation == "S") {
        chord(turtle.scale, turtle.note.getTime());
    } else if (operation == "seed") {
        twister.seed(int(real(command[2])));
    }
}

/**
 * The first element of the command is always the operation,
 * the second element is always the target. Not all operations
 * are defined for all targets, and not all operations have the same
 * parameters. The logic switches first on target, then on
 * operation, then on any remaining parameters of the command.
 */
void ChordLindenmayer::interpret(std::vector<std::string> tokens) {
    const std::string &operation = tokens[0];
    std::string target;
    std::string switchTarget;
    if (tokens.size() > 1) {
        target = tokens[1];
        switchTarget = target.substr(0, target.find('['));
    }
    if (switchTarget == "T") {
        turtleOperation(operation, target, tokens);
    } else if (switchTarget == "N") {
        noteOperation(operation, target, tokens);
    } else if (switchTarget == "S") {
        noteStepOperation(operation, target, tokens);
    } else if (switchTarget == "O") {
        noteOrientationOperation(operation, target, tokens);
    } else if (switchTarget == "C") {
        chordOperation(operation, target, tokens);
    } else if (switchTarget == "M") {
        modalityOperation(operation, target, tokens);
    } else if (switchTarget == "V") {
        voicingOperation(operation, target, tokens);
    } else if (switchTarget == "Sc") {
        scaleOperation(operation, target, tokens);
    } else if (switchTarget == "Sd") {
        scaleDegreeOperation(operation, target, tokens);
    } else if (switchTarget == "P") {
        scoreOperation(operation, target, tokens);
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

