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
#ifndef CHORDLINDENMAYER_TO_SOUND_H
#define CHORDLINDENMAYER_TO_SOUND_H

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%include "std_string.i"
%include "std_vector.i"
%include "std_map.i"
%{
#include "ChordSpace.hpp"
#include "Conversions.hpp"
#include "Event.hpp"
#include "Score.hpp"
#include "Node.hpp"
#include "Voicelead.hpp"
#include "VoiceleadingNode.hpp"
#include "System.hpp"
#include <sstream>
#include <stack>
#include <string>
#include <map>
#include <vector>
#include <eigen3/Eigen/Dense>
%}
%template(StringMap) std::map<std::string, std::string>;
#else
#include "ChordSpace.hpp"
#include "Conversions.hpp"
#include "Event.hpp"
#include "Score.hpp"
#include "Node.hpp"
#include "Voicelead.hpp"
#include "VoiceleadingNode.hpp"
#include "System.hpp"
#include <sstream>
#include <stack>
#include <string>
#include <map>
#include <vector>
#include <eigen3/Eigen/Dense>
#endif

namespace csound
{
    
extern void SILENCE_PUBLIC printChord(std::ostream &stream, std::string label, const std::vector<double> &chord);

struct SILENCE_PUBLIC Turtle
{
    Event note;
    Event step;
    Event orientation;
    Chord chord;
    Scale scale;
    int scaleDegree;
    double rangeBass;
    double rangeSize;
    double voicing;
    Chord modality;
    Turtle()
    {
        initialize();
    }
    virtual ~Turtle() 
    {
    }
    Turtle(const Turtle &other)
    {
        *this = other;
    }
    void initialize()
    {
        note = csound::Event();
        step = csound::Event();
        for(size_t i = 0; i < Event::HOMOGENEITY; i++)
        {
            step[i] = 1.0;
        }
        orientation = csound::Event();
        orientation[Event::TIME] = 1.0;
        chord.clear();
        modality.clear();
        scale.clear();
        scaleDegree = 1;
        rangeBass = 36;
        rangeSize = 60;
        voicing = 0;
        modality = Conversions::nameToPitches("C Major");
    }
    Turtle &operator = (const Turtle &other)
    {
        note = other.note;
        step = other.step;
        orientation = other.orientation;
        chord = other.chord;
        scale = other.scale;
        scaleDegree = other.scaleDegree;
        rangeBass = other.rangeBass;
        rangeSize = other.rangeSize;
        voicing = other.voicing;
        modality = other.modality;
        return *this;
    }
#if __cpplusplus >= 201103L
    Turtle &operator = (Turtle &&other) = default;
#endif
    bool operator < (const Turtle &other) const
    {
        if (note < other.note) {
            return true;
        } else if (other.note < note) {
            return false;
        }
        if (step < other.step) {
            return true;
        } else if (other.step < step) {
            return false;
        }
        if (orientation < other.orientation) {
            return true;
        } else if (other.orientation < orientation) {
            return false;
        }
        if (chord < other.chord) {
            return true;
        } else if (other.chord < chord) {
            return false;
        }
        if (rangeBass < other.rangeBass) {
            return false;
        } else if (other.rangeBass < rangeBass) {
            return true;
        }
        if (rangeSize < other.rangeSize) {
            return true;
        } else if (other.rangeSize < rangeSize) {
            return false;
        }
        if (voicing < other.voicing) {
            return true;
        } else if (other.voicing < voicing) {
            return false;
        }
        if (modality < other.modality) {
            return true;
        }
        if (scale < other.scale) {
            return true;
        }
        if (scaleDegree < other.scaleDegree) {
            return true;
        }
        return false;
    }
    virtual std::string __str__() const
    {
        std::stringstream stream;
        stream << "Turtle:       " << std::endl;
        stream << " note:        " << note.toString() << std::endl;
        stream << " step:        " << step.toString() << std::endl;
        stream << " orientation: " << orientation.toString() << std::endl;
        printChord(stream, " chord:       ", chord);
        printChord(stream, " scale:       ", scale);
        stream << " scaleDegree: " << scaleDegree << std::endl;
        stream << " rangeBass:   " << rangeBass << std::endl;
        stream << " rangeSize:   " << rangeSize << std::endl;
        stream << " voicing:     " << voicing << std::endl;
        printChord(stream, " modality:    ", modality);
        return stream.str();
    }
};

struct SILENCE_PUBLIC Command
{
    std::string operation;
    std::string target;
    std::string equivalence;
    int index;
    double x;
    std::vector<double> v;
};

/**
 * A Lindenmayer system consists of a turtle representing a position in 
 * musical space, commands for moving the turtle or writing its state into 
 * a musical score, an axiom or initial set of commands, and zero or more 
 * rules for replacing commands with arbitrary sequences of commands.
 *
 * The turtle T represents the current state of the Lindenmayer system. 
 * The turtle consists of a note vector N that represents a position in 
 * musical space, a step size S, an orientation O, a chord C, a chord 
 * defining modality M, an octavewise chord revoicing V, a scale Sc, a 
 * scale degree Sd, and a range Ra. Turtle commands are defined 
 * `(operation target ...)` or `(operation target[dimension] ...)`:
 * ```
 * ([ T)        Push the current turtle state on a stack (start a branch).
 * (] T)        Pop the current turtle state from the stack (return to 
 *              the branching point).
 * (W N e)      Write the current turtle position as a note into the score
 *              under equivalence class e (0, 0, R).
 * (F N x e)    Move the turtle position N "forward" x steps S along its 
 *              current orientation O) under equivalence class e 
 *              (0, O, R): 
 *              N := N + (x * S * O).
 * (o N[d] x e) Apply algebraic operation o (=, +, -, *, /) to dimension d 
 *              (i, t, d, k, v, p, x, y, z, s) of the turtle position N 
 *              with parameter x under equivalence class e (0, O, R):
 *              N[d] := N[d] + S[d] o x.
 * (o S[d] x e) Apply algebraic operation o (=, +, -, *, /) to dimension d 
 *              (i, t, d, k, v, p, x, y, z, s) of the turtle step size S
 *              with parameter x under equivalence class e (0, O, R):
 *              S[d] := S[d] + S[d] o x.
 * (R O d1 d2)  Rotate the turtle orientation O in the plane of dimensions 
 *              d1 and d2 by angle x radians:
 *              R = makeRotation(d, e, x); O := R * O.
 * (W C e)      Write the current turtle chord C with voicing V to the 
 *              score under equivalence class e (0, O, R). This chord will 
 *              default to the same instrument and other dimensions as the 
 *              current turtle note N.
 * (o C v e)    Apply algebraic operation o (=, +, -, *, /) to the turtle 
 *              chord C as a whole with parameter v (a vector or chord name) 
 *              under equivalence class e (0, O, R):
 *              C := C o x.
 * (o C[i] x e) Apply algebraic operation o (=, +, -, *, /) to voice i 
 *              of the turtle chord C with parameter x under equivalence 
 *              class e (0, O, R):
 *              C[i] := C[i] o v.
 * (o M v e)    Apply algebraic operation o (=, +, -, *, /) to the turtle 
 *              modality M as a whole with parameter v (a vector or chord 
 *              name) under equivalence class e (0, O, R):
 *              M := M o v.
 * (o M[i] x e) Apply algebraic operation o (=, +, -, *, /) to voice i 
 *              of the turtle modality M with parameter x under 
 *              equivalence class e (0, O, R):
 *              M[i] := M[i] o x.
 * (o V x)      Apply algebraic operation o (=, +, -, *, /) to the voicing 
 *              index of the turtle chord with parameter x: 
 *              V := V o x. Of necessity the equivalence class is the 
 *              range of the score.
 * (I C x)      Invert the turtle chord C by reflecting it around 
 *              pitch-class x.
 * (K C)        Apply Neo-Riemannian inversion by exchange to the turtle 
 *              chord C.
 * (Q C x)      Apply Neo-Riemannian contextual transposition by x 
 *              pitch-classes (with reference to the turtle's modality M) 
 *              to the turtle chord C.
 * (+ C)        Add a voice (doubling the root) to the turtle chord C.
 * (- C)        Remove a voice (the uppermost) from the turtle chord C.
 * (o Sd x)     Apply algebraic operation o (=, +, -, *, /) to the turtle 
 *              scale degree Sd, with parameter x.
 * (C Sd m)     Obtain the turtle chord C of m voices as the current scale 
 *              degree Sd degree of the turtle scale Sc.
 * (C Sc n m)   Obtain the turtle chord C with m voices as the nth degree 
 *              of the turtle scale Sc.
 * (o Sc x)     Apply algebraic operation o (=, +, -, *, /) to the turtle 
 *              scale Sc; x may be a scalar, a vector to define the scale, 
 *              or the name of a scale.
 * (M Sc n k)   Modulate the turtle scale Sc to a new scale Sc with the 
 *              common chord at the current scale degree with n voices; if 
 *              there is more than one scale with that common chord, 
 *              choose the kth scale.
 * (C P)        Apply the current turtle chord C to the score, starting at 
 *              the current time and continuing until the next A command.
 * (C P L)      Apply the current turtle chord C to the score, using the 
 *              closest voice-leading from the previous chord (if any), 
 *              starting at the current time and continuing to the next 
 *              chord or scale.
 * (Sc P)       Apply the current turtle scale Sc to the score, starting 
 *              at the current time and continuing until the next chord 
 *              or scale.
 * (0 P)        End the scope of the previous chord or scale.   
 * (= P n)      Assign the range n to the size of the score, i.e. define 
 *              range equivalence.
 * ```
 * An arithmetic operation may also consist of sampling a random 
 * distribution, e.g. (u N[k] minimum maximum); the parameters of the 
 * distribution must be given. All the arithmetic operations are:
 * ```
 * Assignment           = x e
 * Addition             + x e
 * Subtraction          - x e
 * Multiplication       * x e
 * Division             / x e
 * Uniform              uni min max
 * Normal (Gaussian)    nor mean sigma min max
 * Binomial             bin p k min max
 * Negative binomial    nbi p k min max
 * Poisson              poi min max
 * Exponential          exp lambda min max
 * Gamma                gam alpha beta min max
 * Weibull              wei a b min max
 * Extreme value        ext a b min max
 * Log normal           log mean sigma min max
 * Chi squared          chi n min max
 * Cauchy               cau a b min max
 * Fisher               fis m n min max
 * Student              stu n min max
 * ```
 * PLEASE NOTE: Scale commands take precedence over chord commands. Not 
 * all commands are implemented. Unimplemented commands silently perform 
 * no operations, but may still be used to define replacement rules.
 */
class SILENCE_PUBLIC ChordLindenmayer :
    public VoiceleadingNode
{
public:
    ChordLindenmayer();
    virtual ~ChordLindenmayer();
    virtual int getIterationCount() const;
    virtual void setIterationCount(int count);
    virtual double getAngle() const;
    virtual void setAngle(double angle);
    virtual std::string getAxiom() const;
    virtual void setAxiom(std::string axiom);
    virtual void addRule(std::string command, std::string replacement);
    virtual std::string getReplacement(std::string command);
    /**
     * Scores are generated as follows:
     * <ol>
     * <li> The initial value of the turtle is set by the Lindenmayer system.<\li>
     * <li> The Lindenmayer system is rewritten by taking the axiom, parsing it into words,
     *      and replacing each word with the product of a rewriting rule, if one exists, or itself,
     *      if there is no rule. This procedure is iterated for a specified number of times.</li>
     * <li> The finished, rewritten Lindenmayer system is interpreted as a series of commands for
     *      moving a turtle around in various music spaces to write a score.</li><ol>
     * <li> Notes (N operations) are written directly into the score.</li>
     * <li> Chords (C operations) are written into the score as notes.</li>
     * <li> L and A operations are written into the score as voice-leading operations,
     *      to be applied after all notes have been generated.</li></ol>
     * <li> Overlapping and directly abutting notes in the score are joined.</li>
     * <li> The L and A operations are actually applied to the score.
     * <li> Overlapping and abutting notes in the score are again joined.</li>
     * </ol>
     */
    virtual void generate();
    virtual void generate(Score &score);
    virtual void clear();
    Score score;
    int iterationCount;
    double angle;
    std::string axiom;
    std::string production;
    Turtle turtle;
    std::map<std::string, std::string> rules;
    std::stack<Turtle> turtleStack;
    clock_t beganAt;
    clock_t endedAt;
    clock_t elapsed;
    virtual void initialize();
    /**
     * Iterates the replacement rules on the axiom and subsequent productions 
     * to produce the final production, a possibly long string of turtle 
     * commands.
     */
    virtual void generateLindenmayerSystem();
    /** 
     * Parses the final production into commands, each a tuple of strings,
     * and interprets each command to write notes and chord progressions into 
     * the score.
     */
    virtual void writeScore();
    virtual void fixStatus();
    virtual void tieOverlappingNotes();
    virtual void applyVoiceleadingOperations();
    virtual void interpret(std::vector<std::string> command);
    virtual int getDimension (const std::string &dimension) const;
    virtual Eigen::MatrixXd createRotation (int dimension1, int dimension2, double angle) const;
    /**
     * Returns the result of applying the equivalence class to the value,
     * both in the argument and as the return value; there may be no effect.
     */
    virtual double equivalence(double &value, const std::string &equivalenceClass) const;

};
}
#endif
