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
#include "ScoreModel.hpp"
#include "CppSound.hpp"
#include "Node.hpp"
#include "Score.hpp"
%}
#else
#include "ScoreModel.hpp"
#include "CppSound.hpp"
#include "Node.hpp"
#include "Score.hpp"
#include <stdint.h>
#endif

namespace csound
{
/**
 * A ScoreModel that uses Csound to render generated scores,
 * via the CppSound class.
 */
class SILENCE_PUBLIC MusicModel :
    public ScoreModel
{
public:
    int threadCount;
    MusicModel();
    virtual ~MusicModel();
    virtual void initialize();
    virtual int generate();
    virtual intptr_t getThis();
    virtual Node *getThisNode();
    /**
     * Translate the generated score to a Csound score
     * and export it for performance.
     * The time given by extendSeconds is used for a concluding e statement.
     */
    virtual void createCsoundScore(std::string addToScore = "",
                                   double extendSeconds = 0.);
    /**
     * Convenience function that erases the existing score,
     * appends optional text to it,
     * invokes generate(), invokes createCsoundScore(), and invokes perform().
     */
    virtual int render();
    virtual void stop();
    /**
     * Uses Csound to perform the current score. If a Csound command has been 
     * set in this, that is used; otherwise, if an output soundfile has been 
     * specified in this, a command line is generated and used; otherwise, a 
     * default command line is used.
     *
     * In all cases, a CSD file is generated in memory and rendered.
     */
    virtual int perform();
    /**
     * Clear all contents of this.
     */
    virtual void clear();
    /**
     * Set the Csound orchestra
     * (convenience wrapper for CppSound::setOrchestra()).
     */
    virtual void setCsoundOrchestra(std::string orchestra);
    /**
     * Return the Csound orchestra
     * (convenience wrapper for CppSound::getOrchestra()).
      */
    virtual std::string getCsoundOrchestra() const;
    /**
     * Set a Csound score fragment to be prepended
     * to the generated score (createCsoundScore is called with it).
     */
    virtual void setCsoundScoreHeader(std::string header);
    /**
     * Return the Csound score header that is prepended
     * to generated scores.
     */
    virtual std::string getCsoundScoreHeader() const;
    /**
     * Re-assign instrument number for export to Csound score
     * (convenience wrapper for Score::arrange()).
     */
    virtual void arrange(int oldInstrumentNumber, int newInstrumentNumber);
    /**
     * Re-assign instrument number and adjust gain
     * for export to Csound score
     * (convenience wrapper for Score::arrange()).
     */
    virtual void arrange(int oldInstrumentNumber,
                         int newInstrumentNumber,
                         double gain);
    /**
     * Re-assign instrument number, adjust gain,
     * and change pan for export to Csound score
     * (convenience wrapper for Score::arrange()).
     */
    virtual void arrange(int oldInstrumentNumber,
                         int newInstrumentNumber,
                         double gain,
                         double pan);
    /**
     * Re-assign instrument by name for export to Csound score.
     */
    virtual void arrange(int silenceInstrumentNumber,
                         std::string csoundInstrumentName);
    /**
     * Re-assign instrument by name and adjust gains for export to Csound score.
     */
    virtual void arrange(int silenceInstrumentNumber,
                         std::string csoundInstrumentName,
                         double gain);
    /**
     * Re-assign instrument by name, adjust gain, and change pan for export to Csound score.
     */
    virtual void arrange(int silenceInstrumentNumber,
                         std::string csoundInstrumentName,
                         double gain,
                         double pan);
    /**
      * Remove instrument number, gain, and pan assignments
      * (convenience wrapper for Score::removeArrangement()).
      */
    virtual void removeArrangement();
    /**
     * Set Csound command line
     * (convenience wrapper for CppSound::setCommand()).
     */
    virtual void setCsoundCommand(std::string command);
    /**
     * Return Csound command line
     * (convenience wrapper for CppSound::getCommand()).
     */
    virtual std::string getCsoundCommand() const;
    /**
     * Pass the invoking program's command-line arguments to processArgs()
     * and it will perform the following commands:
     *
     * --csound        Render generated score using set Csound orchestra.
     * --midi          Render generated score as MIDI file and play it (default).
     * --pianoteq      Play generated MIDI sequence file with Pianoteq.
     * --pianoteq-wav  Render score to soundfile using Pianoteq,
     *                 post-process it, and play it.
     * --playmidi      Play generated MIDI filev
     *                 post-process it, and play it.
     * --playwav       Play rendered or normalized output soundfile.
     * --post          Post-process Csound output soundfile:
     *                 normalize, CD, MP3, tag, and play it.
     * 
     * If none of these are given, all command-line arguments are passed 
     * directly to Csound.
     */
    virtual int processArgs(const std::vector<std::string> &args);

    virtual int cppsoundCleanup() {
        return cppSound->Cleanup();
    }
    virtual int cppsoundCompile(int argc, const char **argv) {
        return cppSound->Compile(argc, argv);
    }
    virtual int cppsoundCompileCsdText(const std::string &csd_text) {
        return cppSound->CompileCsdText(csd_text.c_str());
    }
    virtual std::string cppsoundGetCommand() const {
        return cppSound->getCommand();
    }
    virtual void cppsoundInputMessage(const std::string &message) const {
        cppSound->inputMessage(message.c_str());
    }
    virtual int cppsoundLoad(std::string filename) {
        return cppSound->load(filename);
    }
    virtual int cppsoundPerform() {
        return cppSound->perform();
    }
    virtual int cppsoundPerformKsmps() {
        return cppSound->PerformKsmps();
    }
    virtual void cppsoundReset() {
        cppSound->Reset();
    }
    virtual void cppsoundSetCommand(const std::string &command) {
        cppSound->setCommand(command);
    }
    virtual void cppsoundSetFilename(const std::string &filename) {
        cppSound->setFilename(filename);
    }
    virtual int cppsoundStart() {
        return cppSound->Start();
    }
    virtual void cppsoundStop() {
        cppSound->stop();
    }
    virtual double getExtendSeconds() const {
        return extendSeconds;
    }
    virtual void setExtendSeconds(double extendSeconds_) {
        extendSeconds = extendSeconds_;
    }
    /**
     * Does not use the csound::Composition options; passes 
     * argc and argv directly to Csound.
     */
    virtual void csoundArgv(int argc, const char **argv);
protected:
    /**
     * Self-contained Csound object.
     */
    CppSound cppSound_;
    /**
     * Pointer to a Csound object that is
     * used to render scores. Defaults to
     * the internal Csound object, but
     * can be re-set to an external Csound object.
     */
    CppSound *cppSound;
    /**
     * Prepended to generated score.
     */
    std::string csoundScoreHeader;
    double extendSeconds = -1;
};

}
