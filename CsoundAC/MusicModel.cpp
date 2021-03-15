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
#include "MusicModel.hpp"
#include "Exception.hpp"
#include "Composition.hpp"
#include "System.hpp"
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <stdint.h>

namespace csound
{
MusicModel::MusicModel() :
    threadCount(1),
    cppSound(&cppSound_)
{
}

MusicModel::~MusicModel()
{
    //clear();
}

void MusicModel::initialize()
{
}

int MusicModel::generate()
{
    int errorStatus = 0;
    cppSound->removeScore();
    if (children.size()) {
        score.clear();
    }
    traverse(getLocalCoordinates(), score);
    score.process();
    if (duration > 0.) {
        score.setDuration(duration);
    }
    System::message("Generated %d events.\n", score.size());
    // Not implemented fully yet.
    //score.save(getMusicXmlfileFilepath());
    // Always save MIDI.
    score.save(getMidifileFilepath());    
    return errorStatus;
}

int MusicModel::render()
{
    int errorStatus = generate();
    if (errorStatus) {
        return errorStatus;
    }
    errorStatus = perform();
    return errorStatus;
}

void MusicModel::createCsoundScore(std::string addToScore, double extendSeconds_)
{
    System::inform("addToScore.length(): %d\n", addToScore.length());
    if (addToScore.length() > 2) {
        cppSound->removeScore();
        cppSound->addScoreLine(addToScore);
    }
    cppSound->addScoreLine(score.getCsoundScore(tonesPerOctave, conformPitches));
    if (extendSeconds_ >= 0.) {
        extendSeconds = extendSeconds_;
        char e_statement[0x100];
        std::snprintf(e_statement, 0x100, "\s 0.0\ne %9.4f\n", extendSeconds);
        cppSound->addScoreLine(e_statement);
    }
}

int MusicModel::perform()
{
    auto started_at = System::startTiming();
    int errorStatus = 0;
    std::string old_command = cppSound->getCommand();
    auto csound_command = getCsoundCommand();
    System::message("MusicModel::perform using Csound command: %s\n", csound_command.c_str());
    cppSound->setCommand(getCsoundCommand());
    createCsoundScore(csoundScoreHeader, getExtendSeconds());
    std::string csd = cppSound->getCSD();
    // Always save the generated csd.
    std::string csd_filename = getOutputSoundfileFilepath() + "-generated.csd";
    std::ofstream stream;
    stream.open(csd_filename.c_str(), std::ofstream::out | std::ofstream::trunc);
    stream.write(csd.c_str(), csd.size());
    stream.close();
    //System::message("MusicModel::perform using csd: %s\n", csd.c_str());
    errorStatus = csoundCompileCsdText(cppSound->getCsound(), csd.c_str());
    errorStatus = csoundStart(cppSound->getCsound());
    while (true) {
        errorStatus = csoundPerformKsmps(cppSound->getCsound());
        if (errorStatus != 0) {
            break;
        }
    }
    if (errorStatus == 1) {
        errorStatus = 0;
    }
    cppSound->setCommand(old_command);
    auto seconds = System::stopTiming(started_at);
    System::message("Performance took %9.4f seconds.\n", seconds);
    return errorStatus;
}

void MusicModel::clear()
{
    Node::clear();
    Composition::clear();
    cppSound->removeScore();
}

void MusicModel::setCsoundOrchestra(std::string orchestra)
{
    cppSound->setOrchestra(orchestra);
}

std::string MusicModel::getCsoundOrchestra() const
{
    return cppSound->getOrchestra();
}

void MusicModel::setCsoundScoreHeader(std::string header)
{
    csoundScoreHeader = header;
}

std::string MusicModel::getCsoundScoreHeader() const
{
    return csoundScoreHeader;
}

void MusicModel::arrange(int oldInstrumentNumber, int newInstrumentNumber)
{
    score.arrange(oldInstrumentNumber, newInstrumentNumber);
}

void MusicModel::arrange(int oldInstrumentNumber,
                         int newInstrumentNumber,
                         double gain)
{
    score.arrange(oldInstrumentNumber, newInstrumentNumber, gain);
}

void MusicModel::arrange(int oldInstrumentNumber,
                         int newInstrumentNumber,
                         double gain,
                         double pan)
{
    score.arrange(oldInstrumentNumber, newInstrumentNumber, gain, pan);
}

void MusicModel::arrange(int silenceInstrumentNumber,
                         std::string csoundInstrumentName)
{
    int csoundInstrumentNumber =
        cppSound->getInstrumentNumber(csoundInstrumentName);
    arrange(silenceInstrumentNumber, csoundInstrumentNumber);
}

void MusicModel::arrange(int silenceInstrumentNumber,
                         std::string csoundInstrumentName, double gain)
{
    int csoundInstrumentNumber =
        cppSound->getInstrumentNumber(csoundInstrumentName);
    arrange(silenceInstrumentNumber, csoundInstrumentNumber, gain);
}

void MusicModel::arrange(int silenceInstrumentNumber,
                         std::string csoundInstrumentName, double gain, double pan)
{
    int csoundInstrumentNumber =
        cppSound->getInstrumentNumber(csoundInstrumentName);
    arrange(silenceInstrumentNumber, csoundInstrumentNumber, gain, pan);
}

void MusicModel::removeArrangement()
{
    score.removeArrangement();
}

void MusicModel::setCsoundCommand(std::string command)
{
    cppSound->setCommand(command);
}

std::string MusicModel::getCsoundCommand() const
{
    std::string csound_command = cppSound->getCommand();
    if (csound_command.size() > 0) {
        return csound_command;
    }
    auto output_soundfile_filepath = getOutputSoundfileFilepath();
    if (output_soundfile_filepath.size() == 0) {
        output_soundfile_filepath = "music_model_output.wav";
    }
    char buffer[0x200];
    std::sprintf(buffer,
                // message flags: 1 + 2 + 32 + 128 
                 "--midi-key=4 --midi-velocity=5 -m168 -j%d -RWdfo%s",
                 threadCount,
                 output_soundfile_filepath.c_str());
    return buffer;
}

intptr_t MusicModel::getThis()
{
    return (intptr_t) this;
}


Node *MusicModel::getThisNode()
{
    return (Node *)this;
}

int MusicModel::processArgs(const std::vector<std::string> &args)
{
    System::inform("MusicModel::processArgs...\n");
    generateAllNames();
    std::map<std::string, std::string> argsmap;
    std::string key;
    for (size_t i = 0, n = args.size(); i < n; ++i) {
        const std::string token = args[i];
        std::string value = "";
        if (token.find("--") == 0) {
            key = token;
            System::inform("argument[%2d]: %s\n", i, key.c_str());
        } else {
            value = token;
            System::inform("argument[%2d]: %s =  %s\n", i, key.c_str(), value.c_str());
        }
        argsmap[key] = value;
    }
    char command[0x200];
    int errorStatus = 0;
    bool postPossible = false;
    std::string playSoundfileName = getOutputSoundfileFilepath();
    if ((argsmap.find("--dir") != argsmap.end()) && !errorStatus) {
        setOutputDirectory(argsmap["--dir"]);
    }
    if ((argsmap.find("--midi") != argsmap.end()) && !errorStatus) {
        errorStatus = generate();
        if (errorStatus) {
            return errorStatus;
        }
        getScore().save(getMidifileFilepath().c_str());
    }
    if ((argsmap.find("--notation") != argsmap.end()) && !errorStatus) {
        translateToNotation();
    }
    if ((argsmap.find("--audio") != argsmap.end()) && !errorStatus) {
        postPossible = false;
        const char *audiosystem = argsmap["--audio"].c_str();
        const char *devicename = argsmap["--device"].c_str();
        std::sprintf(command,
                     "csound --midi-key=4 --midi-velocity=5 -m162 -+rtaudio=%s -o%s",
                     audiosystem, devicename);
        System::inform("Csound command: %s\n", command);
        setCsoundCommand(command);
        getScore().save(getMidifileFilepath().c_str());
        errorStatus = render();
    }
    if ((argsmap.find("--csound") != argsmap.end()) && !errorStatus) {
        postPossible = true;
        getScore().save(getMidifileFilepath().c_str());
        errorStatus = render();
    }
    if ((argsmap.find("--pianoteq") != argsmap.end()) && !errorStatus) {
        std::sprintf(command, "Pianoteq --midi=%s\n", getMidifileFilepath().c_str());
        System::inform("Executing command: %s\n", command);
        errorStatus = std::system(command);
    }
    if ((argsmap.find("--pianoteq-wav") != argsmap.end()) && !errorStatus) {
        postPossible = true;
        std::sprintf(command, "Pianoteq --headless --midi %s --rate 48000 --wav %s\n", getMidifileFilepath().c_str(), getOutputSoundfileFilepath().c_str());
        System::inform("Executing command: %s\n", command);
        errorStatus = std::system(command);
    }
    if ((argsmap.find("--playmidi") != argsmap.end()) && !errorStatus) {
        std::sprintf(command, "%s %s\n", argsmap["--playmidi"].c_str(), getMidifileFilepath().c_str());
        System::inform("Executing command: %s\n", command);
        errorStatus = std::system(command);
    }
    if ((argsmap.find("--post") != argsmap.end()) && postPossible) {
        errorStatus = translateMaster();
        playSoundfileName = getNormalizedSoundfileFilepath();
    }
    if ((argsmap.find("--playwav") != argsmap.end()) && !errorStatus) {
        std::sprintf(command, "%s %s\n", argsmap["--playwav"].c_str(), playSoundfileName.c_str());
        System::inform("Csound command: %s\n", command);
        errorStatus = std::system(command);
    }
    System::inform("MusicModel::processArgv.\n");
    return errorStatus;
}

void MusicModel::stop()
{
    System::inform("MusicModel::stop...\n");
    cppSound->stop();
}
}
