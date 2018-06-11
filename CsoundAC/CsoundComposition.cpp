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
#include "CsoundComposition.hpp"
#include "Exception.hpp"
#include "Composition.hpp"
#include "Node.hpp"
#include "System.hpp"
#include <cstdio>
#include <cstdlib>
#include <stdint.h>

namespace csound
{
  CsoundComposition::CsoundComposition() :
    csoundThreaded(&csoundThreaded_),
    threadCount(1)
  {
  }

  CsoundComposition::~CsoundComposition()
  {
    //clear();
  }

  void CsoundComposition::initialize()
  {
  }

  int CsoundComposition::generate()
  {
    int errorStatus = 0;
    if (children.size()) {
      Composition::score.clear();
    }
    traverse(getLocalCoordinates(), Composition::score);
    System::message("Generated %d events.\n", Composition::score.size());
    return errorStatus;
  }

  int CsoundComposition::render()
  {
    int errorStatus = generate();
    if (errorStatus) {
      return errorStatus;
    }
    errorStatus = perform();
    return errorStatus;
  }

  void CsoundComposition::createCsoundScore(std::string addToScore, double extendSeconds)
  {
    System::inform("addToScore.length(): %d\n", addToScore.length());
    if (addToScore.length() > 2) {
      csoundThreaded->ReadScore(addToScore.c_str());
    }
    csoundThreaded->ReadScore(Composition::score.getCsoundScore(tonesPerOctave, conformPitches).c_str());
    char buffer[0x100];
    std::sprintf(buffer, "\ne %9.3f\n", extendSeconds);
    csoundThreaded->ReadScore(buffer);
  }

  int CsoundComposition::perform()
  {
    int errorStatus = 0;
    CsoundFile::setCommand(getCsoundCommand());
    createCsoundScore(csoundScoreHeader);
    errorStatus = csoundThreaded->Perform();
    if (errorStatus == 1) {
      errorStatus = 0;
    }
    // The Csound command is managed from CsoundComposition,
    // not from CsoundThreaded. So we clear out what we set.
    CsoundFile::setCommand("");
    return errorStatus;
  }

  void CsoundComposition::clear()
  {
    Node::clear();
    Composition::clear();
    CsoundFile::removeScore();
  }

  void CsoundComposition::setCsoundThreaded(CsoundThreaded *csoundThreaded)
  {
    if(!csoundThreaded)
      {
        this->csoundThreaded = &csoundThreaded_;
      }
    else
      {
        this->csoundThreaded = csoundThreaded;
      }
  }

  CsoundThreaded *CsoundComposition::getCsoundThreaded()
  {
    return csoundThreaded;
  }

  void CsoundComposition::setCsoundOrchestra(std::string orchestra)
  {
    CsoundFile::setOrchestra(orchestra);
  }

  std::string CsoundComposition::getCsoundOrchestra() const
  {
    return CsoundFile::getOrchestra();
  }

  void CsoundComposition::setCsoundScoreHeader(std::string header)
  {
    csoundScoreHeader = header;
  }

  std::string CsoundComposition::getCsoundScoreHeader() const
  {
    return csoundScoreHeader;
  }

  void CsoundComposition::arrange(int oldInstrumentNumber, int newInstrumentNumber)
  {
    Composition::score.arrange(oldInstrumentNumber, newInstrumentNumber);
  }

  void CsoundComposition::arrange(int oldInstrumentNumber,
                           int newInstrumentNumber,
                           double gain)
  {
    Composition::score.arrange(oldInstrumentNumber, newInstrumentNumber, gain);
  }

  void CsoundComposition::arrange(int oldInstrumentNumber,
                           int newInstrumentNumber,
                           double gain,
                           double pan)
  {
    Composition::score.arrange(oldInstrumentNumber, newInstrumentNumber, gain, pan);
  }

  void CsoundComposition::arrange(int silenceInstrumentNumber,
                           std::string csoundInstrumentName)
  {
    int csoundInstrumentNumber =
      CsoundFile::getInstrumentNumber(csoundInstrumentName);
    arrange(silenceInstrumentNumber, csoundInstrumentNumber);
  }

  void CsoundComposition::arrange(int silenceInstrumentNumber,
                           std::string csoundInstrumentName, double gain)
  {
    int csoundInstrumentNumber =
      CsoundFile::getInstrumentNumber(csoundInstrumentName);
    arrange(silenceInstrumentNumber, csoundInstrumentNumber, gain);
  }

  void CsoundComposition::arrange(int silenceInstrumentNumber,
                           std::string csoundInstrumentName, double gain, double pan)
  {
    int csoundInstrumentNumber =
      CsoundFile::getInstrumentNumber(csoundInstrumentName);
    arrange(silenceInstrumentNumber, csoundInstrumentNumber, gain, pan);
  }

  void CsoundComposition::removeArrangement()
  {
    CsoundFile::removeArrangement();
  }

  std::string CsoundComposition::getCsoundCommand() const
  {
    std::string command_ = CsoundFile::getCommand();
    if (command_.size() == 0) {
      char buffer[0x200];
      std::sprintf(buffer,
                   "csound --midi-key=4 --midi-velocity=5 -m195 -j%d -RWdfo %s",
           threadCount,
                   CsoundFile::getOutputSoundfileName().c_str());
      command_ = buffer;
    }
    return command_;
  }

  intptr_t CsoundComposition::getThis()
  {
    return (intptr_t) this;
  }

  int CsoundComposition::processArgs(const std::vector<std::string> &args)
  {
    System::inform("BEGAN CsoundComposition::processArgv()...\n");
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
    std::string playSoundfileName = CsoundFile::getOutputSoundfileName();
    if ((argsmap.find("--dir") != argsmap.end()) && !errorStatus) {
      setOutputDirectory(argsmap["--dir"]);
    }
    if ((argsmap.find("--midi") != argsmap.end()) && !errorStatus) {
      errorStatus = generate();
      if (errorStatus) {
        return errorStatus;
      }
      Composition::getScore().save(CsoundFile::getMidiFilename().c_str());
    }
    if ((argsmap.find("--notation") != argsmap.end()) && !errorStatus) {
        translateToNotation();
    }
    if ((argsmap.find("--audio") != argsmap.end()) && !errorStatus) {
      postPossible = false;
      const char *audiosystem = argsmap["--audio"].c_str();
      const char *devicename = argsmap["--device"].c_str();
      std::sprintf(command,
                   "csound --midi-key=4 --midi-velocity=5 -m195 -+rtaudio=%s -o %s",
                   audiosystem, devicename);
      System::inform("Csound command: %s\n", command);
      setCsoundCommand(command);
      errorStatus = render();
    }
    if ((argsmap.find("--csound") != argsmap.end()) && !errorStatus) {
      postPossible = true;
      errorStatus = render();
    }
    if ((argsmap.find("--pianoteq") != argsmap.end()) && !errorStatus) {
      std::sprintf(command, "Pianoteq --midi=%s\n", CsoundFile::getMidiFilename().c_str());
      System::inform("Executing command: %s\n", command);
      errorStatus = std::system(command);
    }
    if ((argsmap.find("--pianoteq-wav") != argsmap.end()) && !errorStatus) {
      postPossible = true;
      std::sprintf(command, "Pianoteq --headless --midi %s --rate 48000 --wav %s\n", CsoundFile::getMidiFilename().c_str(), CsoundFile::getOutputSoundfileName().c_str());
      System::inform("Executing command: %s\n", command);
      errorStatus = std::system(command);
    }
    if ((argsmap.find("--playmidi") != argsmap.end()) && !errorStatus) {
      std::sprintf(command, "%s %s\n", argsmap["--playmidi"].c_str(), CsoundFile::getMidiFilename().c_str());
      System::inform("Executing command: %s\n", command);
      errorStatus = std::system(command);
    }
    if ((argsmap.find("--post") != argsmap.end()) && !errorStatus && postPossible) {
      errorStatus = translateMaster();
      playSoundfileName = getNormalizedSoundfileName();
    }
    if ((argsmap.find("--playwav") != argsmap.end()) && !errorStatus) {
      std::sprintf(command, "%s %s\n", argsmap["--playwav"].c_str(), playSoundfileName.c_str());
      System::inform("Csound command: %s\n", command);
      errorStatus = std::system(command);
    }
    System::inform("ENDED CsoundComposition::processArgv().\n");
    return errorStatus;
  }

  void CsoundComposition::stop()
  {
    std::cout << "CsoundComposition::stop()..." << std::endl;
    csoundThreaded->Stop();
  }
  
}
