/*

    Copyright (C) 2018 Michael Gogins

    This file is part of csound-extended.

    The Csound Library is free software; you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    Csound is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Csound; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA

    As a special exception, if other files instantiate templates or
    use macros or inline functions from this file, this file does not
    by itself cause the resulting executable or library to be covered
    by the GNU Lesser General Public License. This exception does not
    however invalidate any other reasons why the library or executable
    file might be covered by the GNU Lesser General Public License.
*/
#include <cstdio>
#include <eigen3/Eigen/Dense>
#include <emscripten/bind.h>
#include <fstream>
#include <iostream>
#include <CsoundAC/Silence.hpp>
#include <string>
#include <sstream>

static void hello() {
    csound::System::message("Hello, World, this is CsoundAC!\n");
}

/*
The objective here is not completeness, but usefulness. As much must be 
exposed as is needed for freely composing with CsoundAC. The situation 
is complicated by the fact that we do not use MusicModel with its 
built-in C++ interface to Csound, but defer to the WebAssembly build of 
Csound, which means we use ScoreModel rather than MusicModel to compose.

Filling out the Embind definitions is a big job that needs to be kept in order 
and to enable keeping track of work completed. This is done by following the 
order in the Doxygen documentation for the `csound` namespace by structure 
name and method name, and marking finished classes with a // FINISHED comment.
In other words, classes and their methods are in alphabetical order. Other 
functions, constants, and such should however put in, in declaration order, 
just above the class that appears in the same header file.

The order of definitions does not matter when compiling, so just keep these 
in the order describe above order. 
*/

EMSCRIPTEN_BINDINGS(csoundac) {  
    // FINISHED
    emscripten::class_<csound::Cell, emscripten::base<csound::ScoreNode> >("Cell")
        .constructor<>()
        .property("durationSeconds", &csound::Cell::getDurationSeconds, &csound::Cell::setDurationSeconds)
        .property("importFilename", &csound::Cell::getImportFilename, &csound::Cell::setImportFilename)
        .property("relativeDuration", &csound::Cell::getRelativeDuration, &csound::Cell::setRelativeDuration)
        .property("repeatCount", &csound::Cell::getRepeatCount, &csound::Cell::setRepeatCount)
    ;
    // FINISHED
    emscripten::class_<csound::CellAdd, emscripten::base<csound::Node> >("CellAdd")
        .constructor<>()
        .function("add", &csound::CellAdd::add)
    ;
    // FINISHED
    emscripten::class_<csound::CellChord, emscripten::base<csound::Node> >("CellChord")
        .constructor<>()
        .function("chord", &csound::CellChord::chord)
    ;
    // FINISHED
    emscripten::class_<csound::CellMultiply, emscripten::base<csound::Node> >("CellMultiply")
        .constructor<>()
        .function("multiply", &csound::CellChord::chord)
    ;
    // FINISHED
    emscripten::class_<csound::CellRandom, emscripten::base<csound::Random> >("CellRandom")
        .constructor<>()
        .function("random", &csound::CellRandom::random)
    ;
    // FINISHED
    emscripten::class_<csound::CellReflect, emscripten::base<csound::Node> >("CellReflect")
        .constructor<>()
        .function("reflect", &csound::CellReflect::reflect)
    ;
    // FINISHED
    emscripten::class_<csound::CellRemove, emscripten::base<csound::Node> >("CellRemove")
        .constructor<>()
        .function("remove", &csound::CellRemove::remove)
    ;
    // FINISHED
    emscripten::class_<csound::CellRepeat, emscripten::base<csound::Node> >("CellRepeat")
        .constructor<>()
        .function("repeat", &csound::CellRepeat::repeat)
    ;
    // FINISHED
    emscripten::class_<csound::CellSelect, emscripten::base<csound::Node> >("CellSelect")
        .constructor<>()
        .function("select", &csound::CellSelect::select)
    ;
    // FINISHED
    emscripten::class_<csound::CellShuffle, emscripten::base<csound::Random> >("CellShuffle")
        .constructor<>()
        .function("shuffle", &csound::CellShuffle::shuffle)
    ;
    // NOT SUPPORTED
    //~ // Can't seem to do scoped enums.
    //~ emscripten::enum_<csound::Chord::CHORD_DIMENSION>("CHORD_DIMENSION")
        //~ .value("PITCH", CHORD_DIMENSION::PITCH)
        //~ .value("DURATION", CHORD_DIMENSION::DURATION)
        //~ .value("LOUDNESS", CHORD_DIMENSION::LOUDNESS)
        //~ .value("INSTRUMENT", CHORD_DIMENSION::INSTRUMENT)
        //~ .value("PAN", CHORD_DIMENSION::PAN)
        //~ .value("COUNT", CHORD_DIMENSION::COUNT)
    //~ ;
    emscripten::function("euclidean", &csound::euclidean);
    emscripten::function("factorial", &csound::factorial);
    emscripten::function("midpoint", &csound::midpoint);
    emscripten::function("EPSILON", &csound::EPSILON);
    emscripten::function("epsilonFactor", &csound::epsilonFactor);
    emscripten::function("eq_epsilon", &csound::eq_epsilon);
    emscripten::function("gt_epsilon", &csound::gt_epsilon);
    emscripten::function("lt_epsilon", &csound::lt_epsilon);
    emscripten::function("ge_epsilon", &csound::ge_epsilon);
    emscripten::function("le_epsilon", &csound::le_epsilon);
    emscripten::function("OCTAVE", &csound::OCTAVE);
    emscripten::function("MIDDLE_C", &csound::MIDDLE_C);
    emscripten::function("C4", &csound::C4);
    emscripten::function("T", &csound::T);
    emscripten::function("I", &csound::I);
    emscripten::function("modulo", &csound::modulo);
    emscripten::function("epc", &csound::epc);
    emscripten::function("pitchClassForName", &csound::pitchClassForName);
    emscripten::function("nameForPitchClass", &csound::nameForPitchClass);
    emscripten::function("nameForChord", &csound::nameForChord);
    emscripten::function("chordForName", &csound::chordForName);
    emscripten::function("nameForScale", &csound::nameForScale);
    emscripten::function("scaleForName", &csound::scaleForName);
    emscripten::function("add_chord", &csound::add_chord);
    emscripten::function("add_scale", &csound::add_scale);
    emscripten::function("iterator", &csound::iterator);
    emscripten::function("next", &csound::next);
    emscripten::function("operator==", &csound::operator==);
    emscripten::function("operator<", emscripten::select_overload<bool(const csound::Chord&,const csound::Chord&)>(&csound::operator<));
    emscripten::function("operator<=", &csound::operator<=);
    emscripten::function("operator>", &csound::operator>);
    emscripten::function("operator>=", &csound::operator>=);
    emscripten::function("voiceleading", &csound::voiceleading);
    emscripten::function("parallelFifth", &csound::parallelFifth);
    emscripten::function("voiceleadingSmoothness", &csound::voiceleadingSmoothness);
    emscripten::function("voiceleadingSmoother", &csound::voiceleadingSmoother);
    emscripten::function("voiceleadingSimpler", &csound::voiceleadingSimpler);
    emscripten::function("voiceleadingCloser", &csound::voiceleadingCloser);
    emscripten::function("voiceleadingClosestRange", &csound::voiceleadingClosestRange);
    emscripten::function("closestPitch", &csound::closestPitch);
    emscripten::function("conformToPitchClassSet", &csound::conformToPitchClassSet);
    emscripten::function("conformToChord", &csound::conformToChord);
    emscripten::function("insert", &csound::insert);
    emscripten::function("slice", &csound::slice);
    emscripten::function("apply", &csound::apply);
    emscripten::function("gather", &csound::gather);
    emscripten::function("octavewiseRevoicings", &csound::octavewiseRevoicings);
    emscripten::function("octavewiseRevoicing", &csound::octavewiseRevoicing);
    emscripten::function("indexForOctavewiseRevoicing", &csound::indexForOctavewiseRevoicing);
    emscripten::function("scale", &csound::scale);
    emscripten::function("chord", &csound::chord);
    emscripten::function("transpose_degrees", &csound::transpose_degrees);
    emscripten::register_vector<csound::Chord>("ChordVector");
    emscripten::register_vector<csound::Scale>("ScaleVector");
    emscripten::register_vector<double>("DoubleVector");
    emscripten::register_map<std::string,std::string>("StringToStringMap");
    emscripten::class_<csound::Chord>("Chord")
        .constructor<>()
        .constructor<csound::Chord>()
        // NOT SUPPORTED
        //.function("a", &csound::Chord::a)
        .function("ceiling", &csound::Chord::ceiling)
        .function("contains", &csound::Chord::contains)
        .function("count", &csound::Chord::count)
        .function("cycle", &csound::Chord::cycle)
        .function("distanceToOrigin", &csound::Chord::distanceToOrigin)
        .function("distanceToUnisonDiagonal", &csound::Chord::distanceToUnisonDiagonal)
        .function("eI", &csound::Chord::eI)
        .function("eO", &csound::Chord::eO)
        .function("eOP", &csound::Chord::eOP)
        .function("eOPI", &csound::Chord::eOPI)
        .function("eOPT", &csound::Chord::eOPT)
        .function("eOPTI", &csound::Chord::eOPTI)
        .function("eOPTT", &csound::Chord::eOPTT)
        .function("eOPTTI", &csound::Chord::eOPTTI)
        .function("eP", &csound::Chord::eP)
        .function("epcs", &csound::Chord::epcs)
        .function("equals", &csound::Chord::equals)
        .function("eR", &csound::Chord::eR)
        .function("eRP", &csound::Chord::eRP)
        .function("eRPI", &csound::Chord::eRPI)
        .function("eRPT", &csound::Chord::eRPT)
        .function("eRPTI", &csound::Chord::eRPTI)
        .function("eRPTT", &csound::Chord::eRPTT)
        .function("eRPTTI", &csound::Chord::eRPTTI)
        .function("et", &csound::Chord::et)
        .function("eT", &csound::Chord::eT)
        .function("eTT", &csound::Chord::eTT)
        .function("eV", &csound::Chord::eV)
        .function("floor", &csound::Chord::floor)
        .function("fromString", &csound::Chord::fromString)
        .function("getDuration", &csound::Chord::getDuration)
        .function("getInstrument", &csound::Chord::getInstrument)
        .function("getLoudness", &csound::Chord::getLoudness)
        .function("getPan", &csound::Chord::getPan)
        .function("getPitch", &csound::Chord::getPitch)
        .function("I", &csound::Chord::I)
        .function("Iform", &csound::Chord::Iform)
        .function("information", &csound::Chord::information)
        .function("iseI", &csound::Chord::iseI)
        .function("iseI_chord", &csound::Chord::iseI_chord, emscripten::allow_raw_pointers())
        .function("iseOP", &csound::Chord::iseOP)
        .function("iseOPI", &csound::Chord::iseOPI)
        .function("iseOPT", &csound::Chord::iseOPT)
        .function("iseOPTI", &csound::Chord::iseOPTI)
        .function("iseOPTT", &csound::Chord::iseOPTT)
        .function("iseOPTTI", &csound::Chord::iseOPTTI)
        .function("iseP", &csound::Chord::iseP)
        .function("isepcs", &csound::Chord::isepcs)
        .function("iseR", &csound::Chord::iseR)
        .function("iseRP", &csound::Chord::iseRP)
        .function("iseRPI", &csound::Chord::iseRPI)
        .function("iseRPT", &csound::Chord::iseRPT)
        .function("iseRPTI", &csound::Chord::iseRPTI)
        .function("iseRPTT", &csound::Chord::iseRPTT)
        .function("iseRPTTI", &csound::Chord::iseRPTTI)
        .function("iset", &csound::Chord::iset)
        .function("iseT", &csound::Chord::iseT)
        .function("iseTT", &csound::Chord::iseTT)
        .function("iseV", &csound::Chord::iseV)
        .function("K", &csound::Chord::K)
        .function("layer", &csound::Chord::layer)
        .function("max", &csound::Chord::max)
        .function("maximallyEven", &csound::Chord::maximallyEven)
        .function("maximumInterval", &csound::Chord::maximumInterval)
        .function("min", &csound::Chord::min)
        .function("minimumInterval", &csound::Chord::minimumInterval)
        .function("move", &csound::Chord::move)
        .function("name", &csound::Chord::name)
        .function("note", &csound::Chord::note)
        .function("notes", &csound::Chord::notes)
        .function("nrD", &csound::Chord::nrD)
        .function("nrH", &csound::Chord::nrH)
        .function("nrL", &csound::Chord::nrL)
        .function("nrN", &csound::Chord::nrN)
        .function("nrP", &csound::Chord::nrP)
        .function("nrR", &csound::Chord::nrR)
        .function("nrS", &csound::Chord::iseV)
        .function("operator=", emscripten::select_overload<csound::Chord &(const csound::Chord &)>(&csound::Chord::operator=))
        .function("origin", &csound::Chord::origin)
        .function("permutations", &csound::Chord::permutations)
        .function("Q", &csound::Chord::Q)
        .function("resize", &csound::Chord::resize)
        .function("setDuration", &csound::Chord::setDuration)
        .function("setInstrument", &csound::Chord::setInstrument)
        .function("setLoudness", &csound::Chord::setLoudness)
        .function("setPan", &csound::Chord::setPan)
        .function("setPitch", &csound::Chord::setPitch)
        .function("T", &csound::Chord::T)
        .function("T_voiceleading", &csound::Chord::T_voiceleading)
        .function("Tform", &csound::Chord::Tform)
        .function("toScore", &csound::Chord::toScore)
        .function("toString", &csound::Chord::toString)
        .function("v", &csound::Chord::v)
        .function("voiceleading", &csound::Chord::voiceleading)
        .function("voices", &csound::Chord::voices)
    ;
    // FINISHED
    emscripten::class_<csound::ChordLindenmayer, emscripten::base<csound::VoiceleadingNode> >("ChordLindenmayer")
        .constructor<>()
        .function("addRule", &csound::ChordLindenmayer::addRule)
        .function("createRotation", &csound::ChordLindenmayer::createRotation)
        // NOT SUPPORTED
        // .function("equivalence", &csound::ChordLindenmayer::equivalence)
        .function("fixStatus", &csound::ChordLindenmayer::fixStatus)
        .function("generateLindenmayerSystem", &csound::ChordLindenmayer::generateLindenmayerSystem)
        .function("getAngle", &csound::ChordLindenmayer::getAngle)
        .function("getAxiom", &csound::ChordLindenmayer::getAxiom)
        .function("getDimension", &csound::ChordLindenmayer::getDimension)
        .function("getIterationCount", &csound::ChordLindenmayer::getIterationCount)
        .function("getReplacement", &csound::ChordLindenmayer::getReplacement)
        .function("initialize", &csound::ChordLindenmayer::initialize)
        .function("interpret", &csound::ChordLindenmayer::interpret)
        // NOT SUPPORTED
        // .function("parseCommand", &csound::ChordLindenmayer::parseCommand)
        .function("setAngle", &csound::ChordLindenmayer::setAngle)
        .function("setAxiom", &csound::ChordLindenmayer::setAxiom)
        .function("setIterationCount", &csound::ChordLindenmayer::setIterationCount)
        .function("tieOverlappingNotes", &csound::ChordLindenmayer::tieOverlappingNotes)
        .function("writeScore", &csound::ChordLindenmayer::writeScore)
    ;
    // FINISHED
    emscripten::class_<csound::ChordScore, emscripten::base<csound::Score> >("ChordScore")
        .constructor<>()
        .function("conformToChords", &csound::ChordScore::conformToChords)
        .function("getChord", &csound::ChordScore::getChord, emscripten::allow_raw_pointers())
        .function("insertChord", &csound::ChordScore::insertChord)
    ;
    // FINISHED
    emscripten::class_<csound::ChordSpaceGroup>("ChordSpaceGroup")
        .constructor<>()
        .function("createChordSpaceGroup", &csound::ChordSpaceGroup::createChordSpaceGroup)
        .function("createFilename", &csound::ChordSpaceGroup::createFilename)
        .function("fromChord", &csound::ChordSpaceGroup::fromChord)
        .function("initialize", &csound::ChordSpaceGroup::initialize)
        .function("list", &csound::ChordSpaceGroup::list)
        .function("load", &csound::ChordSpaceGroup::load)
        .function("preinitialize", &csound::ChordSpaceGroup::preinitialize)
        .function("save", &csound::ChordSpaceGroup::save)
        .function("toChord", &csound::ChordSpaceGroup::toChord)
        .property("countI", &csound::ChordSpaceGroup::getCountI)
        .property("countP", &csound::ChordSpaceGroup::getCountP)
        .property("countT", &csound::ChordSpaceGroup::getCountT)
        .property("countV", &csound::ChordSpaceGroup::getCountV)
        .property("g", &csound::ChordSpaceGroup::getG)
        .property("N", &csound::ChordSpaceGroup::getN)
        .property("range", &csound::ChordSpaceGroup::getRange)
     ;
    // FINISHED
    emscripten::class_<csound::Composition>("Composition")
        .constructor<>()
        .function("clear", &csound::Composition::clear)
        .function("clearOutputSoundfileName", &csound::Composition::clearOutputSoundfileName)
        .function("generate", &csound::Composition::generate)
        .function("generateAllNames", &csound::Composition::generateAllNames)
        .class_function("generateFilename", &csound::Composition::generateFilename)
        .function("getAlbum", &csound::Composition::getAlbum)
        .function("getArtist", &csound::Composition::getArtist)
        .function("getAuthor", &csound::Composition::getAuthor)
        .function("getBasename", &csound::Composition::getBasename)
        .function("getCdSoundfileFilepath", &csound::Composition::getCdSoundfileFilepath)
        .function("getConformPitches", &csound::Composition::getConformPitches)
        .function("getCopyright", &csound::Composition::getCopyright)
        .function("getDuration", &csound::Composition::getDuration)
        .function("getFileFilepath", &csound::Composition::getFileFilepath)
        .function("getFilename", &csound::Composition::getFilename)
        .function("getFomusfileFilepath", &csound::Composition::getFomusfileFilepath)
        .function("getLicense", &csound::Composition::getLicense)
        .function("getLilypondfileFilepath", &csound::Composition::getLilypondfileFilepath)
        .function("getMidifileFilepath", &csound::Composition::getMidifileFilepath)
        .function("getMp3SoundfileFilepath", &csound::Composition::getMp3SoundfileFilepath)
        .function("getMusicXmlfileFilepath", &csound::Composition::getMusicXmlfileFilepath)
        .function("getNormalizedSoundfileFilepath", &csound::Composition::getNormalizedSoundfileFilepath)
        .function("getOutputDirectory", &csound::Composition::getOutputDirectory)
        .function("getOutputSoundfileFilepath", &csound::Composition::getOutputSoundfileFilepath)
        .function("getPerformanceRightsOrganization", &csound::Composition::getPerformanceRightsOrganization)
        .function("getScore", &csound::Composition::getScore)
        .function("getTieOverlappingNotes", &csound::Composition::getTieOverlappingNotes)
        .function("getTimestamp", &csound::Composition::getTimestamp)
        .function("getTitle", &csound::Composition::getTitle)
        .function("getTonesPerOctave", &csound::Composition::getTonesPerOctave)
        .function("getYear", &csound::Composition::getYear)
        .class_function("makeTimestamp", &csound::Composition::makeTimestamp)
        .function("normalizeOutputSoundfile", &csound::Composition::normalizeOutputSoundfile)
        .function("perform", &csound::Composition::perform)
        .function("performAll", &csound::Composition::performAll)
        .function("performMaster", &csound::Composition::performMaster)
        .function("performAll", &csound::Composition::performAll)
        .function("processArgs", &csound::Composition::processArgs)
        .function("processArgv", &csound::Composition::processArgv, emscripten::allow_raw_pointers())
        .function("render", &csound::Composition::render)
        .function("renderAll", &csound::Composition::renderAll)
        .function("setAlbum", &csound::Composition::setAlbum)
        .function("setArtist", &csound::Composition::setArtist)
        .function("setAuthor", &csound::Composition::setAuthor)
        .function("setConformPitches", &csound::Composition::setConformPitches)
        .function("setCopyright", &csound::Composition::setCopyright)
        .function("setDuration", &csound::Composition::setDuration)
        .function("setFilename", &csound::Composition::setFilename)
        .function("setLicense", &csound::Composition::setLicense)
        .function("setOutputDirectory", &csound::Composition::setOutputDirectory)
        .function("setOutputSoundfileName", &csound::Composition::setOutputSoundfileName)
        .function("setPerformanceRightsOrganization", &csound::Composition::setPerformanceRightsOrganization)
        .function("setScore", &csound::Composition::setScore)
        .function("setTieOverlappingNotes", &csound::Composition::setTieOverlappingNotes)
        .function("setTitle", &csound::Composition::setTitle)
        .function("setTonesPerOctave", &csound::Composition::setTonesPerOctave)
        .function("setYear", &csound::Composition::setYear)
        .function("tagFile", &csound::Composition::tagFile)
        .function("translateMaster", &csound::Composition::translateMaster)
        .function("translateToCdAudio", &csound::Composition::translateToCdAudio)
        .function("translateToMp3", &csound::Composition::translateToMp3)
        .function("translateToMp4", &csound::Composition::translateToMp4)
        .function("translateToNotation", &csound::Composition::translateToNotation)
        .function("write", &csound::Composition::write, emscripten::allow_raw_pointers())
    ;
    // FINISHED
    emscripten::class_<csound::Conversions>("Conversions")
        .constructor<>()
        .class_function("amplitudeToDecibels", &csound::Conversions::amplitudeToDecibels)
        .class_function("amplitudeToGain", &csound::Conversions::amplitudeToGain)
        .class_function("amplitudeToMidi", &csound::Conversions::amplitudeToMidi)
        .class_function("boolToString", &csound::Conversions::boolToString)
        .class_function("decibelsToAmplitude", &csound::Conversions::decibelsToAmplitude)
        .class_function("decibelsToMidi", &csound::Conversions::decibelsToMidi)
        .class_function("doubleToString", &csound::Conversions::doubleToString)
        .class_function("dupstr", &csound::Conversions::dupstr, emscripten::allow_raw_pointers())
        .class_function("EPSILON", &csound::Conversions::EPSILON)
        .class_function("epsilonFactor", &csound::Conversions::epsilonFactor)
        .class_function("eq_epsilon", &csound::Conversions::eq_epsilon)
        .class_function("findClosestPitchClass", &csound::Conversions::findClosestPitchClass)
        .class_function("gainToAmplitude", &csound::Conversions::gainToAmplitude)
        .class_function("gainToDb", &csound::Conversions::gainToDb)
        .class_function("ge_epsilon", &csound::Conversions::ge_epsilon)
        .class_function("get2PI", &csound::Conversions::get2PI)
        .class_function("getMaximumAmplitude", &csound::Conversions::getMaximumAmplitude)
        .class_function("getMaximumDynamicRange", &csound::Conversions::getMaximumDynamicRange)
        .class_function("getMiddleCHz", &csound::Conversions::getMiddleCHz)
        .class_function("getNORM_7", &csound::Conversions::getNORM_7)
        .class_function("getPI", &csound::Conversions::getPI)
        .class_function("getSampleSize", &csound::Conversions::getSampleSize)
        .class_function("gt_epsilon", &csound::Conversions::gt_epsilon)
        .class_function("hzToMidi", &csound::Conversions::hzToMidi)
        .class_function("hzToOctave", &csound::Conversions::hzToOctave)
        .class_function("hzToSamplingIncrement", &csound::Conversions::hzToSamplingIncrement)
        .class_function("initialize", &csound::Conversions::initialize)
        .class_function("intToString", &csound::Conversions::intToString)
        .class_function("le_epsilon", &csound::Conversions::le_epsilon)
        .class_function("leftPan", &csound::Conversions::leftPan)
        .class_function("lt_epsilon", &csound::Conversions::lt_epsilon)
        .class_function("midiToAmplitude", &csound::Conversions::midiToAmplitude)
        .class_function("midiToDecibels", &csound::Conversions::midiToDecibels)
        .class_function("midiToGain", &csound::Conversions::midiToGain)
        .class_function("midiToHz", &csound::Conversions::midiToHz)
        .class_function("midiToOctave", &csound::Conversions::midiToOctave)
        .class_function("midiToPitchClass", &csound::Conversions::midiToPitchClass)
        .class_function("midiToPitchClassSet", &csound::Conversions::midiToPitchClassSet)
        .class_function("midiToRoundedOctave", &csound::Conversions::midiToRoundedOctave)
        .class_function("midiToSamplingIncrement", &csound::Conversions::midiToSamplingIncrement)
        .class_function("modulus", &csound::Conversions::modulus)
        .class_function("mToName", &csound::Conversions::mToName)
        .class_function("nameToM", &csound::Conversions::nameToM)
        .class_function("nameToPitches", &csound::Conversions::nameToPitches)
        .class_function("octaveToHz", &csound::Conversions::octaveToHz)
        .class_function("octaveToMidi", &csound::Conversions::octaveToMidi)
        .class_function("octaveToSamplingIncrement", &csound::Conversions::octaveToSamplingIncrement)
        .class_function("phaseToTableLengths", &csound::Conversions::phaseToTableLengths)
        .class_function("pitchClassSetToMidi", &csound::Conversions::pitchClassSetToMidi)
        .class_function("pitchClassToMidi", &csound::Conversions::pitchClassToMidi)
        .class_function("rightPan", &csound::Conversions::rightPan)
        .class_function("round", &csound::Conversions::round)
        .class_function("stringToBool", &csound::Conversions::stringToBool)
        .class_function("stringToDouble", &csound::Conversions::stringToDouble)
        .class_function("stringToInt", &csound::Conversions::stringToInt)
        .class_function("stringToVector", &csound::Conversions::stringToVector)
        .class_function("swapInt", &csound::Conversions::swapInt)
        .class_function("swapShort", &csound::Conversions::swapShort)
        .class_function("temper", &csound::Conversions::temper)
        // NOT SUPPORTED
        //.class_function("trim", &csound::Conversions::trim)
        // NOT SUPPORTED
        //.class_function("trimQuotes", &csound::Conversions::trimQuotes)
    ;    
    // FINISHED
    emscripten::class_<Counterpoint>("Counterpoint")
        .constructor<>()
        .function("ABS", &Counterpoint::ABS)
        .function("AddInterval", &Counterpoint::AddInterval)
        .function("ADissonance", &Counterpoint::ADissonance)
        .function("AnOctave", &Counterpoint::AnOctave)
        .function("AnySpecies", &Counterpoint::AnySpecies, emscripten::allow_raw_pointers())
        .function("ARRBLT", &Counterpoint::ARRBLT, emscripten::allow_raw_pointers())
        .function("ASeventh", &Counterpoint::ASeventh)
        .function("ASkip", &Counterpoint::ASkip)
        .function("AStep", &Counterpoint::AStep)
        .function("ATenth", &Counterpoint::ATenth)
        .function("AThird", &Counterpoint::AThird)
        .function("BadMelody", &Counterpoint::BadMelody)
        .function("Bass", &Counterpoint::Bass)
        .function("Beat8", &Counterpoint::Beat8)
        .function("BestFitFirst", &Counterpoint::BestFitFirst)
        .function("Cantus", &Counterpoint::Cantus)
        .function("Check", &Counterpoint::Check)
        .function("CleanRhy", &Counterpoint::CleanRhy)
        .function("clear", &Counterpoint::clear)
        .function("ConsecutiveSkipsInSameDirection", &Counterpoint::ConsecutiveSkipsInSameDirection)
        .function("counterpoint", &Counterpoint::counterpoint, emscripten::allow_raw_pointers())
        .function("CurRhy", &Counterpoint::CurRhy)
        .function("DirectMotionToPerfectConsonance", &Counterpoint::DirectMotionToPerfectConsonance)
        .function("Doubled", &Counterpoint::Doubled)
        .function("DownBeat", &Counterpoint::DownBeat)
        .function("ExtremeRange", &Counterpoint::ExtremeRange)
        .function("fillCantus", &Counterpoint::fillCantus)
        .function("FillRhyPat", &Counterpoint::FillRhyPat)
        .function("FirstNote", &Counterpoint::FirstNote)
        .function("GoodRhy", &Counterpoint::GoodRhy)
        .function("initialize", &Counterpoint::initialize)
        .function("InMode", &Counterpoint::InMode)
        .function("LastNote", &Counterpoint::LastNote)
        .function("Look", &Counterpoint::Look, emscripten::allow_raw_pointers())
        .function("MAX", &Counterpoint::MAX)
        //.function("message", &Counterpoint::message)
        .function("MIN", &Counterpoint::MIN)
        .function("MotionType", &Counterpoint::MotionType)
        .function("NextToLastNote", &Counterpoint::NextToLastNote)
        .function("Other", &Counterpoint::Other)
        .function("OtherVoiceCheck", &Counterpoint::OtherVoiceCheck)
        .function("OutOfRange", &Counterpoint::OutOfRange)
        .function("PitchRepeats", &Counterpoint::PitchRepeats)
        .function("RANDOM", &Counterpoint::RANDOM)
        .function("SaveIndx", &Counterpoint::SaveIndx, emscripten::allow_raw_pointers())
        .function("SaveResults", &Counterpoint::SaveResults)
        .function("SetUs", &Counterpoint::SetUs)
        .function("Size", &Counterpoint::Size)
        .function("SpecialSpeciesCheck", &Counterpoint::SpecialSpeciesCheck)
        .function("toCsoundScore", &Counterpoint::toCsoundScore)
        .function("TooMuchOfInterval", &Counterpoint::TooMuchOfInterval)
        .function("TotalRange", &Counterpoint::TotalRange)
        .function("UpBeat", &Counterpoint::UpBeat)
        .function("Us", &Counterpoint::Us)
        .function("UsedRhy", &Counterpoint::UsedRhy)
        .function("VIndex", &Counterpoint::VIndex)
        .function("winners", &Counterpoint::winners, emscripten::allow_raw_pointers())
    ;
    // FINISHED but multiple inheritance is not supported in Embind.
    emscripten::class_<csound::CounterpointNode, /* emscripten::base<Counterpoint>, */ emscripten::base<csound::Node> >("CounterpointNode")
        .constructor<>()
        .property("generationMode", &csound::CounterpointNode::getGenerationMode, &csound::CounterpointNode::setGenerationMode)
        .property("musicMode", &csound::CounterpointNode::getMusicMode, &csound::CounterpointNode::setMusicMode)
        .property("species", &csound::CounterpointNode::getSpecies, &csound::CounterpointNode::setSpecies)
        .property("voices", &csound::CounterpointNode::getVoices, &csound::CounterpointNode::setVoices)
        .property("secondsPerPulse", &csound::CounterpointNode::getSecondsPerPulse, &csound::CounterpointNode::setSecondsPerPulse)
        // NOT SUPPORTED
        //.property("voiceBeginnings", &csound::CounterpointNode::getVoiceBeginnings, &csound::CounterpointNode::setVoiceBeginnings, emscripten::allow_raw_pointers())
    ;
    emscripten::function("operator<", emscripten::select_overload<bool(const csound::Event&,const csound::Event&)>(&csound::operator<));
    // FINISHED
    emscripten::class_<csound::Event, emscripten::base<Eigen::VectorXd> >("Event")
        .constructor<>()
        .constructor<const csound::Event&>()
        .constructor<std::string>()
        .constructor<const Eigen::VectorXd&>()
        .constructor<double, double, double, double, double, double, double, double, double, double, double>()
        .constructor<std::vector<double> >()
        .function("clearProperties", &csound::Event::clearProperties)
        .function("conformToPitchClassSet", &csound::Event::conformToPitchClassSet)
        .function("createNoteOffEvent", &csound::Event::createNoteOffEvent)
        .function("dump", &csound::Event::dump)
        .function("getAmplitude", &csound::Event::getAmplitude)
        .function("getChannel", &csound::Event::getChannel)
        .function("getDepth", &csound::Event::getDepth)
        .function("getDuration", &csound::Event::getDuration)
        .function("getFrequency", &csound::Event::getFrequency)
        .function("getGain", &csound::Event::getGain)
        .function("getHeight", &csound::Event::getHeight)
        .function("getInstrument", &csound::Event::getInstrument)
        .function("getKey", &csound::Event::getKey)
        .function("getKey_tempered", &csound::Event::getKey_tempered)
        .function("getKeyNumber", &csound::Event::getKeyNumber)
        .function("getLeftGain", &csound::Event::getLeftGain)
        .function("getMidiStatus", &csound::Event::getMidiStatus)
        .function("getKeyNumber", &csound::Event::getKeyNumber)
        .function("getOffTime", &csound::Event::getOffTime)
        .function("getPan", &csound::Event::getPan)
        .function("getPhase", &csound::Event::getPhase)
        .function("getPitches", &csound::Event::getPitches)
        .function("getProperties", &csound::Event::getProperties)
        .function("getProperty", &csound::Event::getProperty)
        .function("getRightGain", &csound::Event::getRightGain)
        .function("getStatus", &csound::Event::getStatus)
        .function("getStatusNumber", &csound::Event::getStatusNumber)
        .function("getTime", &csound::Event::getTime)
        .function("getVelocity", &csound::Event::getVelocity)
        .function("getVelocityNumber", &csound::Event::getVelocityNumber)
        .function("initialize", &csound::Event::initialize)
        .function("isMatchingEvent", &csound::Event::isMatchingEvent)
        .function("isMatchingNoteOff", &csound::Event::isMatchingNoteOff)
        .function("isMidiEvent", &csound::Event::isMidiEvent)
        .function("isNote", &csound::Event::isNote)
        .function("isNoteOff", &csound::Event::isNoteOff)
        .function("isNoteOn", &csound::Event::isNoteOn)
        .function("operator=", emscripten::select_overload<csound::Event&(const csound::Event&)>(&csound::Event::operator=))
        .function("removeProperty", &csound::Event::removeProperty)
        .function("set", &csound::Event::set)
        .function("setAmplitude", &csound::Event::setAmplitude)
        .function("setDepth", &csound::Event::setDepth)
        .function("setDuration", &csound::Event::setDuration)
        .function("setFrequency", &csound::Event::setFrequency)
        .function("setHeight", &csound::Event::setHeight)
        .function("setInstrument", &csound::Event::setInstrument)
        .function("setKey", &csound::Event::setKey)
        .function("setOffTime", &csound::Event::setOffTime)
        .function("setPan", &csound::Event::setPan)
        .function("setPhase", &csound::Event::setPhase)
        .function("setPitches", &csound::Event::setPitches)
        .function("setProperty", &csound::Event::setProperty)
        .function("setStatus", &csound::Event::setStatus)
        .function("setTime", &csound::Event::setTime)
        .function("setVelocity", &csound::Event::setVelocity)
        .function("temper", &csound::Event::temper)
        .function("toCsoundIStatement", &csound::Event::toCsoundIStatement)
        .function("toCsoundIStatementHeld", &csound::Event::toCsoundIStatementHeld)
        .function("toCsoundIStatementRelease", &csound::Event::toCsoundIStatementRelease)
        .function("toString", &csound::Event::toString)
    ;
    emscripten::function("hello", &hello);
    // FINISHED
    emscripten::class_<csound::Intercut, emscripten::base<csound::ScoreNode> >("Intercut")
        .constructor<>()
    ;
    // FINISHED
    emscripten::class_<csound::KMeansMCRM, emscripten::base<csound::MCRM> >("KMeansMCRM")
        .constructor<>()
        .function("deterministic_algorithm", &csound::KMeansMCRM::deterministic_algorithm)
        .function("iterate", &csound::KMeansMCRM::iterate, emscripten::allow_raw_pointers())
        .function("means_to_notes", &csound::KMeansMCRM::means_to_notes)
        .function("random_algorithm", &csound::KMeansMCRM::random_algorithm)
    ;
    // FINISHED
    emscripten::class_<csound::Koch, emscripten::base<csound::ScoreNode> >("Koch")
        .constructor<>()
        .function("setPitchOffsetForLayer", &csound::Koch::setPitchOffsetForLayer)
    ;
    // FINISHED
    emscripten::class_<csound::Lindenmayer, emscripten::base<csound::ScoreNode> >("Lindenmayer")
        .constructor<>()
        .function("addRule", &csound::Lindenmayer::addRule)
        .function("getAngle", &csound::Lindenmayer::getAngle)
        .function("getAxiom", &csound::Lindenmayer::getAxiom)
        .function("getIterationCount", &csound::Lindenmayer::getIterationCount)
        .function("setAngle", &csound::Lindenmayer::setAngle)
        .function("setAxiom", &csound::Lindenmayer::setAxiom)
        .function("setIterationCount", &csound::Lindenmayer::setIterationCount)
    ;
    /* NOT SUPPORTED 
    // ECL doesn't yet build for WebAssembly as far as I know.
    // FINISHED
    emscripten::class_<csound::LispGenerator, emscripten::base<csound::LispNode> >("LispGenerator")
        .constructor<>()
    ;
    // FINISHED
    emscripten::class_<csound::LispNode, emscripten::base<csound::Node> >("LispNode")
        .constructor<>()
        .function("appendTopLevelForm", &csound::LispNode::appendTopLevelForm)
        .function("getStringFromForm", &csound::LispNode::getStringFromForm)
        .function("getTopLevelForms", &csound::LispNode::getTopLevelForms)
     ;
    // FINISHED
    emscripten::class_<csound::LispTransformer, emscripten::base<csound::LispNode> >("LispTransformer")
        .constructor<>()
    ;
    */
    // FINISHED
    emscripten::class_<csound::MCRM, emscripten::base<csound::ScoreNode> >("MCRM")
        .constructor<>()
        .function("iterate", &csound::MCRM::iterate)
        .function("resize", &csound::MCRM::resize)
        .function("setDepth", &csound::MCRM::setDepth)
        .function("setTransformationElement", &csound::MCRM::setTransformationElement)
        .function("setWeight", &csound::MCRM::setWeight)
    ;
    // FINISHED
    emscripten::class_<csound::Node>("Node")
        .constructor<>()
        .function("addChild", &csound::Node::addChild, emscripten::allow_raw_pointers())
        .function("childCount", &csound::Node::childCount)
        .function("clear", &csound::Node::clear)
        .function("createTransform", &csound::Node::createTransform)
        .function("element", &csound::Node::element)
        .function("generate", &csound::Node::generate)
        .function("getChild", &csound::Node::getChild, emscripten::allow_raw_pointers())
        .function("getLocalCoordinates", &csound::Node::getLocalCoordinates)
        .function("setElement", &csound::Node::setElement)
        .function("transform", &csound::Node::transform)
        .function("traverse", &csound::Node::traverse)
    ;
    // FINISHED
    emscripten::class_<csound::Random, emscripten::base<csound::Node> >("Random")
        .constructor<>()
        .function("createDistribution", &csound::Random::createDistribution)
        .function("getRandomCoordinates", &csound::Random::getRandomCoordinates)
        .function("sample", &csound::Random::sample)
        .function("seed", &csound::Random::seed)
    ;
    // FINISHED
    emscripten::class_<csound::RemoveDuplicates, emscripten::base<csound::Node> >("RemoveDuplicates")
        .constructor<>()
    ;
    // FINISHED
    emscripten::class_<csound::Rescale, emscripten::base<csound::Node> >("Rescale")
        .constructor<>()
       .function("getRescale", &csound::Rescale::getRescale)
       .function("initialize", &csound::Rescale::initialize)
       .function("setRescale", &csound::Rescale::setRescale)
    ;
    // FINISHED
    emscripten::class_<csound::Scale>("Scale")
        .constructor<>()
        .constructor<std::string>()
        .constructor<std::string, const csound::Chord&>()
        .function("chord", &csound::Scale::chord)
        .function("operator=", &csound::Scale::operator=)
        .function("getTypeName", &csound::Scale::getTypeName)
        .function("modulations", &csound::Scale::modulations)
        .function("modulations_for_scale_types", &csound::Scale::modulations_for_scale_types)
        .function("relative_tonicizations", &csound::Scale::relative_tonicizations)
        .function("relative_tonicizations_for_scale_types", &csound::Scale::relative_tonicizations_for_scale_types)
        .function("secondary", &csound::Scale::secondary)
        .function("semitones_for_degree", &csound::Scale::semitones_for_degree)
        .function("tonic", &csound::Scale::tonic)
        .function("tonicizations", &csound::Scale::tonicizations)
        .function("transpose", &csound::Scale::transpose)
        .function("transpose_degrees", &csound::Scale::transpose_degrees)
        .function("transpose_to_degree", &csound::Scale::transpose_to_degree)
    ;
    emscripten::class_<csound::Score>("Score")
        .constructor<>()
        .function("chord", &csound::Score::chord)
    ;
    // FINISHED
    emscripten::class_<csound::ScoreNode, emscripten::base<csound::Node> >("ScoreNode")
        .constructor<>()
        .function("getScore", &csound::ScoreNode::getScore, emscripten::allow_raw_pointers())
    ;
    emscripten::class_<Eigen::VectorXd>("VectorXd")
        .constructor<>()
    ;
 
}