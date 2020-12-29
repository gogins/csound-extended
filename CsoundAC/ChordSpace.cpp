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
#define EIGEN_INITIALIZE_MATRICES_BY_ZERO
// Header file only library.
#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%{
#include <algorithm>
#include <cfloat>
#include <ChordSpace.hpp>
#include <climits>
#include <cmath>
#include <cstdarg>
#include <eigen3/Eigen/Dense>
#include <Event.hpp>
#include <functional>
#include <iostream>
#include <iterator>
#include <map>
#include <Score.hpp>
#include <set>
#include <sstream>
#include <vector>
%}
%include "std_string.i"
%include "std_vector.i"
#else
#include <algorithm>
// Header file only library.
#include <boost/math/special_functions/ulp.hpp>
#include <cfloat>
// Header file only library.
#include "ChordSpace.hpp"
#include <climits>
#include <cmath>
#include <csignal>
#include <cstdarg>
#include <eigen3/Eigen/Dense>
#include "Event.hpp"
#include <functional>
#include <iostream>
#include <iterator>
#include <map>
#include <random>
#include "Score.hpp"
#include <set>
#include <sstream>
#include <vector>
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat"

namespace csound {
    
SILENCE_PUBLIC std::vector<Chord> allOfEquivalenceClass(int voice_count, std::string equivalence_class, double range, double g, int sector, bool printme) {
    std::vector<Chord> fundamental_domain;
    if (equivalence_class == "RP") {
        fundamental_domain = fundamentalDomainByPredicate<EQUIVALENCE_RELATION_RP>(voice_count, range, g, sector, printme);       
    } else if (equivalence_class == "RPT") {
        fundamental_domain = fundamentalDomainByPredicate<EQUIVALENCE_RELATION_RPT>(voice_count, range, g, sector, printme);       
    } else if (equivalence_class == "RPTg") {
        fundamental_domain = fundamentalDomainByPredicate<EQUIVALENCE_RELATION_RPTg>(voice_count, range, g, sector, printme);       
    } else if (equivalence_class == "RPI") {
        fundamental_domain = fundamentalDomainByPredicate<EQUIVALENCE_RELATION_RPI>(voice_count, range, g, sector, printme);       
    } else if (equivalence_class == "RPTI") {
        fundamental_domain = fundamentalDomainByPredicate<EQUIVALENCE_RELATION_RPTI>(voice_count, range, g, sector, printme);       
    } else if (equivalence_class == "RPTgI") {
        fundamental_domain = fundamentalDomainByPredicate<EQUIVALENCE_RELATION_RPTgI>(voice_count, range, g, sector, printme);       
    }
    return fundamental_domain;
}
    
SILENCE_PUBLIC Chord gather(Score &score, double startTime, double endTime) {
    std::vector<Event *> slice_ = slice(score, startTime, endTime);
    std::set<double> pitches;
    for (int i = 0; i < slice_.size(); ++i) {
        pitches.insert(slice_[i]->getKey());
    }
    Chord chord;
    chord.resize(pitches.size());
    int voice = 0;
    for (std::set<double>::iterator it = pitches.begin(); it != pitches.end(); ++it) {
        chord.setPitch(voice, *it);
        voice++;
    }
    return chord;
}

Event note(const Chord &chord,
    int voice,
    double time_,
    double duration_,
    double channel_,
    double velocity_,
    double pan_) {
    Event note;
    note.setTime(time_);
    note.setKey(chord.getPitch(voice));
    if (duration_ != DBL_MAX) {
        note.setDuration(duration_);
    } else {
        note.setDuration(chord.getDuration(voice));
    }
    if (channel_ != DBL_MAX) {
        note.setInstrument(channel_);
    } else {
        note.setInstrument(chord.getInstrument(voice));
    }
    if (velocity_ != DBL_MAX) {
        note.setVelocity(velocity_);
    } else {
        note.setVelocity(chord.getLoudness(voice));
    }
    if (pan_ != DBL_MAX) {
        note.setPan(pan_);
    } else {
        note.setPan(chord.getPan(voice));
    }
    return note;
}

Score notes(const Chord &chord,
    double time_,
    double duration_,
    double channel_,
    double velocity_,
    double pan_) {
    Score score;
    for (int voice = 0; voice < chord.voices(); ++voice) {
        Event event = note(chord, voice, time_, duration_, channel_, velocity_, pan_);
        score.append(event);
    }
    return score;
}

SILENCE_PUBLIC std::vector<Event *> slice(Score &score, double startTime, double endTime) {
    std::vector<Event *> result;
    for (int i = 0, n = score.size(); i < n; ++i) {
        Event *event = &score[i];
        if (event->isNoteOn()) {
            double eventStart = event->getTime();
            if (eventStart >= startTime && eventStart < endTime) {
                result.push_back(event);
            }
        }
    }
    return result;
}

SILENCE_PUBLIC void toScore(const Chord &chord, 
    Score &score,
    double time_, bool voiceIsInstrument) {
    for (int voice = 0; voice < chord.voices(); ++voice) {
        double instrument = double(voice);
        if (!voiceIsInstrument) {
            instrument = chord.getInstrument(voice);
        }
        score.append(time_,
                     chord.getDuration(voice),
                     144.0,
                     instrument,
                     chord.getPitch(voice),
                     chord.getLoudness(voice),
                     0.0,
                     chord.getPan(voice));
    }
}

SILENCE_PUBLIC void apply(Score &score, const Chord &chord, double startTime, double endTime, bool octaveEquivalence) {
    std::vector<Event *> slice_ = slice(score, startTime, endTime);
    for (int i = 0; i < slice_.size(); ++i) {
        Event &event = *slice_[i];
        conformToChord_equivalence(event, chord, octaveEquivalence);
    }
}

SILENCE_PUBLIC void conformToChord_equivalence(Event &event, const Chord &chord, bool octaveEquivalence) {
    if (!event.isNoteOn()) {
        return;
    }
    double pitch = event.getKey();
    if (octaveEquivalence) {
        Chord pcs = chord.epcs();
        pitch = conformToPitchClassSet(pitch, pcs);
    } else {
        pitch = closestPitch(pitch, chord);
    }
    event.setKey(pitch);
}

SILENCE_PUBLIC void conformToChord(Event &event, const Chord &chord) {
    conformToChord_equivalence(event, chord, true);
}

SILENCE_PUBLIC void ChordScore::insertChord(double tyme, const Chord chord) {
    chords_for_times[tyme] = chord;
}
/**
 * Returns a pointer to the first chord that starts at or after the
 * specified time. If there is no such chord, a null pointer is returned.
 */
SILENCE_PUBLIC Chord *ChordScore::getChord(double time_) {
    auto it = chords_for_times.lower_bound(time_);
    if (it != chords_for_times.end()) {
        return &it->second;
    } else {
        return nullptr;
    }
}

/**
 * Conforms the pitch-classes of the events in this to the closest
 * pitch-class of the chord, if any, that obtains at that time.
 */
SILENCE_PUBLIC void ChordScore::conformToChords(bool tie_overlaps, bool octave_equivalence) {
    sort();
    if (chords_for_times.begin() != chords_for_times.end()) {
        for (auto event_iterator = begin(); event_iterator != end(); ++event_iterator) {
            auto chord_iterator = chords_for_times.lower_bound(event_iterator->getTime());
            if (chord_iterator != chords_for_times.end()) {
                conformToChord_equivalence(*event_iterator, chord_iterator->second, octave_equivalence);
            }
        }
    }
    if (tie_overlaps == true) {
        tieOverlappingNotes(true);
    }
}

SILENCE_PUBLIC void ChordScore::getScale(std::vector<Event> &score, int dimension, size_t beginAt, size_t endAt, double &minimum, double &range)
{
    if(beginAt == endAt) {
        return;
    }
    const Event &beginEvent = score[beginAt];
    double maximum = beginEvent[dimension];
    const Event &endEvent = score[endAt - 1];
    minimum = endEvent[dimension];
    if(dimension == Event::TIME) {
        const Event &e = score[beginAt];
        maximum = std::max(e.getTime(), e.getTime() + e.getDuration());
        minimum = std::min(e.getTime(), e.getTime() + e.getDuration());
        double beginning;
        double ending;
        for( ; beginAt != endAt; ++beginAt) {
            const Event &event = score[beginAt];
            beginning = std::min(event.getTime(), event.getTime() + event.getDuration());
            ending = std::max(event.getTime(), event.getTime() + event.getDuration());
            if(ending > maximum) {
                maximum = ending;
            } else if(beginning < minimum) {
                minimum = beginning;
            }
        }
        // Also take into account chord times.
        auto chord_begin = chords_for_times.begin();
        auto chord_rbegin = chords_for_times.rbegin();
        if (chord_begin != chords_for_times.end() && chord_rbegin != chords_for_times.rend()) {
            minimum = std::min(minimum, chord_begin->first);
            maximum = std::max(maximum, chord_rbegin->first);
        }
    } else {
        for( ; beginAt != endAt; ++beginAt) {
            const Event &event = score[beginAt];
            if(event[dimension] > maximum) {
                maximum = event[dimension];
            }
            if(event[dimension] < minimum) {
                minimum = event[dimension];
            }
        }
    }
    range = maximum - minimum;
}

SILENCE_PUBLIC void ChordScore::setScale(std::vector<Event> &score,
              int dimension,
              bool rescaleMinimum,
              bool rescaleRange,
              size_t beginAt,
              size_t endAt,
              double targetMinimum,
              double targetRange)
{
    if(!(rescaleMinimum || rescaleRange)) {
        return;
    }
    if(beginAt == endAt) {
        return;
    }
    double actualMinimum;
    double actualRange;
    getScale(score, dimension, beginAt, endAt, actualMinimum, actualRange);
    double scale;
    if(actualRange == 0.0) {
        scale = 1.0;
    } else {
        scale = targetRange / actualRange;
    }
    for( ; beginAt != endAt; ++beginAt) {
        Event &event = score[beginAt];
        event[dimension] = event[dimension] - actualMinimum;
        if(rescaleRange) {
            event[dimension] = event[dimension] * scale;
        }
        if(rescaleMinimum) {
            event[dimension] = event[dimension] + targetMinimum;
        } else {
            event[dimension] = event[dimension] + actualMinimum;
        }
    }
    // Also rescale chord times.
    if (dimension == Event::TIME) {
        std::map<double, Chord> temp;
        for (auto it = chords_for_times.begin(); it != chords_for_times.end(); ++it) {
            double tyme = it->first;
            const Chord &chord = it->second;
            tyme = tyme - actualMinimum;
            if (rescaleRange) {
                tyme = tyme * scale;
            }
            if (rescaleMinimum) {
                tyme = tyme + targetMinimum;
            } else {
                tyme = tyme + actualMinimum;
            }
            temp[tyme] = chord;
        }
        chords_for_times = temp;
    }
}

SILENCE_PUBLIC double ChordScore::getDuration()
{
    double start = 0.0;
    double end = 0.0;
    for (int i = 0, n = size(); i < n; ++i) {
        const Event &event = at(i);
        if (i == 0) {
            start = event.getTime();
            end = event.getOffTime();
        } else {
            if (event.getTime() < start) {
                start = event.getTime();
            }
            if (event.getOffTime() > end) {
                end = event.getOffTime();
            }
        }
    }
    auto chord_begin = chords_for_times.begin();
    auto chord_rbegin = chords_for_times.rbegin();
    if (chord_begin != chords_for_times.end() && chord_rbegin != chords_for_times.rend()) {
        start = std::min(start, chord_begin->first);
        end = std::max(end, chord_rbegin->first);
    }
    return end - start;
}

SILENCE_PUBLIC void ChordScore::setDuration(double targetDuration)
{
    double currentDuration = getDuration();
    if (currentDuration == 0.0) {
        return;
    }
    double factor = targetDuration / currentDuration;
    for (size_t i = 0, n = size(); i < n; i++) {
        Event &event = (*this)[i];
        double time_ = event.getTime();
        double duration = event.getDuration();
        event.setTime(time_ * factor);
        event.setDuration(duration * factor);
    }
    std::map<double, Chord> temp;
    for (auto it = chords_for_times.begin(); it != chords_for_times.end(); ++it) {
        double tyme = it->first;
        const Chord &chord = it->second;
        tyme = tyme * factor;
        temp[tyme] = chord;
    }
    chords_for_times = temp;
}

SILENCE_PUBLIC void insert(Score &score,
                                  const Chord &chord,
                                  double time_) {
    toScore(chord, score, time_, true);
}

SILENCE_PUBLIC void insert(Score &score,
                                  const Chord &chord,
                                  double time_, 
                                  bool voice_is_instrument) {
    toScore(chord, score, time_, voice_is_instrument);
}



} // End of namespace csound.

#pragma GCC diagnostic push
