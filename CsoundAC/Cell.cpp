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
#include <array>
#include "Cell.hpp"
#include "ChordSpaceBase.hpp"
#include "System.hpp"

namespace csound
{

Cell::Cell() : repeatCount(1), relativeDuration(true), durationSeconds(0)
{
}

Cell::~Cell()
{
}

void Cell::transform(Score &score)
{
    //  Find the total duration of notes produced by the child nodes of this.
    if(score.empty()) {
        return;
    }
    const Event &event = score.front();
    double beginSeconds = event.getTime();
    double endSeconds = beginSeconds;
    double totalDurationSeconds = 0;
    size_t beginAt = 0;
    size_t endAt = score.size();
    for(size_t i = beginAt; i < endAt; i++) {
        const Event &event = score[i];
        if (beginSeconds > event.getTime()) {
            beginSeconds = event.getTime();
        }
        if (endSeconds < event.getOffTime()) {
            endSeconds = event.getOffTime();
        }
    }
    if (relativeDuration) {
        totalDurationSeconds = durationSeconds + (endSeconds - beginSeconds);
    } else {
        totalDurationSeconds = durationSeconds;
    }
    System::message("Repeat section.\n");
    System::message(" Began    %9.4f\n", beginSeconds);
    System::message(" Ended    %9.4f\n", endSeconds);
    System::message(" Duration %9.4f\n", totalDurationSeconds);
    //  Repeatedly clone the notes produced by the child nodes of this,
    //  incrementing the time as required.
    double currentTime = beginSeconds;
    //  First "repeat" is already there!
    for(size_t i = size_t(1); i < (size_t) repeatCount; i++) {
        currentTime += totalDurationSeconds;
        System::message("  Repetition %d time %9.4f\n", i, currentTime);
        for(size_t j = beginAt; j < endAt; j++) {
            Event clonedEvent = score[j];
            clonedEvent.setTime(clonedEvent.getTime() + currentTime);
            score.push_back(clonedEvent);
        }
    }
}

Intercut::Intercut()
{
}

Intercut::~Intercut()
{
}

/**
 * The notes produced by each child node are intercut to produce
 * the notes produced by this; e.g. if there are 3 child nodes, then
 * the notes produced by this are node 0 note 0, node 1 note 0, node
 * 2 note 0; node 0 note 1, node 1 note 1, node node 2 note 1; node 0
 * note 2, node 1 note 2, node 2 note 2, and so on. If the child nodes
 * do not each produce the same number of notes, then the behavior
 * is controlled by the repeatEach flag. Chords are treated as single
 * notes.
 */

void Intercut::traverse(const Eigen::MatrixXd &globalCoordinates,
                        Score &collectingScore)
{
    Eigen::MatrixXd compositeCoordinates = getLocalCoordinates() * globalCoordinates;
    if (children.size() < 2) {
        System::message("Intercut must have at least 2 child nodes.\n");
    }
    std::vector<Score> scores;
    std::vector<size_t> scoreIndexes;
    scores.resize(children.size());
    scoreIndexes.resize(children.size());
    for (int i = 0, n = children.size(); i < n; ++i) {
        children[i]->traverse(compositeCoordinates, scores[i]);
        scores[i].sort();
    }
    bool finished = false;
    double startTime = scores[0][0].getTime();
    for (int intercut = 0; !finished; ++intercut) {
        finished = true;
        int scoreI = intercut % scores.size();
        const Score &intercutScore = scores[scoreI];
        if (scoreIndexes[scoreI] < intercutScore.size()) {
            finished = false;
        }
        for (bool inChord = true; inChord; ) {
            int noteI = scoreIndexes[scoreI] % intercutScore.size();
            ++scoreIndexes[scoreI];
            Event event = intercutScore[noteI];
            double eventTime = event.getTime();
            event.setTime(startTime);
            score.append(event);
            size_t nextNoteI = noteI + 1;
            if (nextNoteI >= intercutScore.size()) {
                inChord = false;
            } else if (eq_tolerance(intercutScore[nextNoteI].getTime(), eventTime)) {
                inChord = true;
            } else {
                inChord = false;
            }
            if (!inChord) {
                startTime = event.getOffTime();
            }
        }
    }
}

Stack::Stack() : duration(0.0)
{
}

Stack::~Stack()
{
}

void Stack::traverse(const Eigen::MatrixXd &globalCoordinates,
                     Score &collectingScore)
{
    Eigen::MatrixXd compositeCoordinates = getLocalCoordinates() * globalCoordinates;
    if (children.size() < 2) {
        System::message("Stack must have at least 2 child nodes.\n");
    }
    std::vector<Score> scores;
    scores.resize(children.size());
    for (int i = 0, n = children.size(); i < n; ++i) {
        children[i]->traverse(compositeCoordinates, scores[i]);
    }
    double newDuration = duration;
    if (duration == 0.0) {
        newDuration = scores[0].getDuration();
    }
    for (int i = 0, n = scores.size(); i < n; ++i) {
        Score &subScore = scores[i];
        subScore.setDuration(newDuration);
        for (size_t j = 0, k = subScore.size(); j < k; ++j) {
            score.append(subScore[j]);
        }
    }
}

Koch::Koch()
{
}

Koch::~Koch()
{
}

void Koch::setPitchOffsetForLayer(int layer, double offset)
{
    pitchOffsetsForLayers[layer] = offset;
}

void Koch::traverse(const Eigen::MatrixXd &globalCoordinates,
                    Score &collectingScore)
{
    Eigen::MatrixXd compositeCoordinates = getLocalCoordinates() * globalCoordinates;
    if (children.size() < 2) {
        System::message("Koch must have at least 2 child nodes.\n");
    }
    Eigen::MatrixXd rescaler = Eigen::MatrixXd::Identity(Event::ELEMENT_COUNT, Event::ELEMENT_COUNT);
    // All notes produced by upper are stacked on top of each note produced by lower.
    // This is the uppermost layer.
    Score upperScore;
    children.back()->traverse(compositeCoordinates, score);
    for (int lowerI = children.size() - 2; lowerI > -1; lowerI--) {
        upperScore = score;
        upperScore.sort();
        score.clear();
        System::message("level: %4d  upperScore: %8d events.\n", lowerI, upperScore.size());
        upperScore.findScale();
        Event upperScoreToOrigin = upperScore.scaleActualMinima;
        Score lowerScore;
        children[lowerI]->traverse(compositeCoordinates, lowerScore);
        lowerScore.sort();
        System::message("level: %4d  lowerScore: %8d events.\n", lowerI, lowerScore.size());
        double pitchOffset = 0.0;
        int layer = lowerI + 1;
        if (pitchOffsetsForLayers.find(layer) != pitchOffsetsForLayers.end()) {
            pitchOffset = pitchOffsetsForLayers[layer];
        }
        for (size_t lowerNoteI = 0, lowerNoteN = lowerScore.size();
                lowerNoteI < lowerNoteN;
                ++lowerNoteI) {
            Event lowerEvent = lowerScore[lowerNoteI];
            score.append(lowerEvent);
            double durationRatio = lowerEvent.getDuration() / upperScore.getDuration();
            Eigen::MatrixXd rescaler = Eigen::MatrixXd::Identity(Event::ELEMENT_COUNT, Event::ELEMENT_COUNT);
            // Time: Move to origin of lower note and rescale by duration of lower note.
            rescaler(Event::TIME, Event::HOMOGENEITY) = lowerEvent.getTime();
            rescaler(Event::TIME, Event::TIME) = durationRatio;
            // Duration: Rescale by duration of lower note.
            rescaler(Event::DURATION, Event::DURATION) = durationRatio;
            // Pitch: Move to lower note minus first upper note.
            rescaler(Event::KEY, Event::HOMOGENEITY) = lowerEvent.getKey() - pitchOffset + upperScoreToOrigin.getKey();
            // Velocity: Move to lower note minus first upper note.
            rescaler(Event::VELOCITY, Event::HOMOGENEITY) = lowerEvent.getVelocity() - upperScoreToOrigin.getVelocity();
            for (size_t upperNoteI = 0, upperNoteN = upperScore.size();
                    upperNoteI < upperNoteN;
                    ++upperNoteI) {
                Eigen::VectorXd upperEvent = rescaler * upperScore[upperNoteI];
                score.append(upperEvent);
            }
        }
        System::message("level: %4d  generated:  %8d events.\n", lowerI, score.size());
    }
}

/**
 * All notes produced by child nodes are repeated for the specified number of
 * iterations, beginning at the start index and proceeding up to but not
 * including the end index, at the specified stride. If absolute_duration is
 * true, then the next repetition occurs after that duration; if false, then
 * the indicated duration is added to the total duration of the repeated
 * notes.
 */
CellRepeat::CellRepeat()
{
}

CellRepeat::~CellRepeat()
{
}

void CellRepeat::transform(Score &score)
{
    System::inform("CellRepeat...\n");
    System::inform("    source:      %8d\n", score.size());
    if(score.empty()) {
        return;
    }
    // Find the end.
    size_t end_ = std::min(end, score.size());
    System::inform("    start:       %8d\n", start);
    System::inform("    end:         %8d\n", end_);
    System::inform("    stride:      %8d\n", stride);
    Score cell;
    for (size_t i = start; i < end_; i = i + stride) {
        cell.push_back(score[i]);
    }
    // Find the duration of the cell to be repeated.
    cell.sort();
    double cell_start = cell.front().getTime();
    double cell_end = cell.back().getOffTime();
    // This duration might not begin at 0.
    double cell_duration = cell_end - cell_start;
    double repetition_duration;
    if (absolute_duration) {
        repetition_duration = duration;
    } else {
        repetition_duration = cell_duration + duration;
    }
    // Then simply repeat the cell.
    Score repeated_cells;
    double begin_repetition_at = 0;
    for (int iteration = 0; iteration < iterations; ++iteration) {
        System::inform("CellRepeat:\n");
        System::inform("    iteration:   %8d\n", iteration);
        System::inform("    repeat duration: %9.4f\n", repetition_duration);
        System::inform("    cell duration:   %9.4f\n", cell_duration);
        System::inform("    cell:        %8d\n", cell.size());
        begin_repetition_at = iteration * repetition_duration;
        System::inform("    start at:        %9.4f\n", begin_repetition_at);
        for (int i = 0; i < cell.size(); ++i) {
            Event event = cell[i];
            event.setTime(event.getTime() + begin_repetition_at);
            repeated_cells.push_back(event);
        }
        System::inform("    produced:    %8d\n", repeated_cells.size());
    }
    score = repeated_cells;
}

void CellRepeat::repeat(size_t iterations_, double duration_, bool absolute_duration_,
                        size_t start_,
                        size_t end_,
                        size_t stride_)
{
    iterations = iterations_;
    duration = duration_;
    absolute_duration = absolute_duration_;
    start = start_;
    end = end_;
    stride = stride_;
}

void CellAdd::transform(Score &score)
{
    size_t end_ = std::min(end, score.size());
    for (size_t i = start; i < end_; i += stride) {
        Event &event = score[i];
        event[dimension] = event[dimension] + value;
    }
}

void CellAdd::add(Event::Dimensions dimension_, double value_, size_t start_, size_t end_, size_t stride_)
{
    dimension = dimension_;
    value = value_;
    start = start_;
    end = end_;
    stride = stride_;
}

void CellMultiply::transform(Score &score)
{
    size_t end_ = std::min(end, score.size());
    for (size_t i = start; i < end_; i += stride) {
        Event &event = score[i];
        event[dimension] = value * event[dimension];
    }
}

void CellMultiply::multiply(Event::Dimensions dimension_, double value_, size_t start_, size_t end_, size_t stride_)
{
    dimension = dimension_;
    value = value_;
    start = start_;
    end = end_;
    stride = stride_;
}

void CellReflect::transform(Score &score)
{
    size_t end_ = std::min(end, score.size());
    for (size_t i = start; i < end_; i += stride) {
        Event &event = score[i];
        event[dimension] = value - event[dimension];
    }
}

void CellReflect::reflect(Event::Dimensions dimension_, double value_, size_t start_, size_t end_, size_t stride_)
{
    dimension = dimension_;
    value = value_;
    start = start_;
    end = end_;
    stride = stride_;
}

void CellSelect::transform(Score &score_)
{
    size_t end_ = std::min(end, score_.size());
    Score score;
    for (size_t i = start; i < end_; i += stride) {
        score.push_back(score_[i]);
    }
    score_ = score;
}

void CellSelect::select(size_t start_, size_t end_, size_t stride_)
{
    start = start_;
    end = end_;
    stride = stride_;
}

void CellRemove::transform(Score &score)
{
    size_t end_ = std::min(end, score.size());
    for(int i = end_ - 1; i >= start; i = i - stride, --end_) {
        double start_time = score[i].getTime();
        score.erase(score.begin() + i);
        double end_time = score[i].getTime();
        double delta_time = end_time - start_time;
        for (int j = i; j > end_; ++j) {
            score[j][Event::TIME] -= delta_time;
        }
    }
}

void CellRemove::remove(size_t start_, size_t end_, size_t stride_)
{
    start = start_;
    end = end_;
    stride = stride_;
}

void CellChord::transform(Score &score)
{
    size_t end_ = std::min(end, score.size());
    for(size_t i = start; i < end_; i += stride) {
        Event &event = score[i];
        conformToChord(event, chord_);
    }
}

void CellChord::chord(const Chord &chord__, size_t start_, size_t end_, size_t stride_)
{
    chord_ = chord__;
    start = start_;
    end = end_;
    stride = stride_;
}

void CellRandom::transform(Score &score)
{
    size_t end_ = std::min(end, score.size());
    createDistribution(distribution);
    for(size_t i = start; i < end_; i += stride) {
        Event &event = score[i];
        double value = sample();
        event[dimension] += value;
    }
}

void CellRandom::random(const std::string &distribution_,
                        Event::Dimensions dimension_,
                        size_t start_,
                        size_t end_,
                        size_t stride_)
{
    distribution = distribution_;
    dimension = dimension_;
    start = start_;
    end = end_;
    stride = stride_;
}

void CellShuffle::transform(Score &score)
{
    size_t end_ = std::min(end, score.size());
    std::vector<std::array<double, 2>> times;
    for (size_t i = start; i < end_; i+= stride) {
        const Event &event = score[i];
        times.push_back({event.getTime(), event.getDuration()});
    }
    std::random_shuffle(times.begin(), times.end());
    size_t time_iterator = 0;
    for (size_t i = start; i < end_; i+= stride) {
        Event &event = score[i];
        event.setTime(times[time_iterator][0]);
        event.setDuration(times[time_iterator][1]);
    }
}

void CellShuffle::shuffle(size_t start_, size_t end_, size_t stride_)
{
    start = start_;
    end = end_;
    stride = stride_;
}

}
