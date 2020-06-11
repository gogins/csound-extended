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
#ifndef CELL_H
#define CELL_H

#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%{
#include <limits>
#include <map>
#include "Random.hpp"
#include "ScoreNode.hpp"
#include "ChordSpace.hpp"
#include <eigen3/Eigen/Dense>
%}
#else
#include <limits>
#include <map>
#include "Random.hpp"
#include "ScoreNode.hpp"
#include "ChordSpace.hpp"
#include <eigen3/Eigen/Dense>
#endif

namespace csound
{
/**
 * Score node that simplifies building up structures of motivic cells,
 * and incrementally transforming them, as in Minimalism.
 */
class SILENCE_PUBLIC Cell : public ScoreNode
{
public:
    /**
     * The number of times to repeat the notes produced by the child nodes
     * of this.
     */
    int repeatCount;
    virtual int getRepeatCount() const {
        return repeatCount;
    }
    virtual void setRepeatCount(int count) {
        repeatCount = count;
    }
    /**
     * Indicates whether the durationSeconds of this cell is added
     * to the duration of the notes produced by the child nodes of this
     * (true) at each repetition, or is simply the duration of the
     * cell (false), in which case the notes of the Nth repetition may
     * (or may not) overlap the notes of the N + 1th repetition.
     */
    bool relativeDuration;
    virtual bool getRelativeDuration() const {
        return relativeDuration;
    }
    virtual void setRelativeDuration(bool value) {
        relativeDuration = value;
    }
    /**
     * If relativeDuraton is true, then this time is added to the
     * duration of the Nth repetition in order to obtain the
     * starting time of the N + 1th repetition; if relativeDuration is
     * false, then this time is added to the starting time of the Nth
     * repetition in order to obtain the starting time of the N + 1th
     * repetition.
     */
    double durationSeconds;
    virtual double getDurationSeconds() const {
        return durationSeconds;
    }
    virtual void setDurationSeconds(double value) {
        durationSeconds = value;
    }
    Cell();
    virtual ~Cell();
    virtual void transform(Score &score);
    virtual std::string getImportFilename() const {
        return importFilename;
    }
    virtual void setImportFilename(std::string filename) {
        importFilename = filename;
    }
};

/**
 * The notes produced by each child node are intercut to produce
 * the notes produced by this; e.g. if there are 3 child nodes, then
 * the notes produced by this are node 0 note 0, node 1 note 0, node
 * 2 note 0; node 0 note 1, node 1 note 1, node node 2 note 1; node 0
 * note 2, node 1 note 2, node 2 note 2, and so on. If the child nodes
 * do not each produce the same number of notes, then production stops
 * with the last note of the longest child.
 */
class SILENCE_PUBLIC Intercut :
    public ScoreNode
{
public:
    Intercut();
    virtual ~Intercut();
    virtual void traverse(const Eigen::MatrixXd &globalCoordinates,
                          Score &collectingScore);
};

/**
 * The notes produced by each (not all) child node,
 * are rescaled to all start at the same time, and last for the
 * same duration; that of the 0th child, or a specified
 * duration.
 */
class SILENCE_PUBLIC Stack :
    public ScoreNode
{
public:
    /**
     * If non-zero, then each the notes of each child node in turn are
     * rescaled to fit within this duration; if zero, then the notes
     * of each child node are rescaled to fit within the duration of the
     * first (0th) node.
     */
    double duration;
    virtual double getDuration() const {
        return duration;
    }
    virtual double setDuration(double value) {
        duration = value;
    }
    Stack();
    virtual ~Stack();
    virtual void traverse(const Eigen::MatrixXd &globalCoordinates,
                          Score &collectingScore);
};

/**
 * All notes produced by child[N - 1] are rescaled and stacked on top
 * of each note produced by child[N - 2], and so on.
 */
class SILENCE_PUBLIC Koch :
    public ScoreNode
{
public:
    std::map<int, double> pitchOffsetsForLayers;
    Koch();
    virtual ~Koch();
    virtual void traverse(const Eigen::MatrixXd &globalCoordinates,
                          Score &score);
    virtual void setPitchOffsetForLayer(int layer, double pitch);
};

/**
 * All notes produced by child nodes are repeated for the specified number of
 * iterations, beginning at the start index and proceeding up to but not
 * including the end index, at the specified stride. If absolute_duration is
 * true, then the next repetition occurs after that duration; if false, then
 * the indicated duration is added to the total duration of the repeated
 * notes.
 */
class SILENCE_PUBLIC CellRepeat :
    public Node
{
    size_t iterations = 1;
    double duration = 0;
    bool absolute_duration = false;
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    CellRepeat();
    virtual ~CellRepeat();
    virtual void transform(Score &score);
    virtual void repeat(size_t iterations, double duration, bool absolute_duration, size_t start, size_t end, size_t stride);
};

/**
 * The indicated factor is added to the indicated dimension of each note
 * produced by the child nodes of this, beginning at the start index and
 * proceeding up to but not including the end index, at the specified
 * stride. Each dimension may have its own factor.
 */
class SILENCE_PUBLIC CellAdd :
    public Node
{
    Event::Dimensions dimension = Event::Dimensions::TIME;
    double value = 0;
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    virtual void transform(Score &score);
    virtual void add(Event::Dimensions dimension, double value, size_t start, size_t end, size_t stride);
};

/**
 * The indicated dimension of each note produced by the child nodes of this,
 * beginning at the start index and proceeding up to but not including the end
 * index, at the specified stride, is multiplied by the indicated factor.
 * Each dimension may have its own factor.
 */
class SILENCE_PUBLIC CellMultiply :
    public Node
{
    Event::Dimensions dimension = Event::Dimensions::TIME;
    double value = 0;
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    virtual void transform(Score &score);
    virtual void multiply(Event::Dimensions dimension, double value, size_t start, size_t end, size_t stride);
};

/**
 * The indicated dimension of each note produced by the child nodes of this,
 * beginning at the start index and proceeding up to but not including the end
 * index, at the specified stride, is reflected (i.e. inverted) around the
 * indicated center.
 */
class SILENCE_PUBLIC CellReflect :
    public Node
{
    Event::Dimensions dimension = Event::Dimensions::TIME;
    double value = 0;
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    virtual void transform(Score &score);
    virtual void reflect(Event::Dimensions dimension, double value, size_t start, size_t end, size_t stride);
};

/**
 * The notes produced by the child nodes of this are returned as sampled from
 * the indicated start index, up to but not including the indicated end index,
 * at the indicated stride.
 */
class SILENCE_PUBLIC CellSelect :
    public Node
{
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    virtual void transform(Score &score);
    virtual void select(size_t start, size_t end, size_t stride);
};

/**
 * Notes are removed from the notes produced by the child nodes of this,
 * beginning at the indicated start index, up to but not including the
 * end index, at the indicated stride. The times of the child notes
 * are adjusted to close the gap, i.e. the times of the child notes are
 * rescaled to close gaps resulting from the deleted notes.
 */
class SILENCE_PUBLIC CellRemove :
    public Node
{
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    virtual void transform(Score &score);
    virtual void remove(size_t start, size_t end, size_t stride);
};

/**
 * Notes produced by the child nodes of this are conformed to the chord,
 * starting at the indicated start index, up to but not including the end
 * index, at the indicated stride.
 */
class SILENCE_PUBLIC CellChord :
    public Node
{
    Chord chord_;
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    virtual void transform(Score &score);
    virtual void chord(const Chord &chord, size_t start, size_t end, size_t stride);
};

/**
 * Notes produced by the child nodes of this, starting at the indicated start
 * index, up to but not including the indicated end index, at the indicated stride,
 * have added to them a random variable from the indicated distribution, rescaled
 * to the indicated minimum and range. Parameters for the random variable are set
 * as for the base Random node.
 */
class SILENCE_PUBLIC CellRandom :
    public Random
{
    std::string distribution;
    Event::Dimensions dimension = Event::Dimensions::KEY;
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    virtual void transform(Score &score);
    virtual void random(const std::string &distribution, Event::Dimensions dimension, size_t start, size_t end, size_t stride);
};

/**
 * Notes produced by the child nodes of this, starting at the indicated start
 * index, up to but not including the indicated end index, at the indicated stride,
 * are randomly shuffled as to time.
 */
class SILENCE_PUBLIC CellShuffle :
    public Random
{
    size_t start = 0;
    size_t end = std::numeric_limits<size_t>::max();
    size_t stride = 1;
public:
    virtual void transform(Score &score);
    virtual void shuffle(size_t start, size_t end, size_t stride);
};
}
#endif

