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
#ifndef SILENCEMCRM_H
#define SILENCEMCRM_H
#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%include "std_string.i"
%include "std_vector.i"
%{
#include "Silence.hpp"
#include <vector>
%}
#else
#include "Silence.hpp"
#include <vector>
#endif

    namespace csound
    {
    class SILENCE_PUBLIC MCRM :
        public ScoreNode
    {
    protected:
        // Hutchinson operator.
        std::vector< Eigen::MatrixXd > transformations;
        // Pseudo-Markov operator.
        Eigen::MatrixXd weights;
        // Depth of recursion.
        int depth;
        // Recursive iteration.
        virtual void iterate(int depth, size_t p, const Event &event, double weight);
    public:
        MCRM();
        virtual ~MCRM();
        void setDepth(int depth);
        void resize(size_t transformations);
        void setTransformationElement(size_t index, size_t row, size_t column, double value);
        void setWeight(size_t precursor, size_t successor, double weight);
        virtual void generate();
        // Node overrides.
        virtual void generate(Score &score);
    };

    /**
     * Uses k-means clustering to translate the accumulated samples that
     * approximate the measure on the iterated function system implemented
     * by the multiple copy reducing machine algorithm into a specified
     * number of notes.
     */
    class SILENCE_PUBLIC KMeansMCRM :
        public MCRM
    {
    public:
        /**
         * The type of algorithm used.
         */
        typedef enum {
            RANDOM = 1,
            DETERMINISTIC
        } ALGORITHM_TYPE;
        ALGORITHM_TYPE algorithm_type;
        enum {
            // time, duration, channel, key, velocity, pan
            MEASURE_DIMENSIONS=6
        };
        /**
         * The number of times a sample is to be generated,
         * whether randomly or deterministically.
         */
        size_t sample_count;
        /**
         * The same as k: the number of centroids or means to be computed
         * from the samples, and later translated to notes.
         */
        size_t means_count;
        /**
         * The accumulated samples that approximate the measure of the IFS.
         */
        std::vector< std::array<double, MEASURE_DIMENSIONS> > samples;
        KMeansMCRM();
        virtual ~KMeansMCRM();
        virtual void iterate(int depth, size_t p, const Event &event, double weight);
        virtual void generate();
        virtual void random_algorithm();
        virtual void deterministic_algorithm();
        virtual void means_to_notes();
    };

    }
#endif
