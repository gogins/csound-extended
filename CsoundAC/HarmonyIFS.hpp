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
%include "std_string.i"
%include "std_vector.i"
%include "std_map.i"
%{
#include "ChordSpaceBase.hpp"
#include "ChordSpace.hpp"
#include "Conversions.hpp"
#include "Event.hpp"
#include "Score.hpp"
#include "ScoreNode.hpp"
#include "Node.hpp"
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
#include "ChordSpaceBase.hpp"
#include "ChordSpace.hpp"
#include "Conversions.hpp"
#include "Event.hpp"
#include "Score.hpp"
#include "ScoreNode.hpp"
#include "Node.hpp"
#include "System.hpp"
#include <sstream>
#include <stack>
#include <string>
#include <map>
#include <vector>
#include <eigen3/Eigen/Dense>
#endif

namespace csound {
        
    /**
     * Represents a point on a time line in a score space that has a time-
     * harmony subspace. The point consists of a homogeneous column vector 
     * with dimensions: 
     * ```
     * t    Time.
     * P    Set class (or prime form).
     * I    Inversion (or reflection in the origin of pitch space).
     * T    Transposition (or translation in pitch space).
     * k    MIDI key number (the actual pitch, may be a fraction).
     * v    MIDI velocity (or loudness).
     * i    Instrument number (1-based).
     * 1    Homogeneity.
     * ```
     * At rendering time, the point will be translated to that pitch which 
     * most closely matches a pitch-class in that chord defined by P, I, 
     * and T.
     */     
    class SILENCE_PUBLIC HarmonyPoint : public Eigen::VectorXd {
         public:
            typedef enum
            {
                HP_TIME = 0,
                HP_PRIME_FORM,
                HP_INVERSION,
                HP_TRANSPOSITION,
                HP_MIDI_KEY,
                HP_MIDI_VELOCITY,
                HP_INSTRUMENT,
                HP_HOMOGENEITY,
                HP_ELEMENT_COUNT
            } Dimensions;
            HarmonyPoint() {
                initialize();
            };
            HarmonyPoint(const HarmonyPoint &other) {
                *this = other;
            }
            HarmonyPoint &operator = (const HarmonyPoint &other) {
                Eigen::VectorXd::operator = (other);
                return *this;
            };
            HarmonyPoint &operator = (const Eigen::VectorXd &other) {
                Eigen::VectorXd::operator = (other);
                return *this;
            };
            virtual ~HarmonyPoint() {};
            virtual void initialize() {
                resize(HP_ELEMENT_COUNT);
                operator *= (0);
                (*this)[HP_HOMOGENEITY] = 1.0;
            };
            virtual double get_t() const {
                return (*this)[HP_TIME];
            };
            virtual double get_P() const {
                return (*this)[HP_PRIME_FORM];
            };
            virtual double get_I() const {
                return (*this)[HP_INVERSION];
            };
            virtual double get_T() const {
                return (*this)[HP_TRANSPOSITION];
            };
            virtual double get_k() const {
                return (*this)[HP_MIDI_KEY];
            };
            virtual double get_v() const {
                return (*this)[HP_MIDI_VELOCITY];
            };
            virtual double get_i() const {
                return (*this)[HP_INSTRUMENT];
            };
            virtual void set_t(double value) {
                (*this)[HP_TIME] = value;
            };
            virtual void set_P(double value) {
                (*this)[HP_PRIME_FORM] = value;
            };
            virtual void set_I(double value) {
                (*this)[HP_INVERSION] = value;
            };
            virtual void set_T(double value) {
                (*this)[HP_TRANSPOSITION] = value;
            };
            virtual void set_k(double value) {
                (*this)[HP_MIDI_KEY] = value;
            };
            virtual void set_v(double value) {
                (*this)[HP_MIDI_VELOCITY] = value;
            };
            virtual void set_i(double value) {
                (*this)[HP_INSTRUMENT] = value;
            };
            virtual std::string toString() const {
                char buffer[0x1000];
                std::snprintf(buffer, 0x1000, "t: %9.4f P: %9.4f I: %9.4f T: %9.4f k: %9.4f v: %9.4f i: %9.4f 1: %9.4f\n", 
                    get_t(), get_P(), get_I(), get_T(), get_k(), get_v(), get_i(), (*this)[HP_HOMOGENEITY]);
                return buffer;
            };
     };
     
    /**
     * Represents an interpolation point for a fractal interpolation function in the
     * __time-harmony subspace__ of the score space, with dimensions:
     * ```
     * t    Time.
     * P    Prime form.
     * I    Inversion.
     * T    Transposition.
     * s_P  Scaling factor of prime form.
     * s_I  Scaling factor of inversion.
     * s_T  Scaling factor of transposition.
     *```
     */     
     class SILENCE_PUBLIC HarmonyInterpolationPoint : public Eigen::VectorXd {
         public:
            typedef enum
            {
                HIP_TIME = 0,
                HIP_PRIME_FORM,
                HIP_INVERSION,
                HIP_TRANSPOSITION,
                HIP_PRIME_FORM_SCALING,
                HIP_INVERSION_SCALING,
                HIP_TRANSPOSITION_SCALING,
                HIP_HOMOGENEITY,
                HIP_ELEMENT_COUNT
            } Dimensions;
            HarmonyInterpolationPoint();
            HarmonyInterpolationPoint(const HarmonyInterpolationPoint &other);
            HarmonyInterpolationPoint(double t, double P, double I, double T, double s_P, double s_I, double s_T);
            virtual ~HarmonyInterpolationPoint();
            virtual void initialize();
            virtual double get_t() const;
            virtual double get_P() const;
            virtual double get_I() const;
            virtual double get_T() const;
            virtual double get_s_P() const;
            virtual double get_s_I() const;
            virtual double get_s_T() const;
            virtual void set_t(double value);
            virtual void set_P(double value);
            virtual void set_I(double value);
            virtual void set_T(double value);
            virtual void set_s_P(double value);
            virtual void set_s_I(double value);
            virtual void set_s_T(double value);
            virtual std::string toString() const;
    };
    
    /**
     * Associates a Chord with an Event representing a musical note.
     */
    struct SILENCE_PUBLIC HarmonyEvent {
        Event note;
        Chord chord;
        const Event &get_note() const {
            return note;
        }
        void set_note(const Event &event) {
            note = event;
        }
        const Chord &get_chord() const {
            return chord;
        }
        void set_chord(const Chord &chord_) {
            chord = chord_;
        }
    };
    
    /**
     * HarmonyIFS is a class for doing algorithmic music composition by means 
     * of fractal interpolation functions. Scores are generated as the 
     * attractors of iterated function systems (IFS) in a score space that has 
     * a harmony subspace, in which time is subdivided such that harmony is a 
     * linear progression of time.
     *
     * Usage:  
     * <nl>
     * <li>Call `add_interpolation_point` several times or more to define 
     * the harmony by setting interpolation points for a harmony as a 
     * fractal function of time.</li>
     * <li>Call `initialize_hutchinson_operator` to mathematically translate 
     * the interpolation points to affine transformation matrices in a 
     * Hutchinson operator.</li>
     * <li>Call `set_transformation` as desired to add additional structure 
     * to the IFS that generates the score. Do not set matrix elements that 
     * will cause harmony to overlap time other than as specified by the 
     * interpolation points. However, pitch, time, instrument, and other 
     * dimensions may be transformed as desired.</li>
     * <li>Call `generate_score_attractor` with a desired depth of iteration 
     * actually generate the score.</li>
     * The HarmonyIFS object may then be included in a music graph, or used as 
     * a standalone score generator.</li>
     * </nl>
     */
    class SILENCE_PUBLIC HarmonyIFS : public ScoreNode {
        public:
            HarmonyIFS();
            virtual ~HarmonyIFS();
            virtual PITV &get_pitv();
            /**
             * Initialize the HarmonyIFS for N voices in a range of MIDI keys 
             * for a note duration in seconds. g is the generator of 
             * transposition.
             */
            virtual void initialize(int voices_, double range_, double bass_, double note_duration_, bool tie_overlaps_, bool rescale_, double g_ = 1.);
            /**
             * Adds an interpolation point to the graph of the fractal interpolation function.
             */
            virtual HarmonyInterpolationPoint add_interpolation_point(double t, double P, double I, double T, double s_P, double s_I, double s_T);
            /**
             * Interpolation points are sorted by time and the corresponding 
             * shear transformations for a Hutchinson operator are computed, 
             * according to Polychronis Manousopoulos, Vasileios Drakopoulos, 
             * and Theoharis Theoharis, "Curve Fitting by Fractal 
             * Interpolation." In: Transactions on Computational
             * Science 1 (Jan. 2008), pp. 85-103. 
             * doi: 10.1007/978-3-540-79299-4_4. Once this function has been 
             * called, the non-shear elements of the transformation matrices 
             * may be modified in any way as long as the Hutchinson operator
             * remains contractive.
             */
            virtual void initialize_hutchinson_operator();
            /** 
             * Translates a point in the attractor of the IFS to a note and 
             * associated chord.
             */
            virtual HarmonyEvent point_to_note(const HarmonyPoint &harmony_point);
            /**
             * Removes duplicate notes from the generated score.
             */
            virtual void remove_duplicate_notes();
            /**
             * Recursively computes the score graph, translates the points to 
             * notes, adds them to the score, ties overlapping notes in the 
             * score, and rescales the score.
             *
             * This function should be called __before__ rendering a music 
             * graph that contains this node.
             */
            virtual void generate_score_attractor(int depth);
            /**
             * Actully computes the score attractor.
             */
            virtual void iterate(int depth, int iteration, HarmonyPoint &point);
            /**
             * Processes the score attractor (the raw notes in the score) to 
             * quantize and rescale certain dimensions, to remove duplicate 
             * notes, and to conform pitches to chords.
             */
            virtual void translate_score_attractor_to_score();
            /**
             * Returns the number of affine transformation matrices in the 
             * Hutchinson operattor of the function system that generates the 
             * score.
             */
            virtual int get_transformation_count() const;
            /**
             * Sets the value of a single matrix element in one of the affine 
             * transformation matrices of the Hutchinson operator. The 
             * matrices are homeogenous transformations with 7 dimensions,
             * in column major order.
             */
            virtual void set_transformation(int transformation, int row, int column, double value); 
            int voices;
            double range;
            double bass;
            double note_duration;
            bool tie_overlaps;
            bool rescale;
            double g;
            PITV pitv;
            std::vector<HarmonyInterpolationPoint> interpolation_points;
            std::vector<Eigen::MatrixXd> hutchinson_operator;
            std::vector<HarmonyEvent> score_attractor;
     };
     
    class SILENCE_PUBLIC HarmonyIFS2 : public ScoreNode {
         public:
            HarmonyIFS2() {};
            virtual ~HarmonyIFS2() {};
    };
 
 }