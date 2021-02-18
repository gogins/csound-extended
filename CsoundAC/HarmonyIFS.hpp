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
     * <pre>
     * t    Time.
     * P    Set class (or prime form).
     * I    Inversion (or reflection in the origin of pitch space).
     * T    Transposition (or translation in pitch space).
     * k    MIDI key number (the actual pitch, may be a fraction).
     * v    MIDI velocity (or loudness).
     * i    Instrument number (1-based).
     * 1    Homogeneity.
     * </pre>
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
            HarmonyInterpolationPoint() {
                initialize();
            }
            HarmonyInterpolationPoint(const HarmonyInterpolationPoint &other) {
                *this = other;
            }
            HarmonyInterpolationPoint(double t, double P, double I, double T, double s_P, double s_I, double s_T) {
                initialize();
                set_t(t);
                set_P(P);
                set_I(I);
                set_T(T);
                set_s_P(s_P);
                set_s_I(s_I);
                set_s_T(s_T);
            }
            HarmonyInterpolationPoint &operator = (const HarmonyInterpolationPoint &other) {
                Eigen::VectorXd::operator = (other);
                return *this;
            }
            HarmonyInterpolationPoint &operator = (const Eigen::VectorXd &other) {
                Eigen::VectorXd::operator = (other);
                return *this;
            }            
            virtual ~HarmonyInterpolationPoint() {
            }
            virtual void initialize() {
                resize(HIP_ELEMENT_COUNT);
                operator *= (0);
                (*this)[HIP_HOMOGENEITY] = 1.0;
            }
            virtual double get_t() const {
                return (*this)[HIP_TIME];
            }
            virtual double get_P() const {
                return (*this)[HIP_PRIME_FORM];
            }
            virtual double get_I() const {
                return (*this)[HIP_INVERSION];
            }
            virtual double get_T() const {
                return (*this)[HIP_TRANSPOSITION];
            }
            virtual double get_s_P() const {
                return (*this)[HIP_PRIME_FORM_SCALING];
            }
            virtual double get_s_I() const {
                return (*this)[HIP_INVERSION_SCALING];
            }
            virtual double get_s_T() const {
                return (*this)[HIP_TRANSPOSITION_SCALING];
            }
            virtual void set_t(double value) {
                (*this)[HIP_TIME] = value;
            }
            virtual void set_P(double value) {
                (*this)[HIP_PRIME_FORM] = value;
            }
            virtual void set_I(double value) {
                (*this)[HIP_INVERSION] = value;
            }
            virtual void set_T(double value) {
                (*this)[HIP_TRANSPOSITION] = value;
            }
            virtual void set_s_P(double value) {
                (*this)[HIP_PRIME_FORM_SCALING] = value;
            }  
            virtual void set_s_I(double value) {
                (*this)[HIP_INVERSION_SCALING] = value;
            }  
            virtual void set_s_T(double value) {
                (*this)[HIP_TRANSPOSITION_SCALING] = value;
            }  
            virtual std::string toString() const {
                char buffer[0x1000];
                std::snprintf(buffer, 0x1000, "t: %9.4f P: %9.4f I: %9.4f T: %9.4f s_P: %9.4f s_I: %9.4f s_T: %9.4f 1: %9.4f\n", 
                    get_t(), get_P(), get_I(), get_T(), get_s_P(), get_s_I(), get_s_T(), (*this)[HIP_HOMOGENEITY]);
                return buffer;
            }
                
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
    
    SILENCE_PUBLIC inline bool interpolation_point_less(const HarmonyInterpolationPoint &a, const HarmonyInterpolationPoint &b) {
        if (a.get_t() < b.get_t()) {
            return true;
        } else {
            return false;
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
     * <ol>
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
     * </ol>
     */
    class SILENCE_PUBLIC HarmonyIFS : public ScoreNode {
        public:
            HarmonyIFS() {
            }
            virtual ~HarmonyIFS() {
            }
            virtual PITV &get_pitv() {
                return pitv;
            }
            /**
             * Initialize the HarmonyIFS for N voices in a range of MIDI keys 
             * for a note duration in seconds. g is the generator of 
             * transposition.
             */
            virtual void initialize(int voices_, double range_, double bass_, double note_duration_, bool tie_overlaps_, bool rescale_, double g_ = 1.) {
                voices = voices_;
                range = range_;
                bass = bass_;
                note_duration = note_duration_;
                tie_overlaps = tie_overlaps_;
                rescale = rescale_;
                g = g_;
                pitv.initialize(voices_, range_, g_, true);
                interpolation_points.clear();
            }
            /**
             * Adds an interpolation point to the graph of the fractal interpolation function.
             */
            virtual HarmonyInterpolationPoint add_interpolation_point(double t, double P, double I, double T, double s_P, double s_I, double s_T) {
                HarmonyInterpolationPoint harmony_interpolation_point = HarmonyInterpolationPoint(t, P, I, T, s_P, s_I, s_T);
                interpolation_points.push_back(harmony_interpolation_point);
                return harmony_interpolation_point;        
            }
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
            virtual void initialize_hutchinson_operator() {
                hutchinson_operator.clear();
                std::sort(interpolation_points.begin(), interpolation_points.end(), interpolation_point_less);
                HarmonyInterpolationPoint p_0 = interpolation_points.front();
                HarmonyInterpolationPoint p_N = interpolation_points.back();
                for (int i = 1, n = interpolation_points.size(); i < n; ++i) {
                    HarmonyInterpolationPoint p_i_1 = interpolation_points[i - 1];
                    HarmonyInterpolationPoint p_i = interpolation_points[i];
                    Eigen::MatrixXd transformation = Eigen::MatrixXd::Identity(8, 8);
                    // t or time dimension.
                    transformation(0, 0) = (p_i.get_t() - p_i_1.get_t()) / (p_N.get_t() - p_0.get_t());
                    transformation(0, 7) = ((p_N.get_t() * p_i_1.get_t()) - (p_0.get_t() * p_i.get_t())) / (p_N.get_t() - p_0.get_t());
                    // P or prime-form dimension.
                    transformation(1, 0) = ((p_i.get_P() - p_i_1.get_P()) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_P() * ((p_N.get_P() - p_0.get_P()) / (p_N.get_t() - p_0.get_t())));
                    transformation(1, 1) = p_i.get_s_P();
                    transformation(1, 7) = (((p_N.get_t() * p_i_1.get_P()) - (p_0.get_t() * p_i.get_P())) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_P() * (((p_i.get_t() * p_0.get_P()) - (p_0.get_t() * p_N.get_P())) / (p_N.get_t() - p_0.get_t())));
                    // I or inversion dimension.
                    transformation(2, 0) = ((p_i.get_I() - p_i_1.get_I()) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_I() * ((p_N.get_I() - p_0.get_I()) / (p_N.get_t() - p_0.get_t())));
                    transformation(2, 2) = p_i.get_s_I();
                    transformation(2, 7) = (((p_N.get_t() * p_i_1.get_I()) - (p_0.get_t() * p_i.get_I())) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_I() * (((p_i.get_t() * p_0.get_I()) - (p_0.get_t() * p_N.get_I())) / (p_N.get_t() - p_0.get_t())));
                    // T or transposition dimension.
                    transformation(3, 0) = ((p_i.get_T() - p_i_1.get_T()) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_T() * ((p_N.get_T() - p_0.get_T()) / (p_N.get_t() - p_0.get_t())));
                    transformation(3, 3) = p_i.get_s_T();
                    transformation(3, 7) = (((p_N.get_t() * p_i_1.get_T()) - (p_0.get_t() * p_i.get_T())) / (p_N.get_t() - p_0.get_t())) - (p_i.get_s_T() * (((p_i.get_t() * p_0.get_T()) - (p_0.get_t() * p_N.get_T())) / (p_N.get_t() - p_0.get_t())));
                    hutchinson_operator.push_back(transformation);
                }
            }
            /** 
             * Translates a point in the attractor of the IFS to a note and 
             * associated chord.
             */
            virtual HarmonyEvent point_to_note(const HarmonyPoint &point) {
                HarmonyEvent event;
                event.note.setTime(point.get_t());
                event.note.setDuration(note_duration);
                event.note.setStatus(144);
                event.note.setInstrument(point.get_i());
                int P = std::round(point.get_P());
                int I = std::round(point.get_I());
                int T = std::round(point.get_T());
                event.chord = pitv.toChord(P, I, T, 0)[2];
                event.note.setKey(point.get_k());
                event.note.setVelocity(point.get_v());
                return event;
            }
            /**
             * Removes duplicate notes from the generated score.
             */
            virtual void remove_duplicate_notes() {
                System::inform("HarmonyIFS::remove_duplicate_notes: before: %d events...\n", score_attractor.size());
                std::map<std::string, Event> unique_events;
                for (auto &event : score) {
                    unique_events[event.toString()] = event;
                }
                score.clear();
                for (auto &event : unique_events) {
                    score.push_back(event.second);
                }
                System::inform("                                    after:  %d events.\n", score_attractor.size());
            }
            /**
             * Recursively computes the score graph, translates the points to 
             * notes, adds them to the score, ties overlapping notes in the 
             * score, and rescales the score.
             *
             * This function should be called __before__ rendering a music 
             * graph that contains this node.
             */
            virtual void generate_score_attractor(int depth) {
                System::inform("HarmonyIFS::generate_score_attractor: depth:  %d...\n", depth);
                score_attractor.clear();
                int iteration = 0;
                HarmonyPoint initial_point;
                initial_point.set_k(60.);
                initial_point.set_v(60.);
                initial_point.set_i(1);
                for (auto &transformation : hutchinson_operator) {
                    std::cerr << transformation << std::endl;
                }
                iterate(depth, iteration, initial_point);
                System::inform("                                      points: %d.\n", score.size());
                translate_score_attractor_to_score();
            }
            /**
             * Actully computes the score attractor.
             */
            virtual void iterate(int depth, int iteration, HarmonyPoint &point) {
                iteration = iteration + 1;
                if (iteration >= depth) {
                    HarmonyEvent event = point_to_note(point);
                    score_attractor.push_back(event);
                    return;
                }
                for (int i = 0, n = hutchinson_operator.size(); i < n; ++i) {
                    const Eigen::MatrixXd &T = hutchinson_operator[i];
                    point = T * point;
                    iterate(depth, iteration, point);
                }
            }
            /**
             * Processes the score attractor (the raw notes in the score) to 
             * quantize and rescale certain dimensions, to remove duplicate 
             * notes, and to conform pitches to chords.
             */
            virtual void translate_score_attractor_to_score() {
                System::inform("HarmonyIFS::translate_score_attractor_to_score...\n");
                score.clear();
                System::inform("  Rescaling, tempering, and conforming notes...\n");
                double minimum_time = score_attractor.front().note.getTime();
                double minimum_key = score_attractor.front().note.getKey();
                double maximum_key = minimum_key;
                for (auto &event : score_attractor) {
                    if (minimum_time > event.note.getTime()) {
                        minimum_time = event.note.getTime();
                    }
                    if (minimum_key > event.note.getKey()) {
                        minimum_key = event.note.getKey();
                    }
                    if (maximum_key < event.note.getKey()) {
                        maximum_key = event.note.getKey();
                    }
                }
                double key_range = maximum_key - minimum_key;
                rescale = 1.;
                if (key_range != 0.) {
                    rescale = range / key_range;
                }
                for (auto &event : score_attractor) {
                    double time_ = event.note.getTime();
                    time_ = time_ - minimum_time;
                    event.note.setTime(time_);
                    double key = event.note.getKey();
                    key = key - minimum_key;
                    key = key * rescale;
                    key = key + minimum_key + bass;
                    event.note.setKey(key);   
                    event.note.temper(12.);
                    conformToChord(event.note, event.chord);
                    score.push_back(event.note);
                }
                System::inform("  Removing duplicate notes...\n");
                remove_duplicate_notes();
                System::inform("  Tieing overlapping notes...\n");
                System::inform("HarmonyIFS::tie overlapping notes: before: %d events...\n", score.size());
                score.tieOverlappingNotes();
                System::inform("                                    after: %d events.\n", score.size());
                score_attractor.clear();
                System::inform("  Finished translating score attractor to final score.\n");
            }
            /**
             * Returns the number of affine transformation matrices in the 
             * Hutchinson operattor of the function system that generates the 
             * score.
             */
            virtual int get_transformation_count() const {
                return hutchinson_operator.size();
            }
            /**
             * Sets the value of a single matrix element in one of the affine 
             * transformation matrices of the Hutchinson operator. The 
             * matrices are homeogenous transformations with 7 dimensions,
             * in column major order.
             */
            virtual void set_transformation(int transformation, int row, int column, double value) {
                hutchinson_operator[transformation](row, column) = value;
            } 
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
     
 }