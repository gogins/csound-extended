#ifndef HARMONYIFS_HPP_INCLUDED
#define HARMONYIFS_HPP_INCLUDED
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
#include <Eigen/Dense>
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
#include <Eigen/Dense>
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
            virtual double t() const {
                return (*this)[HP_TIME];
            };
            virtual double P() const {
                return (*this)[HP_PRIME_FORM];
            };
            virtual double I() const {
                return (*this)[HP_INVERSION];
            };
            virtual double T() const {
                return (*this)[HP_TRANSPOSITION];
            };
            virtual double k() const {
                return (*this)[HP_MIDI_KEY];
            };
            virtual double v() const {
                return (*this)[HP_MIDI_VELOCITY];
            };
            virtual double i() const {
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
            virtual void set_homogeneity(double value) {
                (*this)[HP_HOMOGENEITY] = value;
            };
            virtual std::string toString() const {
                char buffer[0x1000];
                std::snprintf(buffer, 0x1000, "t: %9.4f P: %9.4f I: %9.4f T: %9.4f k: %9.4f v: %9.4f i: %9.4f 1: %9.4f\n", 
                    t(), P(), I(), T(), k(), v(), i(), (*this)[HP_HOMOGENEITY]);
                return buffer;
            };
    };
     
    /**
     * Represents an interpolation point with scaling factors for a fractal 
     * interpolation function in the __time-harmony subspace__ of the score 
     * space.
     */     
    class SILENCE_PUBLIC HarmonyInterpolationPoint {
         public:
            double t;
            double P;
            double I;
            double T;
            double s_PP;
            double s_PI;
            double s_PT;
            double s_IP;
            double s_II;
            double s_IT;
            double s_TP;
            double s_TI;
            double s_TT;
            HarmonyInterpolationPoint() {
            }
            HarmonyInterpolationPoint(const HarmonyInterpolationPoint &other) {
                *this = other;
            }
            HarmonyInterpolationPoint(double t_, 
                                      double P_,    double I_,    double T_, 
                                      double s_PP_, double s_PI_, double s_PT_,
                                      double s_IP_, double s_II_, double s_IT_,
                                      double s_TP_, double s_TI_, double s_TT_) {
                t = t_;
                P = P_;
                I = I_;
                T = T_;
                s_PP = s_PP_;
                s_PI = s_PI_;
                s_PT = s_PT_;
                s_IP = s_IP_;
                s_II = s_II_;
                s_IT = s_IT_;
                s_TP = s_TP_;
                s_TI = s_TI_;
                s_TT = s_TT_;
            }
            virtual ~HarmonyInterpolationPoint() {
            }
            virtual std::string toString() const {
                char buffer[0x1000];
                std::snprintf(buffer, 0x1000, "t:    %9.4f\n"
                                              "P:    %9.4f I:    %9.4f T:  %9.4f\n"
                                              "s_PP: %9.4f s_PI: %9.4f PT: %9.4f\n"
                                              "s_IP: %9.4f s_II: %9.4f IT: %9.4f\n"
                                              "s_TP: %9.4f s_TI: %9.4f TT: %9.4f\n",
                                              t,
                                              P, I, T,
                                              s_PT, s_PI, s_PT,
                                              s_IT, s_II, s_IT,
                                              s_TT, s_TI, s_TT);
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
        if (a.t < b.t) {
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
            virtual PITV &pitv() {
                return pitv_;
            }
            /**
             * Initialize the HarmonyIFS for N voices in a range of MIDI keys 
             * for a note duration in seconds. g is the generator of 
             * transposition.
             */
            virtual void initialize(int voices_, double range_, double bass_, double note_duration_, bool tie_overlapping_notes, bool remove_duplicate_notes, double g_ = 1.) {
                voices = voices_;
                range = range_;
                bass = bass_;
                note_duration = note_duration_;
                tie_overlaps = tie_overlapping_notes;
                remove_duplicates = remove_duplicate_notes;
                g = g_;
                pitv_.initialize(voices_, range_, g_, true);
                interpolation_points.clear();
            }
            /**
             * Adds an interpolation point to the graph of the fractal interpolation function.
             */
            virtual HarmonyInterpolationPoint add_interpolation_point(double t,    
                                                                      double P,    double I,    double T, 
                                                                      double s_PP, double s_PI, double s_PT, 
                                                                      double s_IP, double s_II, double s_IT, 
                                                                      double s_TP, double s_TI, double s_TT) {                 
                HarmonyInterpolationPoint harmony_interpolation_point = HarmonyInterpolationPoint(
                    t,    
                    P,    I,    T, 
                    s_PP, s_PI, s_PT, 
                    s_IP, s_II, s_IT, 
                    s_TP, s_TI, s_TT);
                System::message("HarmonyIfs::add_interpolation_point:\n%s\n", harmony_interpolation_point.toString().c_str());
                interpolation_points.push_back(harmony_interpolation_point);
                return harmony_interpolation_point;        
            }
            /**
             * Adds an interpolation point to the graph of the fractal interpolation function.
             */
            virtual HarmonyInterpolationPoint add_interpolation_point_as_chord(double t_, 
                                                                               const Chord &chord, 
                                                                               double s_PP_, double s_PI_, double s_PT_, 
                                                                               double s_IP_, double s_II_, double s_IT_, 
                                                                               double s_TP_, double s_TI_, double s_TT_) {                 
                const auto &pit = pitv_.fromChord(chord);
                const auto P_ = pit[0];
                const auto I_ = pit[1];
                const auto T_ = pit[2];
                System::message("HarmonyInterpolationPoint::add_interpolation_point_as_chord:\n%s\n", chord.toString().c_str());
                auto interpolation_point = add_interpolation_point(t_,    
                                                                   P_,    I_,    T_, 
                                                                   s_PP_, s_PI_, s_PT_, 
                                                                   s_IP_, s_II_, s_IT_, 
                                                                   s_TP_, s_TI_, s_TT_);
                return interpolation_point;
            }
            /**
             * Interpolation points are sorted by time and the corresponding 
             * shear transformations for a Hutchinson operator are computed, 
             * according to Polychronis Manousopoulos, Vasileios Drakopoulos, 
             * and Theoharis Theoharis, "Curve Fitting by Fractal 
             * Interpolation." In: Transactions on Computational
             * Science 1 (Jan. 2008), pp. 85-103. 
             * doi: 10.1007/978-3-540-79299-4_4. 
             * 
             * Once this function has been called, the non-shear elements of 
             * the transformation matrices may be modified. A warning is 
             * issued if the modulus of the scaling submatrix of any 
             * transformation is >= 0, indicating it is not contractive.
             */
            virtual void initialize_hutchinson_operator() {
                hutchinson_operator.clear();
                std::sort(interpolation_points.begin(), interpolation_points.end(), interpolation_point_less);
                HarmonyInterpolationPoint p_0 = interpolation_points.front();
                HarmonyInterpolationPoint p_N = interpolation_points.back();
                for (int n = 1, N = interpolation_points.size(); n < N; ++n) {
                    HarmonyInterpolationPoint p_n_1 = interpolation_points[n - 1];
                    HarmonyInterpolationPoint p_n = interpolation_points[n];
                    Eigen::MatrixXd transformation = Eigen::MatrixXd::Identity(HarmonyPoint::HP_ELEMENT_COUNT, HarmonyPoint::HP_ELEMENT_COUNT);
                    // Time row:
                    //   Time column:
                    double t_t =       (p_n.t - p_n_1.t) / (p_N.t - p_0.t);
                    //   Homogeneity or translation column:
                    double t_h =      ((p_N.t * p_n_1.t) - (p_0.t * p_n.t)) / (p_N.t - p_0.t);
                    // Prime-form row:
                    //   Time column:
                    double P_t =      ((p_n.P - p_n_1.P) / (p_N.t - p_0.t)) 
                        - (p_n.s_PP * ((p_N.P - p_0.P)   / (p_N.t - p_0.t)))
                        - (p_n.s_PI * ((p_N.I - p_0.I)   / (p_N.t - p_0.t)))
                        - (p_n.s_PT * ((p_N.T - p_0.T)   / (p_N.t - p_0.t)));
                    //   Prime-form column:
                    double P_sPP = p_n.s_PP;
                    //   Inversion column:
                    double P_sPI = p_n.s_PI;
                    //   Transposition column:
                    double P_sPT = p_n.s_PT;
                    //   Homogeneity or translation column:
                    double P_h =      (((p_N.t * p_n_1.P) - (p_0.t * p_n.P)) / (p_N.t - p_0.t)) 
                        - (p_n.s_PP * (((p_N.t * p_0.P)   - (p_0.t * p_N.P)) / (p_N.t - p_0.t)))
                        - (p_n.s_PI * (((p_N.t * p_0.I)   - (p_0.t * p_N.I)) / (p_N.t - p_0.t)))
                        - (p_n.s_PT * (((p_N.t * p_0.T)   - (p_0.t * p_N.T)) / (p_N.t - p_0.t)));
                    // Inversion row:
                    //   Time column:
                    double I_t =      ((p_n.I - p_n_1.I) / (p_N.t - p_0.t))  
                        - (p_n.s_IP * ((p_N.P - p_0.P)   / (p_N.t - p_0.t)))
                        - (p_n.s_II * ((p_N.I - p_0.I)   / (p_N.t - p_0.t)))
                        - (p_n.s_IT * ((p_N.T - p_0.T)   / (p_N.t - p_0.t)));
                    //   Prime-form column:
                    double I_sIP = p_n.s_IP;
                    //   Inversion column:
                    double I_sII = p_n.s_II;
                    //   Transposition column:
                    double I_sIT = p_n.s_IT;
                    //   Homogeneity or translation column:
                    double I_h =      (((p_N.t * p_n_1.I) - (p_0.t * p_n.I)) / (p_N.t - p_0.t)) 
                        - (p_n.s_IP * (((p_N.t * p_0.P)   - (p_0.t * p_N.P)) / (p_N.t - p_0.t)))
                        - (p_n.s_II * (((p_N.t * p_0.I)   - (p_0.t * p_N.I)) / (p_N.t - p_0.t)))
                        - (p_n.s_IT * (((p_N.t * p_0.T)   - (p_0.t * p_N.T)) / (p_N.t - p_0.t)));
                    // Transposition row:
                    //   Time column:
                    double T_t =      ((p_n.T - p_n_1.T) / (p_N.t - p_0.t)) 
                        - (p_n.s_TP * ((p_N.P - p_0.P)   / (p_N.t - p_0.t)))
                        - (p_n.s_TI * ((p_N.I - p_0.I)   / (p_N.t - p_0.t)))
                        - (p_n.s_TT * ((p_N.T - p_0.T)   / (p_N.t - p_0.t)));
                    //   Prime-form column:
                    double T_sTP = p_n.s_TP;
                    //   Inversion column:
                    double T_sTI = p_n.s_TI;
                    //   Transposition column:
                    double T_sTT = p_n.s_TT;
                    //   Homogeneity or translation column:
                    double T_h =      (((p_N.t * p_n_1.T) - (p_0.t * p_n.T)) / (p_N.t - p_0.t)) 
                        - (p_n.s_TP * (((p_N.t * p_0.P)   - (p_0.t * p_N.P)) / (p_N.t - p_0.t)))
                        - (p_n.s_TI * (((p_N.t * p_0.I)   - (p_0.t * p_N.I)) / (p_N.t - p_0.t)))
                        - (p_n.s_TT * (((p_N.t * p_0.T)   - (p_0.t * p_N.T)) / (p_N.t - p_0.t)));
                                      // Time row.
                                      // t, P,     I,     T,     k, v, i, translation.
                    transformation << t_t,  0,     0,     0,     0, 0, 0, t_h,
                                      // Prime-form row.
                                      P_t,  P_sPP, P_sPI, P_sPT, 0, 0, 0, P_h,
                                      // Inversion row.
                                      I_t,  I_sIP, I_sII, I_sIT, 0, 0, 0, I_h,
                                      // Transposition row.
                                      T_t,  T_sTP, T_sTI, T_sTT, 0, 0, 0, T_h,
                                      // Key row.
                                      0,    0,     0,     0,     0, 0, 0, 0,   
                                      // Velocity row.
                                      0,    0,     0,     0,     0, 0, 0, 0,   
                                      // Instrument row.
                                      0,    0,     0,     0,     0, 0, 0, 0,
                                      // Homogeneity row.
                                      0,    0,     0,     0,     0, 0, 0, 1;     
                    auto scaling_matrix = transformation.block<3, 3>(1, 1); 
                    auto modulus = std::abs(scaling_matrix.determinant());
                    System::inform("HarmonyIFS::initialize_hutchinson_operator: modulus of contraction: %9.4f\n", modulus);
                    if (ge_tolerance(modulus, 1.) == true) {
                        System::warn("HarmonyIFS::initialize_hutchinson_operator: Warning!\n"
                            "This transformation is not contractive.\n");
                    }
                    hutchinson_operator.push_back(transformation);
                }
            }
            /** 
             * Translates a point in the attractor of the IFS to a note and 
             * associated chord.
             */
            virtual HarmonyEvent point_to_note(const HarmonyPoint &point) {
                HarmonyEvent event;
                event.note.setTime(point.t());
                event.note.setDuration(note_duration);
                event.note.setStatus(144);
                event.note.setInstrument(point.i());
                int P = std::round(point.P());
                int I = std::round(point.I());
                int T = std::round(point.T());
                event.chord = pitv_.toChord(P, I, T, 0)[2];
                event.note.setKey(point.k());
                event.note.setVelocity(point.v());
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
            virtual void tie_overlapping_notes() {
                System::inform("HarmonyIFS::tie overlapping notes:  before: %d events...\n", score.size());
                score.tieOverlappingNotes(true);
                System::inform("                                    after:  %d events.\n", score.size());
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
                initial_point.set_k(range/2.);
                initial_point.set_v(range/2.);
                initial_point.set_i(1.);
                initial_point.set_homogeneity(1);
                System::inform("HarmonyIFS::generate_score_attractor: initial point:\n%s\n", toString(initial_point).c_str());
                for (int i = 0, n = hutchinson_operator.size(); i < n; ++i) {
                    System::inform("HarmonyIFS::generate_score_attractor: transformation[%3d]\n", i + 1);
                    std::cerr << hutchinson_operator[i] << std::endl << std::endl;
                }
                iterate(depth, iteration, 0, initial_point);
                System::inform("points: %d.\n", score_attractor.size());
                translate_score_attractor_to_score();
            }
            /**
             * Actually computes the score attractor.
             */
            virtual void iterate(int depth, int iteration, int index, const HarmonyPoint point) {
                iteration = iteration + 1;
                if (iteration > depth) {
                    return;
                }
                for (int i = 0, n = hutchinson_operator.size(); i < n; ++i) {
                    const Eigen::MatrixXd &T = hutchinson_operator[i];
                    HarmonyPoint new_point = point;
                    new_point = T * new_point;
                    if (iteration == depth) {
                        HarmonyEvent event = point_to_note(new_point);
                        System::inform("HarmonyIFS::iterate: depth: %2d index: [%2d] iteration: %9d point:\n%s\n", depth, index, iteration, toString(new_point).c_str());
                        score_attractor.push_back(event);
                    }
                    iterate(depth, iteration, i, new_point);
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
                for (const auto &event : score_attractor) {
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
                double actual_key_range = maximum_key - minimum_key;
                double scale_factor = 1.;
                if (actual_key_range != 0.) {
                    scale_factor = range / actual_key_range;
                }
                System::inform("HarmonyIFS::translate:\n");
                System::inform("  minimum key: %9.4f\n", minimum_key);
                System::inform("  maximum key: %9.4f\n", maximum_key);
                System::inform("  key range:   %9.4f\n", actual_key_range);
                System::inform("  rescale by:  %9.4f\n", scale_factor);
                for (auto &event : score_attractor) {
                    System::inform("  pre-translation:  %s\n", event.note.toString().c_str());
                    double time_ = event.note.getTime();
                    time_ = time_ - minimum_time;
                    event.note.setTime(time_);
                    double key = event.note.getKey();
                    key = key - minimum_key;
                    key = key * scale_factor;
                    key = key + minimum_key + bass;
                    event.note.setKey(key);   
                    event.note.temper(12.);
                    conformToChord(event.note, event.chord);
                    System::inform("  post-translation: %s\n\n", event.note.toString().c_str());
                    score.push_back(event.note);
                }
                if (tie_overlaps == true) {
                    tie_overlapping_notes();
                }
                if (remove_duplicates == true) {
                    remove_duplicate_notes();
                }
                score_attractor.clear();
                System::inform("  Finished translating score attractor to final score.\n");
            }
            /**
             * Adds a new affine transformation matrix to the Hutchinson operator.
             * The value of this matrix is initially the identity matrix.
             */
            virtual Eigen::MatrixXd &add_transformation() {
                Eigen::MatrixXd transformation = Eigen::MatrixXd::Identity(HarmonyPoint::HP_ELEMENT_COUNT, 
                    HarmonyPoint::HP_ELEMENT_COUNT);
                hutchinson_operator.push_back(transformation);
                return hutchinson_operator.back();
            };
            /**
             * Returns the number of affine transformation matrices in the 
             * Hutchinson operator of the function system that generates the 
             * score.
             */
            virtual int transformation_count() const {
                return hutchinson_operator.size();
            }
            /**
             * Creates a scaling transformation in one of the affine 
             * transformation matrices of the Hutchinson operator.
             */
            virtual void set_scaling(int transformation, int dimension, double value) {
                hutchinson_operator[transformation](dimension, dimension) = value;
            }
            /**
             * Creates a shear transformation parallel to one non-time axis in one of 
             * the affine transformation matrices of the Hutchinson operator.
             */
            virtual void set_shear(int transformation, int dimension, double value) {
                hutchinson_operator[transformation](HarmonyPoint::HP_TIME, dimension) = value;
            }
            /**
             * Creates a translation transformation in one of the affine 
             * affine transformation matrices of the Hutchinson operator.
             */
            virtual void set_translation(int transformation, int dimension, double value) {
                hutchinson_operator[transformation](dimension, 8) = value;
            }
            /**
             * Creates a rotation in one plane in one of the affine 
             * affine transformation matrices of the Hutchinson operator.
             */
            virtual void set_rotation(int transformation, int dimension1, int dimension2, double degrees) {
                auto radians = degrees * M_PI / 180.;
                hutchinson_operator[transformation](dimension1,dimension1) =  std::cos(radians);
                hutchinson_operator[transformation](dimension1,dimension2) = -std::sin(radians);
                hutchinson_operator[transformation](dimension2,dimension1) =  std::sin(radians);
                hutchinson_operator[transformation](dimension2,dimension2) =  std::cos(radians);
           }
            /**
             * Sets the value of a single matrix element in one of the affine 
             * transformation matrices of the Hutchinson operator. The 
             * matrices are homeogenous transformations with 7 dimensions,
             * in column major order. The transformation is by default the 
             * identity matrix.
             */
            virtual void set_transformation(int transformation, int row, int column, double value) {
                hutchinson_operator[transformation](row, column) = value;
            } 
            int voices;
            double range;
            double bass;
            double note_duration;
            bool tie_overlaps;
            bool remove_duplicates;
            double g;
            PITV pitv_;
            std::vector<HarmonyInterpolationPoint> interpolation_points;
            std::vector<Eigen::MatrixXd> hutchinson_operator;
            std::vector<HarmonyEvent> score_attractor;
     };
     
 }
 
 #endif