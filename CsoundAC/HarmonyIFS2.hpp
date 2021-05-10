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
     * T    Transposition (or translation in pitch space modulo the octave).
     * V    Permutation of octavewise revoicings within range R.
     * 1    Homogeneity.
     * </pre>
     * At rendering time, the point will be translated to that pitch which 
     * most closely matches a pitch-class in that chord defined by P, I, 
     * and T.
     */     
    class SILENCE_PUBLIC HarmonyPoint2 : public Eigen::VectorXd {
         public:
            typedef enum
            {
                HP_TIME = 0,
                HP_PRIME_FORM,
                HP_INVERSION,
                HP_TRANSPOSITION,
                HP_VOICING,
                HP_HOMOGENEITY,
                HP_ELEMENT_COUNT
            } Dimensions;
            HarmonyPoint2() {
                initialize();
            };
            HarmonyPoint2(const HarmonyPoint2 &other) {
                *this = other;
            }
            HarmonyPoint2 &operator = (const HarmonyPoint2 &other) {
                Eigen::VectorXd::operator = (other);
                return *this;
            };
            HarmonyPoint2 &operator = (const Eigen::VectorXd &other) {
                Eigen::VectorXd::operator = (other);
                return *this;
            };
            virtual ~HarmonyPoint2() {};
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
            virtual double V() const {
                return (*this)[HP_VOICING];
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
            virtual void set_V(double value) {
                (*this)[HP_VOICING] = value;
            };
            virtual void set_homogeneity(double value) {
                (*this)[HP_HOMOGENEITY] = value;
            };
            virtual std::string toString() const {
                char buffer[0x1000];
                std::snprintf(buffer, 0x1000, "t: %9.4f P: %9.4f I: %9.4f T: %9.4f V: %9.4f1: %9.4f\n", 
                    t(), P(), I(), T(), V(), (*this)[HP_HOMOGENEITY]);
                return buffer;
            };
    };
     
    /**
     * Represents an interpolation point with scaling factors for a fractal 
     * interpolation function in the __time-harmony subspace__ of the score 
     * space.
     */     
    class SILENCE_PUBLIC HarmonyInterpolationPoint2 {
         public:
            double t;
            double P;
            double I;
            double T;
            double V;
            double s_PP;
            double s_PI;
            double s_PT;
            double s_PV;
            double s_IP;
            double s_II;
            double s_IT;
            double s_IV;
            double s_TP;
            double s_TI;
            double s_TT;
            double s_TV;
            double s_VP;
            double s_VI;
            double s_VT;
            double s_VV;
            HarmonyInterpolationPoint2() {
            }
            HarmonyInterpolationPoint2(const HarmonyInterpolationPoint2 &other) {
                *this = other;
            }
            HarmonyInterpolationPoint2(double t_, 
                                      double P_,    double I_,    double T_,    double V_,
                                      double s_PP_, double s_PI_, double s_PT_, double s_PV_,
                                      double s_IP_, double s_II_, double s_IT_, double s_IV_,
                                      double s_TP_, double s_TI_, double s_TT_, double s_TV_,
                                      double s_VP_, double s_VI_, double s_VT_, double s_VV_) {
                t = t_;
                P = P_;
                I = I_;
                T = T_;
                V = V_;
                s_PP = s_PP_;
                s_PI = s_PI_;
                s_PT = s_PT_;
                s_PV = s_PV_;
                s_IP = s_IP_;
                s_II = s_II_;
                s_IT = s_IT_;
                s_IV = s_IV_;
                s_TP = s_TP_;
                s_TI = s_TI_;
                s_TT = s_TT_;
                s_VP = s_VP_;
                s_VI = s_VI_;
                s_VT = s_VT_;
                s_VV = s_VV_;
            }
            virtual ~HarmonyInterpolationPoint2() {
            }
            virtual std::string toString() const {
                char buffer[0x1000];
                std::snprintf(buffer, 0x1000, "t:    %9.4f\n"
                                              "P:    %9.4f I:    %9.4f T:  %9.4f V:  %9.4f\n"
                                              "s_PP: %9.4f s_PI: %9.4f PT: %9.4f PV: %9.4f\n"
                                              "s_IP: %9.4f s_II: %9.4f IT: %9.4f IV: %9.4f\n"
                                              "s_TP: %9.4f s_TI: %9.4f TT: %9.4f TV: %9.4f\n"
                                              "s_VP: %9.4f s_VI: %9.4f VT: %9.4f VV: %9.4f\n",
                                              t,
                                              P,    I,    T,    V,
                                              s_PP, s_PI, s_PT, s_PV,
                                              s_IP, s_II, s_IT, s_IV,
                                              s_TP, s_TI, s_TT, s_TV,
                                              s_VP, s_VI, s_VT, s_VV);
                return buffer;
            }
    };
    
    SILENCE_PUBLIC inline bool interpolation_point_less2(const HarmonyInterpolationPoint2 &a, const HarmonyInterpolationPoint2 &b) {
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
    class SILENCE_PUBLIC HarmonyIFS2 : public ScoreNode {
        public:
            HarmonyIFS2() {
            }
            virtual ~HarmonyIFS2() {
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
            virtual HarmonyInterpolationPoint2 add_interpolation_point(double t,    
                                                                      double P,    double I,    double T,    double V,
                                                                      double s_PP, double s_PI, double s_PT, double s_PV, 
                                                                      double s_IP, double s_II, double s_IT, double s_IV, 
                                                                      double s_TP, double s_TI, double s_TT, double s_TV,
                                                                      double s_VP, double s_VI, double s_VT, double s_VV) {                 
                HarmonyInterpolationPoint2 harmony_interpolation_point = HarmonyInterpolationPoint2(
                    t,    
                    P,    I,    T,    V,
                    s_PP, s_PI, s_PT, s_PV,
                    s_IP, s_II, s_IT, s_IV, 
                    s_TP, s_TI, s_TT, s_TV,
                    s_VP, s_VI, s_VT, s_VV);
                System::message("HarmonyIfs::add_interpolation_point:\n%s\n", harmony_interpolation_point.toString().c_str());
                interpolation_points.push_back(harmony_interpolation_point);
                return harmony_interpolation_point;        
            }
            /**
             * Adds an interpolation point to the graph of the fractal interpolation function.
             */
            virtual HarmonyInterpolationPoint2 add_interpolation_point_as_chord(double t_, 
                                                                               const Chord &chord, 
                                                                               double s_PP_, double s_PI_, double s_PT_, double s_PV_,
                                                                               double s_IP_, double s_II_, double s_IT_, double s_IV_,
                                                                               double s_TP_, double s_TI_, double s_TT_, double s_TV_,
                                                                               double s_VP_, double s_VI_, double s_VT_, double s_VV_) {                 
                const auto &pit = pitv_.fromChord(chord);
                const auto P_ = pit[0];
                const auto I_ = pit[1];
                const auto T_ = pit[2];
                const auto V_ = pit[3];
                System::message("HarmonyInterpolationPoint2::add_interpolation_point_as_chord:\n%s\n", chord.toString().c_str());
                auto interpolation_point = add_interpolation_point(t_,    
                                                                   P_,    I_,    T_,    V_,
                                                                   s_PP_, s_PI_, s_PT_, s_PV_,
                                                                   s_IP_, s_II_, s_IT_, s_IV_, 
                                                                   s_TP_, s_TI_, s_TT_, s_TV_,
                                                                   s_VP_, s_VI_, s_VT_, s_VV_);
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
                std::sort(interpolation_points.begin(), interpolation_points.end(), interpolation_point_less2);
                HarmonyInterpolationPoint2 p_0 = interpolation_points.front();
                HarmonyInterpolationPoint2 p_N = interpolation_points.back();
                for (int n = 1, N = interpolation_points.size(); n < N; ++n) {
                    HarmonyInterpolationPoint2 p_n_1 = interpolation_points[n - 1];
                    HarmonyInterpolationPoint2 p_n = interpolation_points[n];
                    Eigen::MatrixXd transformation = Eigen::MatrixXd::Identity(HarmonyPoint2::HP_ELEMENT_COUNT, HarmonyPoint2::HP_ELEMENT_COUNT);
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
                        - (p_n.s_PT * ((p_N.T - p_0.T)   / (p_N.t - p_0.t)))
                        - (p_n.s_PV * ((p_N.V - p_0.V)   / (p_N.t - p_0.t)));
                    //   Prime-form column:
                    double P_sPP = p_n.s_PP;
                    //   Inversion column:
                    double P_sPI = p_n.s_PI;
                    //   Transposition column:
                    double P_sPT = p_n.s_PT;
                    //   Voicing column:
                    double P_sPV = p_n.s_PV;
                    //   Homogeneity or translation column:
                    double P_h =      (((p_N.t * p_n_1.P) - (p_0.t * p_n.P)) / (p_N.t - p_0.t)) 
                        - (p_n.s_PP * (((p_N.t * p_0.P)   - (p_0.t * p_N.P)) / (p_N.t - p_0.t)))
                        - (p_n.s_PI * (((p_N.t * p_0.I)   - (p_0.t * p_N.I)) / (p_N.t - p_0.t)))
                        - (p_n.s_PT * (((p_N.t * p_0.T)   - (p_0.t * p_N.T)) / (p_N.t - p_0.t)))
                        - (p_n.s_PV * (((p_N.t * p_0.V)   - (p_0.t * p_N.V)) / (p_N.t - p_0.t)));
                    // Inversion row:
                    //   Time column:
                    double I_t =      ((p_n.I - p_n_1.I) / (p_N.t - p_0.t))  
                        - (p_n.s_IP * ((p_N.P - p_0.P)   / (p_N.t - p_0.t)))
                        - (p_n.s_II * ((p_N.I - p_0.I)   / (p_N.t - p_0.t)))
                        - (p_n.s_IT * ((p_N.T - p_0.T)   / (p_N.t - p_0.t)))
                        - (p_n.s_IV * ((p_N.V - p_0.V)   / (p_N.t - p_0.t)));
                    //   Prime-form column:
                    double I_sIP = p_n.s_IP;
                    //   Inversion column:
                    double I_sII = p_n.s_II;
                    //   Transposition column:
                    double I_sIT = p_n.s_IT;
                    //   Voicing column:
                    double I_sIV = p_n.s_IV;
                    //   Homogeneity or translation column:
                    double I_h =      (((p_N.t * p_n_1.I) - (p_0.t * p_n.I)) / (p_N.t - p_0.t)) 
                        - (p_n.s_IP * (((p_N.t * p_0.P)   - (p_0.t * p_N.P)) / (p_N.t - p_0.t)))
                        - (p_n.s_II * (((p_N.t * p_0.I)   - (p_0.t * p_N.I)) / (p_N.t - p_0.t)))
                        - (p_n.s_IT * (((p_N.t * p_0.T)   - (p_0.t * p_N.T)) / (p_N.t - p_0.t)))
                        - (p_n.s_IV * (((p_N.t * p_0.V)   - (p_0.t * p_N.V)) / (p_N.t - p_0.t)));
                    // Transposition row:
                    //   Time column:
                    double T_t =      ((p_n.T - p_n_1.T) / (p_N.t - p_0.t)) 
                        - (p_n.s_TP * ((p_N.P - p_0.P)   / (p_N.t - p_0.t)))
                        - (p_n.s_TI * ((p_N.I - p_0.I)   / (p_N.t - p_0.t)))
                        - (p_n.s_TT * ((p_N.T - p_0.T)   / (p_N.t - p_0.t)))
                        - (p_n.s_TV * ((p_N.V - p_0.V)   / (p_N.t - p_0.t)));
                    //   Prime-form column:
                    double T_sTP = p_n.s_TP;
                    //   Inversion column:
                    double T_sTI = p_n.s_TI;
                    //   Transposition column:
                    double T_sTT = p_n.s_TT;
                    //   Voicing column:
                    double T_sTV = p_n.s_TV;
                    //   Homogeneity or translation column:
                    double T_h =      (((p_N.t * p_n_1.T) - (p_0.t * p_n.T)) / (p_N.t - p_0.t)) 
                        - (p_n.s_TP * (((p_N.t * p_0.P)   - (p_0.t * p_N.P)) / (p_N.t - p_0.t)))
                        - (p_n.s_TI * (((p_N.t * p_0.I)   - (p_0.t * p_N.I)) / (p_N.t - p_0.t)))
                        - (p_n.s_TT * (((p_N.t * p_0.T)   - (p_0.t * p_N.T)) / (p_N.t - p_0.t)))
                        - (p_n.s_TV * (((p_N.t * p_0.V)   - (p_0.t * p_N.V)) / (p_N.t - p_0.t)));
                    // Voicing row:
                    //   Time column:
                    double V_t =      ((p_n.T - p_n_1.T) / (p_N.t - p_0.t)) 
                        - (p_n.s_VP * ((p_N.P - p_0.P)   / (p_N.t - p_0.t)))
                        - (p_n.s_VI * ((p_N.I - p_0.I)   / (p_N.t - p_0.t)))
                        - (p_n.s_VT * ((p_N.T - p_0.T)   / (p_N.t - p_0.t)))
                        - (p_n.s_VV * ((p_N.V - p_0.V)   / (p_N.t - p_0.t)));
                    //   Prime-form column:
                    double V_sVP = p_n.s_VP;
                    //   Inversion column:
                    double V_sVI = p_n.s_VI;
                    //   Transposition column:
                    double V_sVT = p_n.s_VT;
                    //   Voicing column:
                    double V_sVV = p_n.s_VV;
                    //   Homogeneity or translation column:
                    double V_h =      (((p_N.t * p_n_1.T) - (p_0.t * p_n.T)) / (p_N.t - p_0.t)) 
                        - (p_n.s_VP * (((p_N.t * p_0.P)   - (p_0.t * p_N.P)) / (p_N.t - p_0.t)))
                        - (p_n.s_VI * (((p_N.t * p_0.I)   - (p_0.t * p_N.I)) / (p_N.t - p_0.t)))
                        - (p_n.s_VT * (((p_N.t * p_0.T)   - (p_0.t * p_N.T)) / (p_N.t - p_0.t)))
                        - (p_n.s_VV * (((p_N.t * p_0.V)   - (p_0.t * p_N.V)) / (p_N.t - p_0.t)));
                                      // Time row.
                                      // t, P,     I,     T,     k, v, i, translation.
                                      // t, P,     I,     T,     V,     translation.
                    transformation << t_t,  0,     0,     0,     0,     t_h,
                                      // Prime-form row.
                                      P_t,  P_sPP, P_sPI, P_sPT, P_sPV, P_h,
                                      // Inversion row.
                                      I_t,  I_sIP, I_sII, I_sIT, I_sIV, I_h,
                                      // Transposition row.
                                      T_t,  T_sTP, T_sTI, T_sTT, T_sTV, T_h,
                                      // Voicing row.
                                      V_t,  V_sVP, V_sVI, V_sVT, V_sVV, V_h,
                                      // Homogeneity row.
                                      0,    0,     0,     0,     0,     1;     
                    auto scaling_matrix = transformation.block<4, 4>(1, 1); 
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
             * Removes duplicate notes from the generated score.
             */
            virtual void remove_duplicate_notes() {
                System::inform("HarmonyIFS::remove_duplicate_notes: before: %d events...\n", score.size());
                std::map<std::string, Event> unique_events;
                for (auto &event : score) {
                    unique_events[event.toString()] = event;
                }
                score.clear();
                for (auto &event : unique_events) {
                    score.push_back(event.second);
                }
                System::inform("                                    after:  %d events.\n", score.size());
            }
            /**
             * Notes in the generated chords have a nominal duration. Notes 
             * on the same voice and key that overlap are tied (merged).
             */
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
                score.clear();
                int iteration = 0;
                HarmonyPoint2 initial_point;
                initial_point.set_homogeneity(1);
                System::inform("HarmonyIFS::generate_score_attractor: initial point:\n%s\n", toString(initial_point).c_str());
                for (int i = 0, n = hutchinson_operator.size(); i < n; ++i) {
                    System::inform("HarmonyIFS::generate_score_attractor: transformation[%3d]\n", i + 1);
                    std::cerr << hutchinson_operator[i] << std::endl << std::endl;
                }
                iterate(depth, iteration, 0, initial_point);
                System::inform("points: %d.\n", score.size());
                post_process_score();
            }
            /**
             * Actually computes the score attractor.
             */
            virtual void iterate(int depth, int iteration, int index, const HarmonyPoint2 point) {
                iteration = iteration + 1;
                if (iteration > depth) {
                    return;
                }
                for (int i = 0, n = hutchinson_operator.size(); i < n; ++i) {
                    const Eigen::MatrixXd &T = hutchinson_operator[i];
                    HarmonyPoint2 new_point = point;
                    new_point = T * new_point;
                    if (iteration == depth) {
                        double tyme = new_point.t();
                        int P = std::floor(new_point.P());
                        int I = std::floor(new_point.I());
                        int T = std::floor(new_point.T());
                        int V= std::floor(new_point.V());
                        Chord chord = pitv_.toChord(P, I, T, V)[2];
                        System::inform("HarmonyIFS::iterate: depth: %2d index: [%2d] iteration: %9d time: %9.4f chord:\n%s\n", depth, index, iteration, tyme, chord.toString().c_str());
                        toScore(chord, score, tyme, true);
                    }
                    iterate(depth, iteration, i, new_point);
                }
            }
            /**
             * Processes the score attractor (the raw notes in the score) to 
             * quantize and rescale certain dimensions, and to remove duplicate 
             * notes.
             */
            virtual void post_process_score() {
                System::inform("HarmonyIFS::post_process_score...\n");
                score.rescale(Event::TIME, true, 0., false, 0);                
                score.rescale(Event::DURATION, true, note_duration, true, 0.);                
                score.rescale(Event::KEY, true, bass, false, 0.);                
                if (tie_overlaps == true) {
                    tie_overlapping_notes();
                }
                if (remove_duplicates == true) {
                    remove_duplicate_notes();
                }
                 System::inform("  Finished post-processing score score.\n");
            }
            /**
             * Adds a new affine transformation matrix to the Hutchinson operator.
             * The value of this matrix is initially the identity matrix.
             */
            virtual Eigen::MatrixXd &add_transformation() {
                Eigen::MatrixXd transformation = Eigen::MatrixXd::Identity(HarmonyPoint2::HP_ELEMENT_COUNT, 
                    HarmonyPoint2::HP_ELEMENT_COUNT);
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
                hutchinson_operator[transformation](HarmonyPoint2::HP_TIME, dimension) = value;
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
            std::vector<HarmonyInterpolationPoint2> interpolation_points;
            std::vector<Eigen::MatrixXd> hutchinson_operator;
        };
 };