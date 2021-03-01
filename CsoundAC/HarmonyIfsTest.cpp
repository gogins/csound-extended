#include <Composition.hpp>
#include <MCRM.hpp>
#include <cmath>
#include <eigen3/Eigen/Dense>
#include <functional>
#include <memory>
#include <MusicModel.hpp>
#include <random>
#include <ScoreNode.hpp>
#include <VoiceleadingNode.hpp>
#include <vector>

int main(int argc, const char **argv)
{
    csound::MusicModel model;
    model.setAuthor("Michael Gogins");
    model.setTitle("HarmonyIfsTest");
    model.setAlbum("Silence");
    model.setYear("2021");
    model.setPerformanceRightsOrganization("Irreducible Productions, ASCAP");
    csound::System::setMessageLevel(7);
    csound::HarmonyIFS harmony_ifs;
    // initialize(int voices_, double range_, double bass_, double note_duration_, bool tie_overlaps_, bool remove_duplicates, double g_ = 1.) {
    harmony_ifs.initialize(4 , 60., 30., .125, true, true, 1.);
    auto tonic = csound::chordForName("CM7");
    auto subdominant = csound::chordForName("Dm7");
    auto dominant = csound::chordForName("G7");
    csound::System::message("I:\n%s\n", tonic.information_sector(0).c_str());
    csound::System::message("ii:\n%s\n", subdominant.information_sector(0).c_str());
    csound::System::message("V:\n%s\n", dominant.information_sector(0).c_str());
    ///auto &score = harmony_ifs.getScore();
    harmony_ifs.add_interpolation_point_as_chord(  0., tonic,       .01, .01, .01, .01, .01, .01, .01, .01, .01);
    harmony_ifs.add_interpolation_point_as_chord(100., subdominant, .01, .01, .01, .01, .01, .01, .01, .01, .01);
    harmony_ifs.add_interpolation_point_as_chord(220., dominant,    .01, .01, .01, .01, .01, .01, .01, .01, .01);
    harmony_ifs.add_interpolation_point_as_chord(300., tonic,       .01, .01, .01, .01, .01, .01, .01, .01, .01);
    harmony_ifs.initialize_hutchinson_operator();
    double A = 5.13 * M_PI / 180.;
    csound::System::message("A: %9.4f\n", A);
    int t = 0;
    int k = 4;
    int v = 5;
    int i = 6;
    int h = 7;
    harmony_ifs.set_transformation(0, t, t,   .5);
    harmony_ifs.set_transformation(0, t, h, -1.0);
    harmony_ifs.set_transformation(0, k, k,   .5);
    harmony_ifs.set_transformation(0, k, h,   .0);
    
    harmony_ifs.set_transformation(1, t, t,   .5);
    harmony_ifs.set_transformation(1, t, h,   .0);
    harmony_ifs.set_transformation(1, k, k,   .5);
    harmony_ifs.set_transformation(1, k, h,  1.0);
    
    harmony_ifs.set_transformation(2, t, t,   .5);
    harmony_ifs.set_transformation(2, t, h,  1.0);
    harmony_ifs.set_transformation(2, k, k,   .5);
    harmony_ifs.set_transformation(2, k, h,   .0);
    
    harmony_ifs.generate_score_attractor(7);
       
    //~ auto &t1 = harmony_ifs.add_transformation();
    //~ //t1(t, t) = .5;
    //~ //t1(t, h) = -1.;
    //~ //t1(k, k) = .5;
    //~ auto &t2 = harmony_ifs.add_transformation();
    //~ //t2(t, t) = .5;
    //~ //t2(k, k) = .5;
    //~ //t2(k, h) = 1;
    //~ auto &t3 = harmony_ifs.add_transformation();
    //~ //t3(t, t) = .5;
    //~ //t3(t, h) = 1.;
    //~ //t3(k, k) = .5;
    csound::Rescale rescale;
    rescale.setRescale(csound::Event::INSTRUMENT, true, true, 1., 2.999);
    rescale.setRescale(csound::Event::VELOCITY, true, true, 60., 6.);
    rescale.addChild(&harmony_ifs);
    model.addChild(&rescale);
    model.setDuration(180.);
    const char orc[] = R"(
    
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 6

; Ensure the same random stream for each rendering.
; rand, randh, randi, rnd(x) and birnd(x) are not affected by seed.

seed 574382

;gi_Protoverb vstinit "/home/mkg/.u-he/Protoverb/Protoverb.64.avx.so", 1
gi_Mverb2020 vstinit "/home/mkg/.local/lib/Mverb2020.so", 1
gi_ReverbDragonfly vstinit "/home/mkg/.local/lib/DragonflyHallReverb-vst.so", 1
gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 1

gi_Organteq vstinit "/home/mkg/Organteq\ 1/x86-64bit/Organteq\ 1.lv2/Organteq_1.so", 0

alwayson "OrganOutOrganteq"
alwayson "PianoOutPianoteq"
; alwayson "ReverbSC"
alwayson "Mverb2020"
; alwayson "ReverbDragonfly"
; alwayson "MVerb"
; alwayson "ReverbBabo"
; alwayson "NReverb"
alwayson "MasterOutput"

connect "PianoOutPianoteq", "outleft", "Mverb2020", "inleft"
connect "PianoOutPianoteq", "outright", "Mverb2020", "inright"
connect "OrganOutOrganteq", "outleft", "Mverb2020", "inleft"
connect "OrganOutOrganteq", "outright", "Mverb2020", "inright"
connect "Mverb2020", "outleft", "MasterOutput", "inleft"
connect "Mverb2020", "outright", "MasterOutput", "inright"


gk_OrganNoteOrganteq_midi_dynamic_range init 127
; These are the pedalboard and three manuals.
instr Pedale, Positif, Grand_Orgue, Recit
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_midi_channel = i_instrument - 1
i_time = p2
i_duration = p3 
p3 = i_duration
i_midi_key = p4
i_midi_dynamic_range = i(gk_OrganNoteOrganteq_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1
i_homogeneity = p11
instances active p1
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
vstnote gi_Organteq, i_midi_channel, i_midi_key, i_midi_velocity, i_duration
endin

gk_PianoNotePianoteq_midi_dynamic_range init 127
instr 11,12,13,14
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_PianoNotePianoteq_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_homogeneity = p11
instances active p1
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\n", i_pitch_correction
vstnote gi_Pianoteq, 0, i_midi_key, i_midi_velocity, i_duration
endin

// This must be initialized in the orc header before any #includes.

gk_PianoOutPianoteq_level init 0
gi_PianoOutPianoteq_print init 1
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 0.5
gk_PianoOutPianoteq_bottom_to_top init 0

instr PianoOutPianoteq
; Should be "D4 Daily Practice".
vstprogset gi_Pianoteq, 0
; Sustain off.
vstparamset gi_Pianoteq, 0, 0
; Reverb off.
vstparamset gi_Pianoteq, 72, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 89
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_PianoOutPianoteq_print == 1 then
  vstinfo gi_PianoOutPianoteq_print
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Pianoteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_PianoOutPianoteq_left_to_right
; printks "vstaudio:       %9.4f   %9.4f\n", 0.5, aoutleft, aoutright
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_PianoOutPianoteq_front_to_back, gk_PianoOutPianoteq_left_to_right, gk_PianoOutPianoteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
; printks "PianoOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Piano_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin


gk_OrganOutOrganteq_level init 0
gi_OrganOutOrganteq_print init 1
gk_OrganOutOrganteq_front_to_back init 0
gk_OrganOutOrganteq_left_to_right init 0.5
gk_OrganOutOrganteq_bottom_to_top init 0
instr OrganOutOrganteq
; By default, Organteq .fxp  preset files are saved thus:
; /home/mkg/.local/share/Modartt/Organteq/Presets/My Presets/Church (copy).fxp
; However, vst4cs doesn't load .fxp files, only .fxb preset bank files.
; However again, Organtec doesn't save .fxb files, only .fxp files.
; The vst4cs program change opcode does not seem to work except with loaded .fxb files.
; vstprogset gi_Organteq, 3
; vstmidiout gi_Organteq, 192, 5, 15, 0
; vstmidiout gi_Organteq, 192, 6, 15, 0
; So, the only thing to do is to set each parameter right here. Unfortunately,
; not all the parameters in the GUI are available from code.
; Reverb control.
vstparamset gi_Organteq, 4, 0
; Tutti (test), it works.
; vstparamset gi_Organteq, 6, 1

; Set up all stops...

; Keyboard 1 -- Pedale

vstparamset gi_Organteq, 33, 1
vstparamset gi_Organteq, 34, 0
vstparamset gi_Organteq, 35, 0
vstparamset gi_Organteq, 36, 0
vstparamset gi_Organteq, 37, 1
vstparamset gi_Organteq, 38, 0
vstparamset gi_Organteq, 39, 0
vstparamset gi_Organteq, 40, 0
vstparamset gi_Organteq, 41, 0
vstparamset gi_Organteq, 42, 0

; Keyboard 2 -- Positif

vstparamset gi_Organteq, 43, 0
vstparamset gi_Organteq, 44, 1
vstparamset gi_Organteq, 45, 0
vstparamset gi_Organteq, 46, 0
vstparamset gi_Organteq, 47, 1
vstparamset gi_Organteq, 48, 0
vstparamset gi_Organteq, 49, 0
vstparamset gi_Organteq, 50, 0
vstparamset gi_Organteq, 51, 1
vstparamset gi_Organteq, 52, 0

; Keyboard 3 -- Grand Orgue

vstparamset gi_Organteq, 53, 0
vstparamset gi_Organteq, 54, 1
vstparamset gi_Organteq, 55, 1
vstparamset gi_Organteq, 56, 0
vstparamset gi_Organteq, 57, 0 
vstparamset gi_Organteq, 58, 0
vstparamset gi_Organteq, 59, 0
vstparamset gi_Organteq, 60, 0
vstparamset gi_Organteq, 61, 0
vstparamset gi_Organteq, 62, 0

; Keyboard 4 - Recit 

vstparamset gi_Organteq, 63, 1
vstparamset gi_Organteq, 64, 1
vstparamset gi_Organteq, 65, 0
vstparamset gi_Organteq, 66, 0
vstparamset gi_Organteq, 67, 0
vstparamset gi_Organteq, 68, 0
vstparamset gi_Organteq, 69, 0
vstparamset gi_Organteq, 70, 1
vstparamset gi_Organteq, 71, 0
vstparamset gi_Organteq, 72, 0

k_gain = ampdb(gk_OrganOutOrganteq_level)
i_overall_amps = 89
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_OrganOutOrganteq_print == 1 then
  vstinfo gi_Organteq
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Organteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_OrganOutOrganteq_left_to_right
; printks "vstaudio:       %9.4f   %9.4f\\n", 0.5, aoutleft, aoutright
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_OrganOutOrganteq_front_to_back, gk_OrganOutOrganteq_left_to_right, gk_OrganOutOrganteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
; printks "OrganOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Organ_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Mverb2020_level init 0
gk_Mverb2020_Mix init .5
gk_Mverb2020_Pre_delay init 0.5
gk_Mverb2020_Early_late_mix init 0.5
gk_Mverb2020_Size init 0.5
gk_Mverb2020_Density init 0.5
gk_Mverb2020_Bandwith_Frequency init 0.5
gk_Mverb2020_Decay init 0.85
gk_Mverb2020_Damping_Frequency init 0.5
gk_Mverb2020_Gain init 1
gi_Mverb2020_Program init 4
instr Mverb2020
vstprogset gi_Mverb2020, gi_Mverb2020_Program
vstparamset gi_Mverb2020, 1, gk_Mverb2020_Mix
;vstparamset gi_Mverb2020, 1, gk_Mverb2020_Pre_delay
;vstparamset gi_Mverb2020, 2, gk_Mverb2020_Early_late_mix
;vstparamset gi_Mverb2020, 3, gk_Mverb2020_Size
;vstparamset gi_Mverb2020, 4, gk_Mverb2020_Density
;vstparamset gi_Mverb2020, 5, gk_Mverb2020_Bandwith_Frequency
vstparamset gi_Mverb2020, 6, gk_Mverb2020_Decay
;vstparamset gi_Mverb2020, 7, gk_Mverb2020_Damping_Frequency
;vstparamset gi_Mverb2020, 8, gk_Mverb2020_Gain
k_gain = ampdb(gk_Mverb2020_level)
ainleft inleta "inleft"
ainright inleta "inright"
aoutleft, aoutright vstaudio gi_Mverb2020, ainleft, ainright
outleta "outleft", aoutleft
outleta "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gS_MVerb_preset init "Huge Hall"
gk_MVerb_FB init .975
gk_MVerb_wet init .5
gk_MVerb_random init 1
gk_MVerb_rslow init 1.1
gk_MVerb_rfast init 3.8
gk_MVerb_rmax init .0005
gk_MVerb_print init 1
gk_MVerb_DFact init .75
instr MVerb
//////////////////////////////////////////////
// Original csd by Jon Christopher Nelson.
// Adapted to C++ plugin by Michael Gogins.
// Compute-intensive!
//////////////////////////////////////////////
ainleft  inleta  "inleft"
ainright  inleta  "inright"
aoutleft, aoutright MVerb ainleft, ainright, gS_MVerb_preset; , "wet", gk_MVerb_wet, "FB", gk_MVerb_feedback, "random", 1, "rslow", gk_MVerb_rslow, "rfast", gk_MVerb_rfast, "rmax", gk_MVerb_rmax, "print", gk_MVerb_print, "DFact", gk_MVerb_DFact
outleta  "outleft", aoutleft
outleta  "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_ReverbDragonfly_Dry_Level init .5
gk_ReverbDragonfly_Early_Level init .05
gk_ReverbDragonfly_Late_Level init .75
gk_ReverbDragonfly_Size init .75
gk_ReverbDragonfly_Width init .75
gk_ReverbDragonfly_Predelay init .35
gk_ReverbDragonfly_Diffuse init .9
gk_ReverbDragonfly_Low_Cut init .8
gk_ReverbDragonfly_Low_Cross init .5
gk_ReverbDragonfly_Low_Mult init .5
gk_ReverbDragonfly_High_Cut init .85
gk_ReverbDragonfly_High_Cross init .5
gk_ReverbDragonfly_High_Mult init .34
gk_ReverbDragonfly_Spin init .015
gk_ReverbDragonfly_Wander init .025
gk_ReverbDragonfly_Decay init .075
gk_ReverbDragonfly_Early_Send init .25
gk_ReverbDragonfly_Modulation init .025
instr ReverbDragonfly
ainleft  inleta  "inleft"
ainright  inleta  "inright"
vstparamset gi_ReverbDragonfly,  0, gk_ReverbDragonfly_Dry_Level
vstparamset gi_ReverbDragonfly,  1, gk_ReverbDragonfly_Early_Level
vstparamset gi_ReverbDragonfly,  2, gk_ReverbDragonfly_Late_Level
vstparamset gi_ReverbDragonfly,  3, gk_ReverbDragonfly_Size
vstparamset gi_ReverbDragonfly,  4, gk_ReverbDragonfly_Width
vstparamset gi_ReverbDragonfly,  5, gk_ReverbDragonfly_Predelay
vstparamset gi_ReverbDragonfly,  6, gk_ReverbDragonfly_Diffuse
vstparamset gi_ReverbDragonfly,  7, gk_ReverbDragonfly_Low_Cut
vstparamset gi_ReverbDragonfly,  8, gk_ReverbDragonfly_Low_Cross
vstparamset gi_ReverbDragonfly,  9, gk_ReverbDragonfly_Low_Mult
vstparamset gi_ReverbDragonfly, 10, gk_ReverbDragonfly_High_Cut
vstparamset gi_ReverbDragonfly, 11, gk_ReverbDragonfly_High_Cross
vstparamset gi_ReverbDragonfly, 12, gk_ReverbDragonfly_High_Mult
vstparamset gi_ReverbDragonfly, 13, gk_ReverbDragonfly_Spin
vstparamset gi_ReverbDragonfly, 14, gk_ReverbDragonfly_Wander
vstparamset gi_ReverbDragonfly, 15, gk_ReverbDragonfly_Decay
vstparamset gi_ReverbDragonfly, 16, gk_ReverbDragonfly_Early_Send
vstparamset gi_ReverbDragonfly, 17, gk_ReverbDragonfly_Modulation
aoutleft, aoutright vstaudio gi_ReverbDragonfly, ainleft, ainright
outleta  "outleft", aoutleft
outleta  "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr ReverbBabo
ainleft  inleta  "inleft"
ainright  inleta  "inright"
ixsize  = 10  ; width  of the resonator
iysize  = 20  ; depth  of the resonator
izsize  = 8  ; height of the resonator
idiff   = 1  ; diffusion coefficient
;iexpert = p14  ; power user values stored in this function
; Two babos are required, one for left input and one for right input; outputs 
; are stereo and must be summed.
aleftleftout, aleftrightout babo ainleft*0.7, -4, -8, 2, ixsize, iysize, izsize, idiff ;, iexpert
arightleftout, arightrightout babo ainright*0.7, 4, -8, 2, ixsize, iysize, izsize, idiff;, iexpert
aoutleft = aleftleftout + arightleftout
aoutright = aleftrightout + arightrightout
outleta  "outleft", aoutleft
outleta  "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_ReverbSC_feedback init 0.875
gk_ReverbSC_wet init .5
gi_ReverbSC_delay_modulation init 0.0075
gk_ReverbSC_frequency_cutoff init 15000
instr ReverbSC
gk_ReverbSC_dry = 1.0 - gk_ReverbSC_wet
aleftin init 0
arightin init 0
aleftout init 0
arightout init 0
aleftin inleta "inleft"
arightin inleta "inright"
aleftout, arightout reverbsc aleftin, arightin, gk_ReverbSC_feedback, gk_ReverbSC_frequency_cutoff, sr, gi_ReverbSC_delay_modulation
aleftoutmix = aleftin * gk_ReverbSC_dry + aleftout * gk_ReverbSC_wet
arightoutmix = arightin * gk_ReverbSC_dry + arightout * gk_ReverbSC_wet
outleta "outleft", aleftoutmix
outleta "outright", arightoutmix
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_NReverb_wet init 0.5
gk_NReverb_time init 4
gk_NReverb_khdif init .75
gi_NReverb_nCombs init 8
gi_NReverb_fCombs ftgen 0, 0, 16, -2, -1116, -1188, -1277, -1356, -1422, -1491, -1557, -1617, .8, .79, .78, .77, .76, .75, .74, .73
gi_NReverb_nAllPass init 4
gi_NReverb_fAllPass ftgen 0, 0, 8, -2, -556, -441, -341, -225, .7, .72, .74, .76
instr NReverb
print gi_NReverb_nCombs, gi_NReverb_fCombs, gi_NReverb_nAllPass, gi_NReverb_fAllPass
ainleft inleta  "inleft"
ainright inleta  "inright"
gk_NReverb_dry = 1.0 - gk_NReverb_wet
; ares    nreverb asig,     ktime,           khdif      [, iskip] [,inumCombs]          [, ifnCombs]         [, inumAlpas]           [, ifnAlpas]
aleftout  nreverb ainleft,  gk_NReverb_time, gk_NReverb_khdif, 0, gi_NReverb_nCombs, gi_NReverb_fCombs, gi_NReverb_nAllPass, gi_NReverb_fAllPass
arightout nreverb ainright, gk_NReverb_time, gk_NReverb_khdif, 0, gi_NReverb_nCombs, gi_NReverb_fCombs, gi_NReverb_nAllPass, gi_NReverb_fAllPass
aleftoutmix = ainleft * gk_NReverb_dry + aleftout * gk_NReverb_wet
arightoutmix = ainright * gk_NReverb_dry + arightout * gk_NReverb_wet
outleta "outleft", aleftoutmix
outleta "outright", arightoutmix
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_MasterOutput_level init 0
gS_MasterOutput_filename init "check.wav"
instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
k_gain = ampdb(gk_MasterOutput_level)
printks2 "Master gain: %f\\n", k_gain
iamp init 1
aleft butterlp aleft, 18000
aright butterlp aright, 18000
outs aleft * k_gain, aright * k_gain
; We want something that will play on my phone.
i_amplitude_adjustment = 1; ampdbfs(-3) / 32767
i_filename_length strlen gS_MasterOutput_filename
if i_filename_length > 0 goto filename_exists
goto filename_endif
filename_exists:
prints sprintf("Output filename: %s\\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
filename_endif:
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

)";
    model.setCsoundOrchestra(orc);
    model.setCsoundScoreHeader("f 0 420\n");
    model.processArgv(argc, argv);
}

