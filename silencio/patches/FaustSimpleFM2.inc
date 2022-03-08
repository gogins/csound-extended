gi_FaustSimpleFM2_compiled faustcompile {{

import("all.lib");

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Simple FM synthesizer.
// 2 oscillators and FM feedback on modulant oscillator
//
///////////////////////////////////////////////////////////////////////////////////////////////////
// MIDI IMPLEMENTATION:
//
// CC 1 : FM feedback on modulant oscillator.
// CC 14 : Modulator frequency ratio.
//
// CC 73 : Attack
// CC 76 : Decay
// CC 77 : Sustain
// CC 72 : Release
//
///////////////////////////////////////////////////////////////////////////////////////////////////

// GENERAL, Keyboard
midigate = button("gate");
midifreq = nentry("freq[unit:Hz]", 440, 20, 20000, 1);
midigain = nentry("gain", 1, 0, 1, 0.01);

// modwheel:
feedb = (gFreq-1) * (hslider("feedb[midi:ctrl 1]", 0, 0, 1, 0.001) : si.smoo);
modFreqRatio = hslider("ratio[midi:ctrl 14]",2,0,20,0.01) : si.smoo;

// pitchwheel
bend = ba.semi2ratio(hslider("bend [midi:pitchwheel]",0,-2,2,0.01));

gFreq = midifreq * bend;

//=================================== Parameters Mapping =================================
//========================================================================================
// Same for volum & modulation:
volA = hslider("A[midi:ctrl 73]",0.01,0.01,4,0.01);
volD = hslider("D[midi:ctrl 76]",0.6,0.01,8,0.01);
volS = hslider("S[midi:ctrl 77]",0.2,0,1,0.01);
volR = hslider("R[midi:ctrl 72]",0.8,0.01,8,0.01);
envelop = en.adsre(volA,volD,volS,volR,midigate);

// modulator frequency
modFreq = gFreq*modFreqRatio;

// modulation index
FMdepth = envelop * 1000 * midigain;

// Out amplitude
vol = envelop;

//============================================ DSP =======================================
//========================================================================================

FMfeedback(frq) = (+(_,frq):os.osci ) ~ (* (feedb));
FMall(f) = os.osci(f+(FMdepth*FMfeedback(f*modFreqRatio)));

process = FMall(gFreq) * vol;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustSimpleFM2_compiled

gk_FaustSimpleFM2_level chnexport "gk_FaustSimpleFM2_level", 3
gk_FaustSimpleFM2_A chnexport "gk_FaustSimpleFM2_A", 3
gk_FaustSimpleFM2_D chnexport "gk_FaustSimpleFM2_D", 3
gk_FaustSimpleFM2_S chnexport "gk_FaustSimpleFM2_S", 3
gk_FaustSimpleFM2_R chnexport "gk_FaustSimpleFM2_R", 3
gk_FaustSimpleFM2_gain chnexport "gk_FaustSimpleFM2_gain", 3
gk_FaustSimpleFM2_ratio chnexport "gk_FaustSimpleFM2_ratio", 3
gk_FaustSimpleFM2_feedb chnexport "gk_FaustSimpleFM2_feedb", 3

gk_FaustSimpleFM2_level init 0
gk_FaustSimpleFM2_A init .01
gk_FaustSimpleFM2_D init .93
gk_FaustSimpleFM2_S init .31
gk_FaustSimpleFM2_R init 1.62
gk_FaustSimpleFM2_feedb init .125
gk_FaustSimpleFM2_ratio init 10.49
gk_FaustSimpleFM2_midi_dynamic_range init 20

instr FaustSimpleFM2
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustSimpleFM2_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 74
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustSimpleFM2_level)
i_faust_dsp faustdsp gi_FaustSimpleFM2_compiled
k_frequency cpsmidinn i_midi_key
faustctl i_faust_dsp, "freq", k_frequency
faustctl i_faust_dsp, "A", gk_FaustSimpleFM2_A
faustctl i_faust_dsp, "D", gk_FaustSimpleFM2_D
faustctl i_faust_dsp, "S", gk_FaustSimpleFM2_S
faustctl i_faust_dsp, "R", gk_FaustSimpleFM2_R
faustctl i_faust_dsp, "feedb", gk_FaustSimpleFM2_feedb
faustctl i_faust_dsp, "ratio", gk_FaustSimpleFM2_ratio
faustctl i_faust_dsp, "gate", k(1)
a_signal faustplay i_faust_dsp
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain

#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin



