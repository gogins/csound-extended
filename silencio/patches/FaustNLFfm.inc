gi_FaustNLFfm_compiled faustcompile {{

declare name "NLFfm";
declare description "FM synthesizer implemented with a nonlinear passive allpass filter";
declare author "Romain Michon";
declare copyright "Romain Michon (rmichon@ccrma.stanford.edu)";
declare version "1.0";

import("instruments.lib");

//==================== GUI SPECIFICATION ================

freq = nentry("h:Basic_Parameters/freq [1][unit:Hz] [tooltip:Tone frequency]",440,20,20000,1);
gain = nentry("h:Basic_Parameters/gain [1][tooltip:Gain (value between 0 and 1)]",0.8,0,1,0.01); 
gate = button("h:Basic_Parameters/gate [1][tooltip:noteOn = 1, noteOff = 0]");

typeModulation = nentry("h:Physical_and_Nonlinearity/v:Nonlinear_Filter_Parameters/Modulation_Type 
[2][tooltip: 0=theta is modulated by the incoming signal; 1=theta is modulated by the averaged incoming signal;
2=theta is modulated by the squared incoming signal; 3=theta is modulated by a sine wave of frequency freqMod;
4=theta is modulated by a sine wave of frequency freq;]",0,0,4,1);
nonLinearity = hslider("h:Physical_and_Nonlinearity/v:Nonlinear_Filter_Parameters/Nonlinearity 
[2][tooltip:Nonlinearity factor (value between 0 and 1)]",0,0,1,0.01);
frequencyMod = hslider("h:Physical_and_Nonlinearity/v:Nonlinear_Filter_Parameters/Modulation_Frequency 
[2][unit:Hz][tooltip:Frequency of the sine wave for the modulation of theta (works if Modulation Type=3)]",220,20,1000,0.1);
nonLinAttack = hslider("h:Physical_and_Nonlinearity/v:Nonlinear_Filter_Parameters/Nonlinearity_Attack
[2][unit:s][Attack duration of the nonlinearity]",0.1,0,2,0.01);

vibratoFreq = hslider("h:Envelopes_and_Vibrato/v:Vibrato_Parameters/Vibrato_Freq 
[3][unit:Hz]",5,1,15,0.1);
vibratoGain = hslider("h:Envelopes_and_Vibrato/v:Vibrato_Parameters/Vibrato_Gain
[3][tooltip:A value between 0 and 1]",0.1,0,1,0.01);
vibratoAttack = hslider("h:Envelopes_and_Vibrato/v:Vibrato_Parameters/Vibrato_Attack 
[3][unit:s][tooltip:Vibrato attack duration]",0.5,0,2,0.01);
vibratoRelease = hslider("h:Envelopes_and_Vibrato/v:Vibrato_Parameters/Vibrato_Release 
[3][unit:s][tooltip:Vibrato release duration]",0.01,0,2,0.01);

envelopeAttack = hslider("h:Envelopes_and_Vibrato/v:Envelope_Parameters/Envelope_Attack 
[4][unit:s][tooltip:Envelope attack duration]",0.05,0,2,0.01);
envelopeDecay = hslider("h:Envelopes_and_Vibrato/v:Envelope_Parameters/Envelope_Decay 
[4][unit:s][tooltip:Envelope decay duration]",0.05,0,2,0.01);
envelopeRelease = hslider("h:Envelopes_and_Vibrato/v:Envelope_Parameters/Envelope_Release 
[4][unit:s][tooltip:Envelope release duration]",0.05,0,2,0.01);

//----------------------- Nonlinear filter ----------------------------
//nonlinearities are created by the nonlinear passive allpass ladder filter declared in miscfilter.lib

//nonlinear filter order
nlfOrder = 3; 

//attack - sustain - release envelope for nonlinearity (declared in instruments.lib)
envelopeMod = en.asr(nonLinAttack,1,envelopeRelease,gate);

//nonLinearModultor is declared in instruments.lib, it adapts allpassnn from miscfilter.lib 
//for using it with waveguide instruments
NLFM =  nonLinearModulator((nonLinearity : si.smoo),envelopeMod,freq,
     typeModulation,(frequencyMod : si.smoo),nlfOrder);

//----------------------- Algorithm implementation ----------------------------

//stereoizer is declared in instruments.lib and implement a stereo spacialisation in function of 
//the frequency period in number of samples 
stereo = stereoizer(ma.SR/freq);

//vibrato gain is controlled by envVibrato (declared in instruments.lib)
vibrato = os.osc(vibratoFreq)*vibratoGain*envVibrato(0.1*2*vibratoAttack,0.9*2*vibratoAttack,100,vibratoRelease,gate);

//output gain is controlled by an adsr envelope
envelope = en.adsr(envelopeAttack,envelopeDecay,0.9,envelopeRelease,gate)*gain;
breath = envelope + envelope*vibrato;

process = os.osc(freq)*breath : NLFM : stereo : instrReverb;


}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustNLFfm_compiled

gk_FaustNLFfm_level chnexport "gk_FaustNLFfm_level", 3
gk_FaustNLFfm_gain chnexport "gk_FaustNLFfm_gain", 3
gk_FaustNLFfm_Envelope_Attack chnexport "gk_FaustNLFfm_Envelope_Attack", 3
gk_FaustNLFfm_Envelope_Decay chnexport "gk_FaustNLFfm_Envelope_Decay", 3
gk_FaustNLFfm_Envelope_Release chnexport "gk_FaustNLFfm_Envelope_Release", 3
gk_FaustNLFfm_Vibrato_Attack chnexport "gk_FaustNLFfm_Vibrato_Attack", 3
gk_FaustNLFfm_Vibrato_Freq chnexport "gk_FaustNLFfm_Vibrato_Freq", 3
gk_FaustNLFfm_Vibrato_Gain chnexport "gk_FaustNLFfm_Vibrato_Gain", 3
gk_FaustNLFfm_Vibrato_Release chnexport "gk_FaustNLFfm_Vibrato_Release", 3
gk_FaustNLFfm_Modulation_Frequency chnexport "gk_FaustNLFfm_Modulation_Frequency", 3
gk_FaustNLFfm_Modulation_Type chnexport "gk_FaustNLFfm_Modulation_Type", 3
gk_FaustNLFfm_Nonlinearity chnexport "gk_FaustNLFfm_Nonlinearity", 3
gk_FaustNLFfm_Nonlinearity_Attack chnexport "gk_FaustNLFfm_Nonlinearity_Attack", 3
gk_FaustNLFfm_reverbGain chnexport "gk_FaustNLFfm_reverbGain", 3
gk_FaustNLFfm_roomSize chnexport "gk_FaustNLFfm_roomSize", 3
gk_FaustNLFfm_pan_angle chnexport "gk_FaustNLFfm_pan_angle", 3
gk_FaustNLFfm_spatial_width chnexport "gk_FaustNLFfm_spatial_width", 3

gk_FaustNLFfm_level init 0
gk_FaustNLFfm_gain init .8
gk_FaustNLFfm_Envelope_Attack init .05
gk_FaustNLFfm_Envelope_Decay init .05
gk_FaustNLFfm_Envelope_Release init .05
gk_FaustNLFfm_Vibrato_Attack init .5
gk_FaustNLFfm_Vibrato_Freq init 5 
gk_FaustNLFfm_Vibrato_Gain init .1
gk_FaustNLFfm_Vibrato_Release init .01
gk_FaustNLFfm_Modulation_Frequency init 220
gk_FaustNLFfm_Modulation_Type init 3
gk_FaustNLFfm_Nonlinearity init .1
gk_FaustNLFfm_Nonlinearity_Attack init .1
gk_FaustNLFfm_reverbGain init .137
gk_FaustNLFfm_roomSize init 1.72
gk_FaustNLFfm_pan_angle init .6
gk_FaustNLFfm_spatial_width init .5
gk_FaustNLFfm_midi_dynamic_range init 20

instr FaustNLFfm
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustNLFfm_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 69
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustNLFfm_level)
i_faust_dsp faustdsp gi_FaustNLFfm_compiled
k_frequency cpsmidinn i_midi_key
faustctl i_faust_dsp, "freq", k_frequency
faustctl i_faust_dsp, "gain", gk_FaustNLFfm_gain
faustctl i_faust_dsp, "Envelope_Attack", gk_FaustNLFfm_Envelope_Attack
faustctl i_faust_dsp, "Envelope_Decay", gk_FaustNLFfm_Envelope_Decay
faustctl i_faust_dsp, "Envelope_Release", gk_FaustNLFfm_Envelope_Release
faustctl i_faust_dsp, "Vibrato_Attack", gk_FaustNLFfm_Vibrato_Attack
faustctl i_faust_dsp, "Vibrato_Freq", gk_FaustNLFfm_Vibrato_Freq
faustctl i_faust_dsp, "Vibrato_Gain", gk_FaustNLFfm_Vibrato_Gain
faustctl i_faust_dsp, "Vibrato_Release", gk_FaustNLFfm_Vibrato_Release
faustctl i_faust_dsp, "Modulation_Frequency", gk_FaustNLFfm_Modulation_Frequency
faustctl i_faust_dsp, "Modulation_Type", gk_FaustNLFfm_Modulation_Type
faustctl i_faust_dsp, "Nonlinearity", gk_FaustNLFfm_Nonlinearity
faustctl i_faust_dsp, "Nonlinearity_Attack", gk_FaustNLFfm_Nonlinearity_Attack
faustctl i_faust_dsp, "reverbGain", gk_FaustNLFfm_reverbGain
faustctl i_faust_dsp, "roomSize", gk_FaustNLFfm_roomSize
faustctl i_faust_dsp, "pan angle", gk_FaustNLFfm_pan_angle
faustctl i_faust_dsp, "spatial width", gk_FaustNLFfm_spatial_width
faustctl i_faust_dsp, "gate", k(1)
a_right, a_left faustplay i_faust_dsp
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_right = a_right * i_amplitude * a_declicking * k_gain
a_left = a_left * i_amplitude * a_declicking * k_gain

#ifdef USE_SPATIALIZATION
a_signal = a_left + a_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
outleta "outleft", a_left
outleta "outright", a_right
#endif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin


