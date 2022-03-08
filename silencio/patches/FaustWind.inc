gi_FaustWind_compiled faustcompile {{
//----------------------`wind`--------------------------
// A very simple wind simulator, based on a filtered white noise
//
// #### Usage
//
// ```
//  wind(f) : _
// ```
//
// Where:
//
// * `f`: is the force of the wind: between 0 and 1
//
//----------------------------------------------------------

import("stdfaust.lib");

wind(force) = no.multinoise(2) : par(i, 2, ve.moog_vcf_2bn(force,freq)) : par(i, 2, *(force))
	with {
		freq = (force*87)+1 : ba.pianokey2hz;
	};

process = wind ( hslider("v:wind/force",0.66,0,1,0.01) : si.smooth (0.997) );

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustWind_compiled

gk_FaustWind_level chnexport "gk_FaustWind_level", 3
gk_FaustWind_pan chnexport "gk_FaustWind_pan", 3
gk_FaustWind_force chnexport "gk_FaustWind_force", 3
gk_FaustWind_Q chnexport "gk_FaustWind_Q", 3

gk_FaustWind_level init 0
gk_FaustWind_pan init .5
gk_FaustWind_force init .5
gk_FaustWind_Q init 15
gk_FaustWind_midi_dynamic_range init 20

instr FaustWind
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustWind_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 73
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustWind_level)
i_faust_dsp faustdsp gi_FaustWind_compiled
k_frequency cpsmidinn i_midi_key
k_gain = i_midi_velocity
faustctl i_faust_dsp, "force", gk_FaustWind_force
a_left, a_right faustplay i_faust_dsp
; Filter the dsp output to obtain a pitched sound.
; Bandwidth scales with frequency and energy must be inversely scaled.
; TODO: This scaling should be improved.
; alow, ahigh, aband svfilter  asig, kcf, kq [, iscl] [, iskip] 
i_scale init 1
alow, ahigh, a_out_left svfilter a_left, k_frequency, gk_FaustWind_Q, i_scale
alow, ahigh, a_out_right svfilter a_right, k_frequency, gk_FaustWind_Q, i_scale
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_out_left = a_out_left * i_amplitude * a_declicking * k_gain
a_out_right = a_out_right * i_amplitude * a_declicking * k_gain

#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin


