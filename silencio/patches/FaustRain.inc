gi_FaustRain_compiled faustcompile {{

//----------------------`rain`--------------------------
// A very simple rain simulator
//
// #### Usage
//
// ```
//  rain(d,l) : _,_
// ```
//
// Where:
//
// * `d`: is the density of the rain: between 0 and 1
// * `l`: is the level (volume) of the rain: between 0 and 1
//
//----------------------------------------------------------

import("stdfaust.lib");

rain(density,level) = no.multinoise(2) : par(i, 2, drop) : par(i, 2, *(level))
	with {
		drop = _ <: @(1), (abs < density) : *;
	};

process  = 	rain (
				hslider("v:rain/density", 300, 0, 1000, 1) / 1000,
				hslider("v:rain/volume", 0.5, 0, 1, 0.01)
			);

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustRain_compiled

gk_FaustRain_level chnexport "gk_FaustRain_level", 3
gk_FaustRain_pan chnexport "gk_FaustRain_pan", 3
gk_FaustRain_density chnexport "gk_FaustRain_density", 3
gk_FaustRain_volume chnexport "gk_FaustRain_volume", 3
gk_FaustRain_Q chnexport "gk_FaustRain_Q", 3

gk_FaustRain_level init 0
gk_FaustRain_pan init .5
gk_FaustRain_density init 300
gk_FaustRain_volume  init .5
gk_FaustRain_Q init 5
gk_FaustRain_midi_dynamic_range init 20

instr FaustRain
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustRain_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 87
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustRain_level)
i_faust_dsp faustdsp gi_FaustRain_compiled
k_frequency cpsmidinn i_midi_key
k_gain = i_midi_velocity
faustctl i_faust_dsp, "density", gk_FaustRain_density
faustctl i_faust_dsp, "volume", gk_FaustRain_volume
a_left, a_right faustplay i_faust_dsp
; Filter the dsp output to obtain a pitched sound.
; Bandwidth scales with frequency and energy must be inversely scaled.
; TODO: This scaling should be improved.
; alow, ahigh, aband svfilter  asig, kcf, kq [, iscl] [, iskip] 
i_scale init 2
alow, ahigh, a_out_left svfilter a_left, k_frequency, gk_FaustRain_Q, i_scale
alow, ahigh, a_out_right svfilter a_right, k_frequency, gk_FaustRain_Q, i_scale
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


