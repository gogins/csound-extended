<CsoundSynthesizer>

<CsOptions>
-m165 -odac -m195 -Fbachfug.mid --midi-key=4 --midi-velocity=5
</CsOptions>

<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 5

gk_FaustPiano_level chnexport "gk_FaustPiano_level", 3 ;  0
gi_FaustPiano_attack chnexport "gi_FaustPiano_attack", 3 ;  0.003
gi_FaustPiano_release chnexport "gi_FaustPiano_release", 3 ;  0.01
gk_FaustPiano_midi_dynamic_range chnexport "gk_FaustPiano_midi_dynamic_range", 3 ;  20

gk_FaustPiano_level init 0
gi_FaustPiano_attack init 0.003
gi_FaustPiano_release init 0.01
gk_FaustPiano_midi_dynamic_range init 60

instr FaustPiano
//////////////////////////////////////////////
//  Instrument definition patch FaustPiano.
//  Author: Michael Gogins
//////////////////////////////////////////////
xtratim gi_FaustPiano_attack + gi_FaustPiano_release
a_declicking linsegr 0, gi_FaustPiano_attack, 1, p3, 1, gi_FaustPiano_release, 0
i_instrument = p1
i_time = p2
i_sustain = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustPiano_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
i_frequency = cpsmidinn(i_midi_key)
//  Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 53.5
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustPiano_level)

ip1_freq init i_frequency
ip2_gain init (i_midi_velocity / 127.)
ip3_gate init 1
ip4_Brightness_Factor init .3
ip5_Detuning_Factor init .01
ip6_Hammer_Hardness init 0
ip7_Stiffness_Factor init .28
ip8_reverbGain init .2
ip9_roomSize init 1
ip10_pan_angle init .5
ip11_spatial_width init .5
print ip1_freq
print ip2_gain
print ip3_gate
print ip4_Brightness_Factor
print ip5_Detuning_Factor
print ip6_Hammer_Hardness
print ip7_Stiffness_Factor
print ip8_reverbGain
print ip9_roomSize
print ip10_pan_angle
print ip11_spatial_width
a_left, a_right faust_piano ip1_freq, ip2_gain, ip3_gate, ip4_Brightness_Factor, ip5_Detuning_Factor, ip6_Hammer_Hardness, ip7_Stiffness_Factor, ip8_reverbGain, ip9_roomSize, ip10_pan_angle, ip11_spatial_width
outs a_left * k_gain, a_right * k_gain
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
