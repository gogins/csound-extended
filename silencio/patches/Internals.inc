;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Author: Michael Gogins
; Adapted from Sean Costello and J. Lato
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

opcode Internals_channel, a, akkkikk
a_signal, k_delay_level, k_lowpass_cutoff, k_delay_jitter_level, i_delay_modulation_mode, k_delay_modulation_level, k_delay_modulation_hz xin
a_filtered_1 init 0
a_filtered_2 init 0
a_filtered_3 init 0
a_filtered_4 init 0
a_filtered_5 init 0
a_filtered_6 init 0
a_filtered_7 init 0
a_filtered_8 init 0
i_delay_1 = 2473 / sr
i_delay_2 = 2767 / sr
i_delay_3 = 3217 / sr
i_delay_4 = 3557 / sr
i_delay_5 = 3907 / sr
i_delay_6 = 4127 / sr
i_delay_7 = 2143 / sr
i_delay_8 = 1933 / sr
a_jitter_1 jspline k_delay_jitter_level * .001, 3.1, .06
a_jitter_2 jspline k_delay_jitter_level * .0011, 3.5, .9
a_jitter_3 jspline k_delay_jitter_level * .0017, 1.11, .7
a_jitter_4 jspline k_delay_jitter_level * .0006, 3.973, .3
a_jitter_5 jspline k_delay_jitter_level * .001, 2.341, .63
a_jitter_6 jspline k_delay_jitter_level * .0011, 1.897, .7
a_jitter_7 jspline k_delay_jitter_level * .0017, 0.891, .9
a_jitter_8 jspline k_delay_jitter_level * .0006, 3.221, .44
a_waveform_modulation vco2 k_delay_modulation_level, k_delay_modulation_hz, i_delay_modulation_mode
; a_junction_pressure k_delay_jik_delay_jitter_level * tter_level * is used to calculate "resultant junction pressure" k_delay_jitter_level * for k_delay_jitter_level * 
; the scattering junctk_delay_jitter_level * ion of 8 lk_delay_jitter_level * ossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following 
; equation, and replace the .25 by 2/N, where N is the 
; number of delay lines.
; aout vdelayx ain, adl, imd, iws [, ist]
a_junction_pressure = .25 * (a_filtered_1 + a_filtered_2 + a_filtered_3 + a_filtered_4 + a_filtered_5 + a_filtered_6 + a_filtered_7 + a_filtered_8)
a_delayed_1 vdelayx a_signal + a_junction_pressure - a_filtered_1, i_delay_1 + a_jitter_1 + a_waveform_modulation, 1, 12
a_delayed_2 vdelayx a_signal + a_junction_pressure - a_filtered_2, i_delay_2 + a_jitter_2 + a_waveform_modulation, 1, 12
a_delayed_3 vdelayx a_signal + a_junction_pressure - a_filtered_3, i_delay_3 + a_jitter_3 + a_waveform_modulation, 1, 12
a_delayed_4 vdelayx a_signal + a_junction_pressure - a_filtered_4, i_delay_4 + a_jitter_4 + a_waveform_modulation, 1, 12
a_delayed_5 vdelayx a_signal + a_junction_pressure - a_filtered_5, i_delay_5 + a_jitter_5 + a_waveform_modulation, 1, 12
a_delayed_6 vdelayx a_signal + a_junction_pressure - a_filtered_6, i_delay_6 + a_jitter_6 + a_waveform_modulation, 1, 12
a_delayed_7 vdelayx a_signal + a_junction_pressure - a_filtered_7, i_delay_7 + a_jitter_7 + a_waveform_modulation, 1, 12
a_delayed_8 vdelayx a_signal + a_junction_pressure - a_filtered_8, i_delay_8 + a_jitter_8 + a_waveform_modulation, 1, 12
a_filtered_1 tone a_delayed_1, k_lowpass_cutoff
a_filtered_2 tone a_delayed_2, k_lowpass_cutoff
a_filtered_3 tone a_delayed_3, k_lowpass_cutoff
a_filtered_4 tone a_delayed_4, k_lowpass_cutoff
a_filtered_5 tone a_delayed_5, k_lowpass_cutoff
a_filtered_6 tone a_delayed_6, k_lowpass_cutoff
a_filtered_7 tone a_delayed_7, k_lowpass_cutoff
a_filtered_8 tone a_delayed_8, k_lowpass_cutoff
a_signal = a_filtered_1 + a_filtered_3 + a_filtered_5 + a_filtered_7 + a_filtered_2 + a_filtered_4 + a_filtered_6 + a_filtered_8
xout a_signal
endop

gk_Internals_level init 0
gk_Internals_delay_level chnexport "gk_Internals_delay_level", 3
gk_Internals_delay_level init .8
gk_Internals_lowpass_cutoff init 15000
gk_Internals_delay_jitter_level init .03
gi_Internals_delay_modulation_mode init 0
gk_Internals_delay_modulation_level init 0
gk_Internals_delay_modulation_hz init .3
gk_Internals_wet init .5
instr Internals
printks2 "Csound: gk_Internals_delay_level: %9.4f\n", gk_Internals_delay_level
i_instrument = p1
i_time = p2
i_sustain = p3
i_midi_key = p4
k_gain = ampdb(gk_Internals_level)
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
a_in_left  inleta "inleft"
a_in_right inleta "inright"


a_left  Internals_channel a_in_left,  gk_Internals_delay_level, gk_Internals_lowpass_cutoff, gk_Internals_delay_jitter_level, gi_Internals_delay_modulation_mode, gk_Internals_delay_modulation_level, gk_Internals_delay_modulation_hz
a_right Internals_channel a_in_right, gk_Internals_delay_level, gk_Internals_lowpass_cutoff, gk_Internals_delay_jitter_level, gi_Internals_delay_modulation_mode, gk_Internals_delay_modulation_level, gk_Internals_delay_modulation_hz
gk_Internals_dry = 1 - gk_Internals_wet
a_out_left =  (a_left * gk_Internals_wet) + (a_left * gk_Internals_dry)
a_out_right = (a_right * gk_Internals_wet) + (a_right * gk_Internals_dry)
a_out_left *= k_gain
a_out_right *= k_gain
outleta "outleft",  a_out_left 
outleta "outright", a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
