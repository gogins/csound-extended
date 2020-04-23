gk_CostelloGong_level init 0
gi_CostelloGong_attack init 0.003
gi_CostelloGong_release init 0.01
gk_CostelloGong_midi_dynamic_range init 127
gi_CostelloGong_pitch_modulation init 0
// Phase shift bandwidth is in octaves, centered on the pitch.
gi_CostelloGong_phase_shift_bandwidth init 1
gi_CostelloGong_reverb_feedback init .999
gi_CostelloGong_randomization init 1.2
gi_CostelloGong_lowpass init 12000
gi_CostelloGong_lowest_note init 42
gi_CostelloGong_sine ftgen 0, 0, 65537, 10, 1
instr CostelloGong
//////////////////////////////////////////////
// Gong by Sean Costello.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_CostelloGong_attack + gi_CostelloGong_release
i_midi_key = p4
// Low frequencies don't work very well.
while i_midi_key < gi_CostelloGong_lowest_note do
  i_midi_key += 12
od
i_midi_dynamic_range = i(gk_CostelloGong_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 84
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization * 1.5
k_gain = ampdb(gk_CostelloGong_level)
; Miller Puckette and John Stautner's 
; Feedback Delay Network Reverb, 
; converted into a gong.
; Sample note:
;   p1 p2 p3     p4     p5      p6    p7     p8     p9     p10    p11
; i 99 1  25     200    5000   .999   0      100    200    10000  1.2
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
adel1 init 0
adel2 init 0
adel3 init 0
adel4 init 0
alimit1 init 0
alimit2 init 0
alimit3 init 0
alimit4 init 0
alimita init 0
alimitb init 0
alimitc init 0
alimitd init 0
anoise = 0
ipi = 3.141592654
idel1 = 1/i_frequency 
ilooptime = idel1 * 3
idel2 = 1/i_frequency * 2.25
idel3 = 1/i_frequency * 3.616
idel4 = 1/i_frequency * 5.06
iamp = i_amplitude
ipitchmod = gi_CostelloGong_pitch_modulation
; bilinear z-transform, used to convert frequencies in
; the .sco file for the passive nonlinear filter into
; coefficients that the allpass filter can use. The
; frequency corresponds to the 90 degree phase shift
; frequency of the allpass filter.
i_half_bandwidth = gi_CostelloGong_phase_shift_bandwidth / 2
phase_shift_1 = cpsmidinn(i_midi_key - i_half_bandwidth)
phase_shift_2 = cpsmidinn(i_midi_key + i_half_bandwidth)
ilimit1 = (1 - ipi * phase_shift_1 / sr) / (1 + ipi * phase_shift_1 / sr)
ilimit2 = (1 - ipi * phase_shift_2 / sr) / (1 + ipi * phase_shift_2 / sr)
; print ilimit1, ilimit2
; A single period of a sine wave, with frequency 
; corresponding to the longest delay length in
; the network, is used to "strike" the gong.
krandenv linseg 1, ilooptime, 1, 0, 0, p3-ilooptime, 0
anoise oscili krandenv * gi_CostelloGong_randomization, i_frequency, gi_CostelloGong_sine
igain = gi_CostelloGong_reverb_feedback * 0.70710678117 ; gain of reverb
; used to add random pitch modulation to the
; delay lines. Useful for reverb, but not as 
; useful in this application.
k1 randi .001, 3.1, .06
k2 randi .0011, 3.5, .9
k3 randi .0017, 1.11, .7
k4 randi .0006, 3.973, .3
adum1 delayr 1
adel1a deltapi idel1 + k1 * ipitchmod
delayw anoise + afilt2 + afilt3
; Passive nonlinear filter, a la Scott Van Duyne
; and John Pierce. Essentially a single order
; allpass filter, where the coefficient switches
; between two different values, based upon the 
; state of the delay line it is associated with.
; Produces effects similar to FM. In addition,
; if the allpass filter is tuned to a low frequency
; it will produce a "detuning" of the harmonics in 
; the delay line that adds to the bell-like nature
; of the sound.
kdel1 downsamp adel1a
if kdel1 < 0 goto or
klimit1 = ilimit1
goto next
or:
klimit1 = ilimit2
next:
ax1 delay1 adel1a
adel1 = klimit1 * (adel1a + adel1) - ax1
; Repeat the above for the next 3 delay lines.
adum2 delayr 1
adel2a deltapi idel2 + k2 * ipitchmod
delayw anoise - afilt1 - afilt4
kdel2 downsamp adel2a
if kdel2 < 0 goto or2
klimit2 = ilimit1
goto next2
or2:
klimit2 = ilimit2
next2:
ax2 delay1 adel2a
adel2 = klimit2 * (adel2a + adel2) - ax2
adum3 delayr 1
adel3a deltapi idel3 + k3 * ipitchmod
delayw anoise + afilt1 - afilt4
kdel3 downsamp adel3a
if kdel3 < 0 goto or3
klimit3 = ilimit1
goto next3
or3:
klimit3 = ilimit2
next3:
ax3 delay1 adel3a
adel3 = klimit3 * (adel3a + adel3) - ax3
adum4 delayr 1
adel4a deltapi idel4 + k4 * ipitchmod
delayw anoise + afilt2 - afilt3
kdel4 downsamp adel4a
if kdel4 < 0 goto or4
klimit4 = ilimit1
goto next4
or4:
klimit4 = ilimit2
next4:
ax4 delay1 adel4a
adel4 = klimit4 * (adel4a + adel4) - ax4
afilt1 tone adel1 * igain, gi_CostelloGong_lowpass
afilt2 tone adel2 * igain, gi_CostelloGong_lowpass
afilt3 tone adel3 * igain, gi_CostelloGong_lowpass
afilt4 tone adel4 * igain, gi_CostelloGong_lowpass
aleft = (afilt1 + afilt3) * iamp
aright = (afilt2 + afilt4) * iamp
a_signal = aleft + aright
a_declicking linsegr 0, gi_CostelloGong_attack, 1, i_sustain, 1, gi_CostelloGong_release, 0
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;printks "CostelloGong i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin
