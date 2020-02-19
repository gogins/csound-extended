gk_KarplusStrong3_pluck init .8
gk_KarplusStrong3_level init 0
gi_KarplusStrong3_attack init 0.003
gi_KarplusStrong3_release init 0.01
gi_KarplusStrong3_balance init .6
gk_KarplusStrong3_midi_dynamic_range init 127
gi_KarplusStrong3_sine ftgen 0, 0, 65537, 10, 1
gi_KarplusStrong3_sigmoid ftgen 0, 0, 65537, 19, .5, 1, 270, 1
instr KarplusStrong3
//////////////////////////////////////////////
// Instrument by Josep Comajuncosa
// Adapted by Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_KarplusStrong3_attack + gi_KarplusStrong3_release
i_midi_key = p4
//  Reflect pitches not good for this instrument back into a range that is 
//  good.
while (i_midi_key < 0) do
  i_midi_key += 12
od
while (i_midi_key > 128) do
  i_midi_key -= 12
od
i_midi_dynamic_range = i(gk_KarplusStrong3_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 78.2
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_KarplusStrong3_level)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Low level implementation
; of the classic Karplus-Strong algorithm
; fixed pitches : no vibratos or glissandi !
; implemented by Josep M Comajuncosas / Aug´98
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialised with a wide pulse (buzz)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ipluck = i(gk_KarplusStrong3_pluck); pluck position ( 0 to 1 )
ifreq = i_frequency
idlts = int(kr/ifreq-.5); set waveguide length (an integer number of samples)
idlt = idlts/kr; convert so seconds
idlt max ksmps / sr, idlt
kdlt init idlts; counter for string initialisation
irems = kr/ifreq - idlts +.5;remaining time in fractions of a sample
; set phase delay induced by the FIR lowpass filter
; and the fractional delay in the waveguide
iph = (1-irems)/(1+irems); allpass filter parameter
; approximation valid at low frequencies relative to sr
awgout init 0
if kdlt < 0 goto continue
initialise:
abuzz buzz i_amplitude, 1/idlt, i_amplitude*idlt, gi_KarplusStrong3_sine, ipluck
; fill the buffer with a bandlimited pulse
; knh controls bandwidth
; harmonic richness grows with volume
acomb delay abuzz, ipluck/idlt
apulse = abuzz - acomb
; implement pluck point as a FIR comb filter
continue:
areturn delayr idlt
ainput = apulse + areturn
alpf filter2 ainput, 2, 0, .5, .5
; lowpass filter to simulate energy losses
; could be variable to allow damping control
awgout filter2 alpf, 2, 1, iph, 1, iph
; allpass filter to fine tune the instrument
;awgout dcblock awgout; this seems necessary
; should be compensated in the delay line
; for better pitch accuracy
delayw awgout
awgout dcblock2 awgout; this seems necessary
; ideally should be inside the loop, but then
; the phase delay should be compensated
; for better pitch accuracy
a_signal = awgout
kdlt = kdlt - 1
anoize = 0; supress last impulse when waveguide is loaded
;tricky but easy...
a_declicking linsegr 0, gi_KarplusStrong3_attack, 1, i_sustain, 1, gi_KarplusStrong3_release, 0
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
;printks "KarplusStrong3 i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin
