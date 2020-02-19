gk_PhysicalModel2_level init 0
gi_PhysicalModel2_attack init 0.003
gi_PhysicalModel2_release init 0.01
gi_PhysicalModel2_damping init 100
gk_PhysicalModel2_midi_dynamic_range init 127
;;;gi_PhysicalModel2_sine ftgen 0, 0, 65537, 10, 1
instr PhysicalModel2
//////////////////////////////////////////////
//  Author: Josep Comajuncosas
//  Adapted by Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_PhysicalModel2_attack + gi_PhysicalModel2_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_PhysicalModel2_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
//  Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
//  AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
//  Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 156
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_PhysicalModel2_level)

;Two masses, 1 end. Scheme : m1<-->m2<-->(grnd)
;coded by Josep M Comajuncosas / gelida@intercom.es

i2pi2 =   39.47841760436; (2pi)^2
;set initial position and velocity of the mass
iv0 = 0
ix0 = 10000
;set frequency, z and m
;ifreq = cpspch(p4)
ifreq = i_frequency
iz = gi_PhysicalModel2_damping
im1 = 30
im2 = 30
;normalize (don't touch this!)
iz = iz/(1000000000*sr); z given in N*s/(kg*10e-9)
im1 = im1/1000000000; mass given in kg*10e-9
im2 = im2/1000000000; mass given in kg*10e-9
;calculate k according to given freq
ik = i2pi2*ifreq*ifreq*im1
ik = ik/(sr*sr)
ax1 init ix0
axprev1 init ix0-1000*iv0/sr
ax2 init 0
axprev2 init 0
af1 = ik*(ax2-ax1) + iz*((ax2-axprev2)-(ax1-axprev1))
af2 = -af1 + ik*(-ax2) + iz*(-(ax2-axprev2))
anext1 = af1/im1 + 2*ax1-axprev1
anext2 = af2/im2 + 2*ax2-axprev2
a_signal = anext2
axprev1 = ax1
axprev2 = ax2
ax1 = anext1
ax2 = anext2

a_declicking linsegr 0, gi_PhysicalModel2_attack, 1, i_sustain, 1, gi_PhysicalModel2_release, 0
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
;printks "PhysicalModel2      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin
