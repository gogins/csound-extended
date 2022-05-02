gk_OrganNoteOrganteq_midi_dynamic_range chnexport "gk_OrganNoteOrganteq_midi_dynamic_range", 3 ;  20

gk_OrganNoteOrganteq_midi_dynamic_range init 20

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
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
vstnote gi_Organteq, i_midi_channel, i_midi_key, i_midi_velocity, i_duration
endin
