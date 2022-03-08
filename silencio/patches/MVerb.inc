gS_MVerb_preset chnexport "gS_MVerb_preset", 3 ; "Huge Hall"
gk_MVerb_FB chnexport "gk_MVerb_feedback", 3 ; .975
gk_MVerb_wet chnexport "gk_MVerb_wet", 3 ; .5
gk_MVerb_random chnexport "gk_MVerb_random", 3 ; 1
gk_MVerb_rslow chnexport "gk_MVerb_rslow", 3 ; 1.1
gk_MVerb_rfast chnexport "gk_MVerb_rfast", 3 ; 3.8
gk_MVerb_rmax chnexport "gk_MVerb_rmax", 3 ; .0005
gk_MVerb_print chnexport "gk_MVerb_print", 3 ; 1
gk_MVerb_DFact chnexport "gk_MVerb_DFact", 3 ; .75

gS_MVerb_preset init "Huge Hall"
gk_MVerb_feedback init .975
gk_MVerb_wet init .5
gk_MVerb_random init 1
gk_MVerb_rslow init 1.1
gk_MVerb_rfast init 3.8
gk_MVerb_rmax init .0005
gk_MVerb_print init 1
gk_MVerb_DFact init .75

instr MVerb
//////////////////////////////////////////////
// Original csd by Jon Christopher Nelson.
// Adapted to C++ plugin by Michael Gogins.
// Compute-intensive!
//////////////////////////////////////////////
ainleft  inleta  "inleft"
ainright  inleta  "inright"
aoutleft, aoutright MVerb ainleft, ainright, gS_MVerb_preset, "wet", gk_MVerb_wet, "FB", gk_MVerb_feedback, "random", 1, "rslow", gk_MVerb_rslow, "rfast", gk_MVerb_rfast, "rmax", gk_MVerb_rmax, "print", gk_MVerb_print, "DFact", gk_MVerb_DFact
outleta  "outleft", aoutleft
outleta  "outright", aoutright
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
