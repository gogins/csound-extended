#include <Composition.hpp>
#include <MCRM.hpp>
#include <Eigen/Dense>
#include <functional>
#include <memory>
#include <MusicModel.hpp>
#include <random>
#include <Lisp.hpp>
#include <VoiceleadingNode.hpp>
#include <vector>

/**
 * All composition and synthesis code is defined in the main function.
 * There is no need for any of this code to be in a separate file.
 */
int main(int argc, const char **argv)
{
    csound::MusicModel model;
    // These fields determine output filenames and ID 3 tags.
    model.setAuthor("Michael Gogins");
    model.setTitle("LispNodeTest");
    model.setAlbum("Silence");
    model.setYear("2018");
    model.setPerformanceRightsOrganization("Irreducible Productions, ASCAP");
    csound::initialize_ecl(argc, (char **)argv);
    csound::evaluate_form(R"qqq((print (format t "--- Hello, World, from Embeddable Common Lisp.~%")))qqq");
    csound::evaluate_form("(require :asdf)");
    csound::evaluate_form("(require :nudruz)");
    csound::evaluate_form("(in-package :cm)");
    csound::LispGenerator lisp_generator;
    lisp_generator.appendTopLevelForm(R"qqq(
(progn
    (let ((csound-seq (new seq :name "csound-test")))
    (events (tzplay) csound-seq)
    (print (format t "--- Good-bye, World, from Embeddable Common Lisp.~%"))
    (list-objects csound-seq)
    csound-seq)
)
)qqq");
    csound::LispTransformer lisp_transformer;
    lisp_transformer.appendTopLevelForm(R"qqq(
(progn 
`   (print "Testing LispTransformer...")
    (print "Begore transformation...")
    (list-objects score-from-children)
    (map-objects (lambda (k) (+ k 12)) score-from-children :slot! 'keynum)
    (print "After transformation.")
    (list-objects score-from-children)
    score-from-children)
)
)qqq");
    lisp_transformer.addChild(&lisp_generator);
    model.addChild(&lisp_transformer);
    model.setTieOverlappingNotes(true);
    model.setDuration(240.);
    model.setCsoundOrchestra(R"(
sr = 48000
ksmps = 64
nchnls = 2 
0dbfs = 1

gi_aeolus aeolus_init "/home/mkg/stops-0.3.0", "Aeolus", "waves", 0, 3

instr 1 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 2 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 3 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 4 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 5
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 6
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

alwayson "aeolus_out"

; Send audio from the Aeolus to the output.
instr aeolus_out 
print p1, p2, p3
aeolus_preset gi_aeolus, 1, 1, "~/.aeolus-presets"
a_out[] init 2
a_out aeolus_out gi_aeolus
out a_out
endin                                
            )");
    model.processArgv(argc, argv);
}

