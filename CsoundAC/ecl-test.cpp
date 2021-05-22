#include <cstdio>
#include <ecl/ecl.h>
#include <iostream>
#include <string>

// Build with:

// g++ -Dlinux --std=gnu++17 -lstdc++fs -Wno-write-strings -O2 -g "ecl-test.cpp" -oecl-test -I/usr/local/include -L/usr/local/lib/ecl -lecl -lgc -lgc -lpthread -ldl -lm

cl_object evaluate_form(const std::string &form) {
    return cl_safe_eval(c_string_to_object(form.c_str()), Cnil, Cnil);
}

int main (int argc, char **argv) 
{
    cl_boot(argc, argv);
    evaluate_form("(require :asdf)");
    evaluate_form("(require :nudruz)");
    evaluate_form("(in-package :cm)");
    auto seq = evaluate_form(R"qqq(
(progn 
(print (format t "--- Hello, World, from Embeddable Common Lisp.~%"))
(defparameter csound-seq (new seq :name "csound-test"))
(events (tzplay) csound-seq)
;(render-with-orc csound-seq orc-text :channel-offset 25 :velocity-scale 100))
(print (format t "--- Good-bye, World, from Embeddable Common Lisp.~%"))
;(list-objects csound-seq)
csound-seq
)
    )qqq");
    cl_object subobjects = ecl_slot_value(seq, "subobjects");
    cl_object size = cl_list_length(subobjects);
    for (long int i = 0, n = ecl_fixnum(size); i < n; ++i) {
        cl_object event = cl_nth(ecl_make_fixnum(i), subobjects);
        auto time_= ecl_to_double(ecl_slot_value(event, "time"));
        auto channel = ecl_to_double(ecl_slot_value(event, "channel"));
        auto keynum = ecl_to_double(ecl_slot_value(event, "keynum"));
        auto duration = ecl_to_double(ecl_slot_value(event, "duration"));
        auto amplitude = ecl_to_double(ecl_slot_value(event, "amplitude"));
        System::debug("event %8ld t %9.4f c %9.4f k %9.4f d %9.4f a %9.4f\n", (1 + i), time_, channel, keynum, duration, amplitude);
    }
    cl_object result = evaluate_form("(list-objects csound-seq)");
    return 0;
}
