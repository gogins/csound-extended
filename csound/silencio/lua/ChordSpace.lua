local Silencio = require("Silencio")
local metalua_serialize = require("metalua_serialize")
local io = require("io")
local string = require("string")

local ChordSpace = {}

function ChordSpace.help()
print [[

Correction project:

(1) Diff with ChordSpace.hpp and ensure that the
    algorithms are IDENTICAL TO ChordSpace.hpp.
(2) Restore ChordSpaceView and if the math is not
    correct, or is improved in any way, ensure that both
    files are corrected and improved with IDENTICAL
    algorithms.

Method:

(1) Run down ChordSpace.lua and check EACH function
    against ChordSpace.hpp. Correct the Lua code
    if necessary. When each function is correct,
    mark it as literally "-- EQUIVALENT"
(2) Diff the chord space group files as a final test.
    With the possible exception of rounding errors,
    the files should be IDENTICAL.

So far, I am assuming that operators are indeed calling
their metamethods. VERIFY THIS WITH PRINTING.

C H O R D S P A C E

Copyright 2010, 2011 by Michael Gogins.

This software is licensed under the terms of the GNU Lesser General Public
License.

This package, part of Silencio, implements a geometric approach to some common
operations on chords in neo-Riemannian music theory for use in score
generating procedures:

--  Identifying whether a chord belongs to some equivalence class of music
    theory, or sending a chord to its equivalent within a representative
    fundamental domain of some equivalence class. The equivalence classes are
    octave (O), permutational (P), transpositional, (T), inversional (I), and
    their compounds OP, OPT (set-class or chord type), and OPTI (prime form).

--  Causing chord progressions to move strictly within an orbifold that
    generates some equivalence class.

--  Implementing chord progressions based on the L, P, R, D, K, and Q
    operations of neo-Riemannian theory (thus implementing some aspects of
    "harmony").

--  Implementing chord progressions performed within a more abstract
    equivalence class by means of the best-formed voice-leading within a less
    abstract equivalence class (thus implementing some fundamentals of
    "counterpoint").

The associated ChordSpaceView package can display these chord spaces and
operations for trichords in an interactive 3-dimensional view.

DEFINITIONS

Pitch is the perception of a distinct sound frequency. It is a logarithmic
perception; octaves, which sound 'equivalent' in some sense, represent
doublings or halvings of frequency.

Pitches and intervals are represented as real numbers. Middle C is 60 and the
octave is 12. Our usual system of 12-tone equal temperament, as well as MIDI
key numbers, are completely represented by the whole numbers; any and all
other pitches can be represented simply by using fractions.

A voice is a distinct sound that is heard as having a pitch.

A chord is simply a set of voices heard at the same time, represented here
as a point in a chord space having one dimension of pitch for each voice
in the chord.

For the purposes of algorithmic composition in Silencio, a score is considered
as a sequence of more or less fleeting chords.

EQUIVALENCE CLASSES

An equivalence class identifies elements of a set. Operations that send one
equivalent point to another induce quotient spaces or orbifolds, where the
equivalence operation identifies points on one face of the orbifold with
points on an opposing face. The fundamental domain of the equivalence class
is the space "within" the orbifold.

Plain chord space has no equivalence classes. Ordered chords are represented
as vectors in parentheses (p1, ..., pN). Unordered chords are represented as
sorted vectors in braces {p1, ..., pN}. Unordering is itself an equivalence
class.

The following equivalence classes apply to pitches and chords, and exist in
different orbifolds. Equivalence classes can be combined (Callendar, Quinn,
and Tymoczko, "Generalized Voice-Leading Spaces," _Science_ 320, 2008), and
the more equivalence classes are combined, the more abstract is the resulting
orbifold compared to the parent space.

In most cases, a chord space can be divided into a number, possibly
infinite, of geometrically equivalent fundamental domains for the same
equivalence class. Therefore, here we use the notion of 'representative'
fundamental domain. For example, the representative fundamental domain of
unordered sequences, out of all possible orderings, consists of all sequences
in their ordinary sorted order. It is important, in the following, to identify
representative fundamental domains that combine properly, e.g. such that the
representative fundamental domain of OP / the representative fundamental
domain of PI equals the representative fundamental domain of OPI. And this in
turn may require accounting for duplicate elements of the representative
fundamental domain caused by reflections or singularities in the orbifold.

C       Cardinality equivalence, e.g. {1, 1, 2} == {1, 2}. _Not_ assuming
        cardinality equivalence ensures that there is a proto-metric in plain
        chord space that is inherited by all child chord spaces. Cardinality
        equivalence is never assumed here, because we are working in chord
        spaces of fixed dimensionality; e.g. we represent the note middle C
        not as {60}, but as {60, 60, ..., 60}.

O       Octave equivalence. The fundamental domain is defined by the pitches
        in a chord spanning the range of an octave or less, and summing to
        an octave or less.

P       Permutational equivalence. The fundamental domain is defined by a
        "wedge" of plain chord space in which the voices of a chord are always
        sorted by pitch.

T       Transpositional equivalence, e.g. {1, 2} == {7, 8}. The fundamental
        domain is defined as a plane in chord space at right angles to the
        diagonal of unison chords. Represented by the chord always having a
        sum of pitches equal to 0.

I       Inversional equivalence. Care is needed to distinguish the
        mathematician's sense of 'invert', which means 'pitch-space inversion'
        or 'reflect in a point', from the musician's sense of 'invert', which
        varies according to context but in practice often means 'registral
        inversion' or 'revoice by adding an octave to the lowest tone of a
        chord.' Here, we use 'invert' and 'inversion' in the mathematician's
        sense, and we use the terms 'revoice' and 'voicing' for the musician's
        'invert' and 'inversion'. The inversion point for any inversion lies
        on the unison diagonal. A fundamental domain is defined as any half of
        chord space that is bounded by a plane containing the inversion point.
        Represented as the chord having the first interval between voices be
        smaller than or equal to the final interval (recursing for chords of
        more than 3 voices).

PI      Inversional equivalence with permutational equivalence. The
        'inversion flat' of unordered chord space is a hyperplane consisting
        of all those unordered chords that are invariant under inversion. A
        fundamental domain is defined by any half space bounded by a
        hyperplane containing the inversion flat. It is represented as that
        half of the space on or lower than the hyperplane defined by the
        inversion flat and the unison diagonal.

OP      Octave equivalence with permutational equivalence. Tymoczko's orbifold
        for chords; i.e. chords with a fixed number of voices in a harmonic
        context. The fundamental domain is defined as a hyperprism one octave
        long with as many sides as voices and the ends identified by octave
        equivalence and one cyclical permutation of voices, modulo the
        unordering. In OP for trichords in 12TET, the augmented triads run up
        the middle of the prism, the major and minor triads are in 6
        alternating columns around the augmented triads, the two-pitch chords
        form the 3 sides, and the one-pitch chords form the 3 edges that join
        the sides.

OPT     The layer of the OP prism as close as possible to the origin, modulo
        the number of voices. Chord type. Note that CM and Cm are different
        OPT. Because the OP prism is canted down from the origin, at least one
        pitch in each OPT chord (excepting the origin itself) is negative.

OPI     The OP prism modulo inversion, i.e. 1/2 of the OP prism. The
        representative fundamental consits of those chords less than or equal
        to their inversions modulo OP.

OPTI    The OPT layer modulo inversion, i.e. 1/2 of the OPT layer.
        Set-class. Note that CM and Cm are the same OPTI.

OPERATIONS

Each of the above equivalence classes is, of course, an operation that sends
chords outside the fundamental domain to chords inside the fundamental domain.
And we define the following additional operations:

T(p, x)         Translate p by x.

I(p [, x])      Reflect p in x, by default the origin.

P               Send a major triad to the minor triad with the same root,
                or vice versa (Riemann's parallel transformation).

L               Send a major triad to the minor triad one major third higher,
                or vice versa (Riemann's Leittonwechsel or leading-tone
                exchange transformation).

R               Send a major triad to the minor triad one minor third lower,
                or vice versa (Riemann's relative transformation).

D               Send a triad to the next triad a perfect fifth lower
                (dominant transformation).

P, L, and R have been extended as follows, see Fiore and Satyendra,
"Generalized Contextual Groups", _Music Theory Online_ 11, August 2008:

K(c)            Interchange by inversion;
                K(c) := I(c, c[1] + c[2]).
                This is a generalized form of P; for major and minor triads,
                it is exactly the same as P, but it also works with other
                chord types.

Q(c, n, m)      Contexual transposition;
                Q(c, n, m) := T(c, n) if c is a T-form of m,
                or T(c, -n) if c is an I-form of M. Not a generalized form
                of L or R; but, like them, K and Q generate the T-I group.

J(c, n [, i [, g] ])  Contextual inversion;
                J(c, n [, i ], g] ]) returns all (or, optionally, the ith) 
                inversion(s) of chord c that preserve n pitch-classes of c. 
                The remaining pitches of c invert "around" the invariant 
                pitch-classes. If there is no such inversion, an empty list is 
                returned. If there is more than one such inversion, a list of 
                them is returned, ordered by pitch.
                Algorithm: 
                (1) Create an empty set of inverted chords.
                (2) For each pitch-class from 0 to 11 step g (g is the 
                    generator of transposition, e.g. g = 1 for TET):
                    (a) Invert c in the pitch-class.
                    (b) Test if n pitch-classes of c are invariant. 
                    (c) If so, add the inverted chord to the set of 
                        inversions in octave-equivalent and 
                        permutation-equivalent form.
                (3) Return the sorted set of inversions (or, optionally, the 
                    ith inversion in the set).
]]
end
--[[

LOG

2011-Sep-07

Redoing this package from scratch using GVLS formulas.

2011-Sep-09

There are definite problems with side effects in these tests. The first test
passes, but not when in series with another test.

Is there a bug in "~=" for Lua and/or LuaJIT?

2011-Sep-10

There is definitely one or more bugs in LuaJIT vs. Lua. Tests run to 4 or 5
voices in Lua that do not work in LuaJIT. There appear to be false temporaries
or something in LuaJIT.

2011-Sep-11

I am going to redo the equivalence formulas in sets: vanilla GVLS, GVLS with
my modifications, and mine. This seems like the only way of sorting out the
mess.

2011-Sep-22

It may be that my R, P, T, and I do not fit together to make OPTI because my I
does not use the inversion flat.

[2011-Sep-28: Not so, but because my T was aligned on 12-TET.]

2011-Sep-27

Redo tests of equivalences. [2011-Sep-28: They pass up to 5 voices.]

2011-Sep-28

First, some lessons learned painfully. I can still think quite well, but I AM
slower, and I have to ALLOW for that. So I actually need to think MORE
CAREFULLY (which takes longer) in order to make sure tests and logic are quite
clear, as this will save yet more time in testing and debugging. E.g., if I
had had my current unit tests in place when I began to rewrite this code,
I would have been where I am now four or five months ago. It wouldn't hurt,
either, to ask for help sooner rather than later -- DT's advice was extremely
helpful.

OK, I get it now. My prior attempts to combine representative fundamental
domains were ill-advised.

The fundamental domain formulas are now as correct as the tests I have written
can tell, although I should try to come up with some additional tests. Yet
there still may be problems...

2011-Oct-11

RPTT and RPTTI need unit tests. The definitions in :information and in
ChordSpaceGroup are not consistent, this is why ChordSpaceGroup is failing.

2011-Oct-13

I have redone ChordSpaceGroup using different orbifolds and equivalences that
keep operations in OPTI, I, and T strictly within equal temperament. I do not
use the GVLS fundamental domains directly for this at all. The symmetries of
the operations are all I need musically, so I simply put the GVLS OPTIs into
equal temperament and enumerate them for my set-class group (OPTTI). I do this
by taking the floor of the OPTI and then transposing it up by one unit of
transposition.

2011-Oct-16

Problems with ChordSpaceGroup:toChord and :fromChord. Look at voicing number
and transposition.

2011-Oct-17

Problems, e.g. {-4, 0, 4} transposed 4 is also eOP {-4, 0, 4}, ditto
transposed 8 and 12. The inversion flat would behave similiarly.

In other words, for some chords ChordSpaceGroup:toChord will give the same
chord for several different P, I, T, V, but ChordSpaceGroup:fromChord can only
give the same P, I, T, V for all of those chords. This is correct, but it
means that in unit testing from and should compare only chords, not numbers.

2011-Oct-19

In 1 dimension, all pitches under OP equivalence fall in the fundamental
domain {[0, 12)}; there are no duplicate equivalents.

In 2 dimensions, all chords under OP equivalence fall in the fundamental
domain {[-6, 6], [-6, 6], [12, 0], [0, 12]}; there are duplicate
equivalents, because otherwise the chord {11, 11} has no equivalent in
the domain (it is actually {-1, 11}).

2011-Oct-20

DD'OOOOHHHHHHHHHHHHH.... -0 and 0 are messing up chord comparators and hashes.
Hence the sets of OPTTI are wrong. Hence the ChordSpaceGroup is wrong. But,
now I know what the problem is!

2011-Oct-24

"allOf" fails for OPT and OPTI because of fractional pitches. This must be
dealt with, but is not so serious right now.

More urgently, there is still a problem with OPI for 5 voices. I should
capture the offending chord and test it in solitary confinement. Well, here
it is:

TESTING CHORDS OF  5 VOICES...

pitches:  { -13.0000 -13.0000 -13.0000  -8.0000  -6.0000}
I:        {  13.0000  13.0000  13.0000   8.0000   6.0000}
eO:       {  -1.0000  -1.0000  -1.0000   4.0000   6.0000}  iseO:    false
eP:       { -13.0000 -13.0000 -13.0000  -8.0000  -6.0000}  iseP:    true
eT:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseT:    false
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
eI:       { -13.0000 -13.0000 -13.0000  -8.0000  -6.0000}  iseI:    true
eV:       { -13.0000 -13.0000 -13.0000  -8.0000  -6.0000}  iseV:    true
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
eOP:      {  -1.0000  -1.0000  -1.0000   4.0000   6.0000}  iseOP:   false
pcs:      {   4.0000   6.0000  11.0000  11.0000  11.0000}
eOPT:     {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseOPT:  false
eOPTT:    {  -2.0000  -2.0000  -2.0000   3.0000   5.0000}
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
eOPI:     {  -4.0000   1.0000   1.0000   1.0000   6.0000}  iseOPI:  false
eOPTI:    {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseOPTI: false
eOPTTI:   {  -2.0000  -2.0000  -2.0000   3.0000   5.0000}
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
layer:      -53.00
pitches:  {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}
I:        {   2.4000   2.4000   2.4000  -2.6000  -4.6000}
eO:       {  -2.4000  -2.4000   9.6000   2.6000   4.6000}  iseO:    true
eP:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseP:    true
eT:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseT:    true
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
eI:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseI:    true
eV:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseV:    true
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
eOP:      {  -2.4000  -2.4000   2.6000   4.6000   9.6000}  iseOP:   true
pcs:      {   2.6000   4.6000   9.6000   9.6000   9.6000}
eOPT:     {  -4.6000  -2.6000   2.4000   2.4000   2.4000}  iseOPT:  true
eOPTT:    {  -4.0000  -2.0000   3.0000   3.0000   3.0000}
          {   0.0000   2.0000   7.0000   7.0000   7.0000}
eOPI:     {  -2.6000   2.4000   2.4000   2.4000   7.4000}  iseOPI:  false
eOPTI:    {  -4.6000  -2.6000   2.4000   2.4000   2.4000}  iseOPTI: false
eOPTTI:   {  -4.0000  -2.0000   3.0000   3.0000   3.0000}
          {   0.0000   2.0000   7.0000   7.0000   7.0000}
layer:        0.00
========================================================================
FAILED: chord:eOPTI():iseOPTI() == true
========================================================================
pitches:  { -13.0000 -13.0000 -13.0000  -8.0000   6.0000}
I:        {  13.0000  13.0000  13.0000   8.0000  -6.0000}
eO:       {  -1.0000  -1.0000  -1.0000   4.0000   6.0000}  iseO:    false
eP:       { -13.0000 -13.0000 -13.0000  -8.0000   6.0000}  iseP:    true
eT:       {  -4.8000  -4.8000  -4.8000   0.2000  14.2000}  iseT:    false
          {   0.0000   0.0000   0.0000   5.0000  19.0000}
eI:       { -13.0000 -13.0000 -13.0000  -8.0000   6.0000}  iseI:    true
eV:       {  -8.0000   6.0000 -13.0000 -13.0000 -13.0000}  iseV:    false
          {   5.0000  19.0000   0.0000   0.0000   0.0000}
eOP:      {  -1.0000  -1.0000  -1.0000   4.0000   6.0000}  iseOP:   false
pcs:      {   4.0000   6.0000  11.0000  11.0000  11.0000}
eOPT:     {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseOPT:  false
eOPTT:    {  -2.0000  -2.0000  -2.0000   3.0000   5.0000}
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
eOPI:     {  -4.0000   1.0000   1.0000   1.0000   6.0000}  iseOPI:  false
eOPTI:    {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseOPTI: false
eOPTTI:   {  -2.0000  -2.0000  -2.0000   3.0000   5.0000}
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
layer:      -41.00
pitches:  {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}
I:        {   2.4000   2.4000   2.4000  -2.6000  -4.6000}
eO:       {  -2.4000  -2.4000   9.6000   2.6000   4.6000}  iseO:    true
eP:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseP:    true
eT:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseT:    true
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
eI:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseI:    true
eV:       {  -2.4000  -2.4000  -2.4000   2.6000   4.6000}  iseV:    true
          {   0.0000   0.0000   0.0000   5.0000   7.0000}
eOP:      {  -2.4000  -2.4000   2.6000   4.6000   9.6000}  iseOP:   true
pcs:      {   2.6000   4.6000   9.6000   9.6000   9.6000}
eOPT:     {  -4.6000  -2.6000   2.4000   2.4000   2.4000}  iseOPT:  true
eOPTT:    {  -4.0000  -2.0000   3.0000   3.0000   3.0000}
          {   0.0000   2.0000   7.0000   7.0000   7.0000}
eOPI:     {  -2.6000   2.4000   2.4000   2.4000   7.4000}  iseOPI:  false
eOPTI:    {  -4.6000  -2.6000   2.4000   2.4000   2.4000}  iseOPTI: false
eOPTTI:   {  -4.0000  -2.0000   3.0000   3.0000   3.0000}
          {   0.0000   2.0000   7.0000   7.0000   7.0000}
layer:        0.00
========================================================================
FAILED: chord:eOPTI():iseOPTI() == true
========================================================================
pitches:  { -13.0000 -13.0000 -13.0000  -7.0000  -7.0000}
I:        {  13.0000  13.0000  13.0000   7.0000   7.0000}
eO:       {  -1.0000  -1.0000  -1.0000   5.0000   5.0000}  iseO:    false
eP:       { -13.0000 -13.0000 -13.0000  -7.0000  -7.0000}  iseP:    true
eT:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseT:    false
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
eI:       { -13.0000 -13.0000 -13.0000  -7.0000  -7.0000}  iseI:    true
eV:       { -13.0000 -13.0000 -13.0000  -7.0000  -7.0000}  iseV:    true
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
eOP:      {  -1.0000  -1.0000  -1.0000   5.0000   5.0000}  iseOP:   false
pcs:      {   5.0000   5.0000  11.0000  11.0000  11.0000}
eOPT:     {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseOPT:  false
eOPTT:    {  -2.0000  -2.0000  -2.0000   4.0000   4.0000}
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
eOPI:     {  -5.0000   1.0000   1.0000   1.0000   7.0000}  iseOPI:  false
eOPTI:    {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseOPTI: false
eOPTTI:   {  -2.0000  -2.0000  -2.0000   4.0000   4.0000}
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
layer:      -53.00
pitches:  {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}
I:        {   2.4000   2.4000   2.4000  -3.6000  -3.6000}
eO:       {  -2.4000  -2.4000   9.6000   3.6000   3.6000}  iseO:    true
eP:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseP:    true
eT:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseT:    true
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
eI:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseI:    true
eV:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseV:    true
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
eOP:      {  -2.4000  -2.4000   3.6000   3.6000   9.6000}  iseOP:   true
pcs:      {   3.6000   3.6000   9.6000   9.6000   9.6000}
eOPT:     {  -3.6000  -3.6000   2.4000   2.4000   2.4000}  iseOPT:  true
eOPTT:    {  -3.0000  -3.0000   3.0000   3.0000   3.0000}
          {   0.0000   0.0000   6.0000   6.0000   6.0000}
eOPI:     {  -3.6000   2.4000   2.4000   2.4000   8.4000}  iseOPI:  false
eOPTI:    {  -3.6000  -3.6000   2.4000   2.4000   2.4000}  iseOPTI: false
eOPTTI:   {  -3.0000  -3.0000   3.0000   3.0000   3.0000}
          {   0.0000   0.0000   6.0000   6.0000   6.0000}
layer:        0.00
========================================================================
FAILED: chord:eOPTI():iseOPTI() == true
========================================================================
pitches:  { -13.0000 -13.0000 -13.0000  -7.0000   5.0000}
I:        {  13.0000  13.0000  13.0000   7.0000  -5.0000}
eO:       {  -1.0000  -1.0000  -1.0000   5.0000   5.0000}  iseO:    false
eP:       { -13.0000 -13.0000 -13.0000  -7.0000   5.0000}  iseP:    true
eT:       {  -4.8000  -4.8000  -4.8000   1.2000  13.2000}  iseT:    false
          {   0.0000   0.0000   0.0000   6.0000  18.0000}
eI:       { -13.0000 -13.0000 -13.0000  -7.0000   5.0000}  iseI:    true
eV:       { -13.0000 -13.0000  -7.0000   5.0000 -13.0000}  iseV:    false
          {   0.0000   0.0000   6.0000  18.0000   0.0000}
eOP:      {  -1.0000  -1.0000  -1.0000   5.0000   5.0000}  iseOP:   false
pcs:      {   5.0000   5.0000  11.0000  11.0000  11.0000}
eOPT:     {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseOPT:  false
eOPTT:    {  -2.0000  -2.0000  -2.0000   4.0000   4.0000}
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
eOPI:     {  -5.0000   1.0000   1.0000   1.0000   7.0000}  iseOPI:  false
eOPTI:    {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseOPTI: false
eOPTTI:   {  -2.0000  -2.0000  -2.0000   4.0000   4.0000}
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
layer:      -41.00
pitches:  {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}
I:        {   2.4000   2.4000   2.4000  -3.6000  -3.6000}
eO:       {  -2.4000  -2.4000   9.6000   3.6000   3.6000}  iseO:    true
eP:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseP:    true
eT:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseT:    true
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
eI:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseI:    true
eV:       {  -2.4000  -2.4000  -2.4000   3.6000   3.6000}  iseV:    true
          {   0.0000   0.0000   0.0000   6.0000   6.0000}
eOP:      {  -2.4000  -2.4000   3.6000   3.6000   9.6000}  iseOP:   true
pcs:      {   3.6000   3.6000   9.6000   9.6000   9.6000}
eOPT:     {  -3.6000  -3.6000   2.4000   2.4000   2.4000}  iseOPT:  true
eOPTT:    {  -3.0000  -3.0000   3.0000   3.0000   3.0000}
          {   0.0000   0.0000   6.0000   6.0000   6.0000}
eOPI:     {  -3.6000   2.4000   2.4000   2.4000   8.4000}  iseOPI:  false
eOPTI:    {  -3.6000  -3.6000   2.4000   2.4000   2.4000}  iseOPTI: false
eOPTTI:   {  -3.0000  -3.0000   3.0000   3.0000   3.0000}
          {   0.0000   0.0000   6.0000   6.0000   6.0000}
layer:        0.00
========================================================================
FAILED: chord:eOPTI():iseOPTI() == true
========================================================================

It looks like the real culprit is this eV thing.

2011-Oct-25

Regarding eV, it's tricky because to start with a voicing and permute doesn't
give the same permutations as starting with a chord and permuting. So the
permutations have always to be in the same order. This may be my whole problem
all along.

I think eV may be the first permutation that is the closest to the unison
diagonal.

2011-Oct-27

It's official:

From:   5       0       0       0
        {  -3.0000   0.0000   4.0000}
fromChord: chord:       {  -3.0000   0.0000   4.0000}   true
fromChord: op:          {  -3.0000   0.0000   4.0000}
fromChord: optt:        {  -3.0000   0.0000   4.0000}
fromChord: optt_t:      {  -3.0000   0.0000   4.0000}   0
equals
fromChord: optti:       {  -3.0000   0.0000   4.0000}   -3,1.6653345369377e-016,4
fromChord: voicing:     {   0.0000   0.0000   0.0000}   0
fromChord:              nil     0       0       0
To:     nil     0       0       0
c:\utah\opt\Csound\bin\luajit.exe: .\ChordSpace.lua:2477: attempt to perform arithmetic on local 'P' (a nil value)
stack traceback:
        .\ChordSpace.lua:2477: in function 'toChord'
        ChordSpaceTest.lua:205: in main chunk
        [C]: ?

Hashing will need clamping. That might need a global g.

2013-Jun-22

Conforming this code to ChordSpace.cpp from CsoundAC, which appears to pass
more tests.

TODO:

--  Redo basic unit tests to ensure nothing has been broken.

--  Compute and save a chord space group file if the requested group does not
    exist; always load a chord space group from a file. This saves simply
    oodles of time.

--  Display the fundamental domains in the viewer much more clearly.

--  Display various temperament systems to see how harmony might work with
    voiceleading around the central diagonal. Alternatively, set up columns
    or lattices of chords that are in interesting relations, and see how they
    sound and work together.

--  Implement Rachel Hall, "Linear Contextual Transformations," 2009,
    which seems to further extend the Generalized Contextual Group using
    affine transformations in chord space, and Maxx Cho, "The Voice-Leading
    Automorphism and Riemannian Operators," 2009, which may show that tonality
    arises from a voice-leading automorphism in the Riemannian group.

--  Implement various scales found in 20th and 21st century harmony
    along with 'splitting' and 'merging' operations.
]]

ChordSpace.help()

-- Returns n!
-- EQUIVALENT
function ChordSpace.factorial (n)
  if n == 0 then
    return 1
  else
    return n * ChordSpace.factorial(n - 1)
  end
end

-- For taking numerical errors into account.
-- EQUIVALENT
ChordSpace.EPSILON = 1
local epsilonFactor = 1000

-- EQUIVALENT
while true do
    ChordSpace.EPSILON = ChordSpace.EPSILON / 2
    local nextEpsilon = ChordSpace.EPSILON / 2
    local onePlusNextEpsilon = 1 + nextEpsilon
    if onePlusNextEpsilon == 1 then
        print(string.format('ChordSpace.EPSILON: %g', ChordSpace.EPSILON))
        break
    end
end
-- EQUIVALENT
function ChordSpace.eq_epsilon(a, b, factor)
    factor = factor or epsilonFactor
    if (math.abs(a - b) < (ChordSpace.EPSILON * factor)) then
        return true
    end
    return false
end
-- EQUIVALENT
function ChordSpace.gt_epsilon(a, b, factor)
    factor = factor or epsilonFactor
    local eq = ChordSpace.eq_epsilon(a, b, factor)
    if eq then
        return false
    end
    if a > b then
        return true
    end
    return false
end
-- EQUIVALENT
function ChordSpace.lt_epsilon(a, b, factor)
    factor = factor or epsilonFactor
    local eq = ChordSpace.eq_epsilon(a, b, factor)
    if eq then
        return false
    end
    if a < b then
        return true
    end
    return false
end
-- EQUIVALENT
function ChordSpace.ge_epsilon(a, b, factor)
    factor = factor or epsilonFactor
    local eq = ChordSpace.eq_epsilon(a, b, factor)
    if eq then
        return true
    end
    if a > b then
        return true
    end
    return false
end
-- EQUIVALENT
function ChordSpace.le_epsilon(a, b, factor)
    factor = factor or epsilonFactor
    local eq = ChordSpace.eq_epsilon(a, b, factor)
    if eq then
        return true
    end
    if a < b then
        return true
    end
    return false
end

-- The size of the octave, defined to be consistent with
-- 12 tone equal temperament and MIDI.
-- EQUIVALENT
ChordSpace.OCTAVE = 12

-- Middle C.
-- EQUIVALENT
ChordSpace.MIDDLE_C = 60
-- EQUIVALENT
ChordSpace.C4 = ChordSpace.MIDDLE_C

-- Returns the pitch transposed by semitones, which may be any scalar.
-- NOTE: Does NOT return the result under any equivalence class.
-- EQUIVALENT
local function T(pitch, semitones)
    return pitch + semitones
end

-- Returns the pitch reflected in the center, which may be any pitch.
-- NOTE: Does NOT return the result under any equivalence class.
-- EQUIVALENT
local function I(pitch, center)
    center = center or 0
    return center - pitch
end

-- Returns the Euclidean distance between chords a and b,
-- which must have the same number of voices.
-- EQUIVALENT
function ChordSpace.euclidean(a, b)
    local sumOfSquaredDifferences = 0
    for voice = 1, #a do
        sumOfSquaredDifferences = sumOfSquaredDifferences + math.pow((a[voice] - b[voice]), 2)
    end
    return math.sqrt(sumOfSquaredDifferences)
end

-- Chords represent simultaneously sounding pitches.
-- The pitches are represented as semitones with 0 at the origin
-- and middle C as 60.
-- EQUIVALENT
Chord = {}

-- Returns a new chord object with no voices.
-- EQUIVALENT
function Chord:new(o)
    o = o or {duration = {}, channel = {}, velocity = {}, pan = {}}
    if not o.duration then
        o.duration = {}
    end
    if not o.channel then
        o.channel = {}
    end
    if not o.velocity then
        o.velocity = {}
    end
    if not o.pan then
        o.pan = {}
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

-- Returns a string representation of the chord.
-- Quadratic complexity, but short enough not to matter.
-- EQUIVALENT
function Chord:__tostring()
    local buffer = '{'
    for voice = 1, #self do
        buffer = buffer .. string.format('%12.7f', self[voice])
    end
    buffer = buffer .. '}'
    return buffer
end

-- Resizes a chord to the specified number of voices.
-- Existing voices are not changed. Extra voices are removed.
-- New voices are initialized to 0.
-- NONEQUIVALENT In Eigen, data are lost.
function Chord:resize(voices)
    while #self < voices do
        table.insert(self, 0)
        table.insert(self.duration, 0)
        table.insert(self.channel, 0)
        table.insert(self.velocity, 0)
        table.insert(self.pan, 0)
    end
    while #self > voices do
        table.remove(self)
        table.remove(self.duration)
        table.remove(self.channel)
        table.remove(self.velocity)
        table.remove(self.pan)
    end
end
-- EQUIVALENT
function Chord:setDuration(value)
    for i = 1, #self do
        self.duration[i] = value
    end
end
-- EQUIVALENT
function Chord:getDuration(voice)
    voice = voice or 1
    return self.duration[voice]
end
-- EQUIVALENT
function Chord:setChannel(value)
    for i = 1, #self do
        self.channel[i] = value
    end
end
-- EQUIVALENT
function Chord:getChannel(voice)
    voice = voice or 1
    return self.channel[voice]
end
-- EQUIVALENT
function Chord:setVelocity(value)
    for i = 1, #self do
        self.velocity[i] = value
    end
end
-- EQUIVALENT
function Chord:getVelocity(voice)
    voice = voice or 1
    return self.velocity[voice]
end
-- EQUIVALENT
function Chord:setPan(value)
    for i = 1, #self do
        self.pan[i] = value
    end
end
-- EQUIVALENT
function Chord:getPan(voice)
    voice = voice or 1
    return self.pan[voice]
end
-- EQUIVALENT
function Chord:count(pitch)
    local n = 0
    for voice = 1, #self do
        if self[voice] == pitch then
            n = n + 1
        end
    end
    return n
end

function ChordSpace.invariantPcs(a_, b_) 
    local a = a_:eOP()
    local b = b_:eOP()
    local count = 0
    for voice = 1, #a do
        local p = a[voice]
        if a:count(p) == b:count(p) then
            count = count + 1
        end
    end
    return count
end

-- Redefines the metamethod to implement value semantics
-- for ==, for the pitches in this only.
-- EQUIVALENT
function Chord:__eq(other)
    if not (#self == #other) then
        return false
    end
    for voice = 1, #self do
        --if not (self[voice] == other[voice]) then
        if not (ChordSpace.eq_epsilon(self[voice], other[voice])) then
            return false
        end
    end
    return true
end
-- NO EQUIVALENT
function Chord:__eq_epsilon(other)
    if not (#self == #other) then
        return false
    end
    for voice = 1, #self do
        if not (ChordSpace.eq_epsilon(self[voice], other[voice])) then
            return false
        end
    end
    return true
end

-- This hash function is used to give chords value semantics for sets.
-- NO EQUIVALENT Not needed in C++, which already has value semantics.
function Chord:__hash()
    local buffer = ''
    local comma = ','
    for voice = 1, #self do
        local digit = tostring(self[voice])
        if voice == 1 then
            buffer = buffer .. digit
        else
            buffer = buffer .. comma .. digit
        end
    end
    return buffer
end

-- Redefines the metamethod to implement value semantics
-- for <, for the pitches in this only.
-- EQUIVALENT
function Chord:__lt(other)
    local voices = math.min(#self, #other)
    for voice = 1, voices do
        if ChordSpace.lt_epsilon(self[voice], other[voice]) then
            return true
        end
        if ChordSpace.gt_epsilon(self[voice], other[voice]) then
            return false
        end
    end
    if #self < #other then
        return true
    end
    return false
end
-- EQUIVALENT
function Chord:__le(other)
    if self:__eq(other) then
        return true
    end
    return self:__lt(other)
end

-- Returns whether or not the chord contains the pitch.
-- EQUIVALENT
function Chord:contains(pitch)
    for voice, pitch_ in ipairs(self) do
        if pitch_ == pitch then
            return true
        end
    end
    return false
end

-- Returns the lowest pitch in the chord,
-- and also its voice index.
-- EQUIVALENT
function Chord:min()
    local lowestVoice = 1
    local lowestPitch = self[lowestVoice]
    for voice = 2, #self do
        if self[voice] < lowestPitch then
            lowestPitch = self[voice]
            lowestVoice = voice
        end
    end
    return lowestPitch, lowestVoice
end

-- Returns the minimum interval in the chord.
-- EQUIVALENT
function Chord:minimumInterval()
    local minimumInterval_ = math.abs(self[1] - self[2])
    for v1 = 1, #self do
        for v2 = 1, #self do
            if t (v1 == v2) then
                local interval = math.abs(self[v1] - self[v2])
                if interval < minimumInterval_ then
                    minimumInterval_ = interval
                end
            end
        end
    end
    return minimumInterval_
end

-- Returns the highest pitch in the chord,
-- and also its voice index.
-- EQUIVALENT
function Chord:max()
    local highestVoice = 1
    local highestPitch = self[highestVoice]
    for voice = 2, #self do
        if self[voice] > highestPitch then
            highestPitch = self[voice]
            highestVoice = voice
        end
    end
    return highestPitch, highestVoice
end

-- Returns the maximum interval in the chord.
-- EQUIVALENT
function Chord:maximumInterval()
    local maximumInterval_ = math.abs(self[1] - self[2])
    for v1 = 1, #self do
        for v2 = 1, #self do
            if not (v1 == v2) then
                local interval = math.abs(self[v1] - self[v2])
                if interval > maximumInterval_ then
                    maximumInterval_ = interval
                end
            end
        end
    end
    return maximumInterval_
end

-- Returns a new chord whose pitches are the floors of this chord's pitches.
-- EQUIVALENT
function Chord:floor()
    local chord = self:clone()
    for voice = 1, #self do
        chord[voice] = math.floor(self[voice])
    end
    return chord
end

-- Returns a new chord whose pitches are the ceilings of this chord's pitches.
-- EQUIVALENT
function Chord:ceil()
    local chord = self:clone()
    for voice = 1, #self do
        chord[voice] = math.ceil(self[voice])
    end
    return chord
end

-- Returns a value copy of the chord.
-- NO EQUIVALENT C++ has a copy constructor for this.
function Chord:clone()
    local clone_ = Chord:new()
    for voice, pitch in ipairs(self) do
        clone_[voice] = pitch
    end
    for voice, value in ipairs(self.duration) do
        clone_.duration[voice] = value
    end
    for voice, value in ipairs(self.channel) do
        clone_.channel[voice] = value
    end
    for voice, value in ipairs(self.velocity) do
        clone_.velocity[voice] = value
    end
    for voice, value in ipairs(self.pan) do
        clone_.pan[voice] = value
    end
    return clone_
end

-- Returns the origin of the chord's space.
-- EQUIVALENT
function Chord:origin()
    local clone_ = self:clone()
    for voice = 1, #clone_ do
        clone_[voice] = 0
    end
    return clone_
end
-- EQUIVALENT
function Chord:distanceToOrigin()
    local origin = self:origin()
    return ChordSpace.euclidean(self, origin)
end

-- Returns the Euclidean distance from this chord
-- to the unison diagonal of its chord space.
-- EQUIVALENT
function Chord:distanceToUnisonDiagonal()
    local unison = self:origin()
    local pitch = self:layer() / #self
    for voice = 1, #self do
        unison[voice] = pitch
    end
    return ChordSpace.euclidean(self, unison)
end

-- Returns the maximally even chord in the chord's space,
-- e.g. the augmented triad for 3 dimensions.
-- EQUIVALENT
function Chord:maximallyEven()
    local clone_ = self:clone()
    local g = ChordSpace.OCTAVE / #clone_
    for i = 1, #clone_ do
        clone_[i] = (i - 1) * g
    end
    return clone_
end

-- Returns the sum of the pitches in the chord.
-- EQUIVALENT
function Chord:layer()
    local s = 0
    for voice, pitch in ipairs(self) do
        s = s + pitch
    end
    return s
end

-- Transposes the chord by the indicated interval (may be a fraction).
-- NOTE: Does NOT return the result under any equivalence class.
-- EQUIVALENT
function Chord:T(interval)
    local clone_ = self:clone()
    for voice = 1, #clone_ do
        clone_[voice] = T(clone_[voice], interval)
    end
    return clone_
end

-- Inverts the chord by another chord that is on the unison diagonal, by
-- default the origin.
-- NOTE: Does NOT return the result under any equivalence class.
-- EQUIVALENT
function Chord:I(center)
    center = center or 0
    local inverse = self:clone()
    for voice = 1, #inverse do
        inverse[voice] = I(self[voice], center)
    end
    return inverse
end

-- Returns the remainder of the dividend divided by the divisor,
-- according to the Euclidean definition.
-- EQUIVALENT
function ChordSpace.modulo(dividend, divisor)
   local quotient = 0.0
    if divisor < 0.0 then
        quotient = math.ceil(dividend / divisor)
    end
    if divisor > 0.0 then
        quotient = math.floor(dividend / divisor)
    end
    local remainder = dividend - (quotient * divisor)
    return remainder
end

-- Returns the equivalent of the pitch under pitch-class equivalence, i.e.
-- the pitch is in the interval [0, OCTAVE).

function ChordSpace.epc(pitch)
    --[[
    --while pitch < 0 do
    while ChordSpace.lt_epsilon(pitch, 0) do
        pitch = pitch + ChordSpace.OCTAVE
    end
    --while pitch >= ChordSpace.OCTAVE do
    while ChordSpace.ge_epsilon(pitch, ChordSpace.OCTAVE) do
        pitch = pitch - ChordSpace.OCTAVE
    end
    return pitch
    ]]
    local pc = ChordSpace.modulo(pitch, ChordSpace.OCTAVE)
    return pc
end

-- Returns whether the chord is within the fundamental domain of
-- pitch-class equivalence, i.e. is a pitch-class set.
-- EQUIVALENT
function Chord:isepcs()
    for voice = 1, #chord do
        --if not (self[voice] == ChordSpace.epc(chord[voice])) then
        if not ChordSpace.eq_epsilon(self[voice], ChordSpace.epc(chord[voice])) then
            return false
        end
    end
    return true
end
-- EQUIVALENT to normalize<EQUIVALENCE_RELATION_r>
function Chord:er(range)
    local chord = self:clone()
    for voice = 1, #chord do
        chord[voice] = ChordSpace.modulo(chord[voice], range)
    end
    return chord
end

-- Returns the equivalent of the chord under pitch-class equivalence,
-- i.e. the pitch-class set of the chord.
-- EQUIVALENT to normalize<EQUIVALENCE_RELATION_r> for octave.
function Chord:epcs()
    return self:er(ChordSpace.OCTAVE)
end

-- Returns whether the chord is within the fundamental domain of
-- transposition to 0.
-- EQUIVALENT
function Chord:iset()
    local et = self:et()
    if not (et == self) then
        return false
    end
    return true
end

-- Returns the equivalent of the chord within the fundamental domain of
-- transposition to 0.
-- EQUIVALENT
function Chord:et()
    local min_ = self:min()
    return self:T(-min_)
end

-- Returns whether the chord is within the representative fundamental domain
-- of the indicated range equivalence.
-- EQUIVALENT
function Chord:iseR(range)
    --[[ GVLS:
    local max_ = self:max()
    local min_ = self:min()
    if not (max_ <= (min_ + range)) then
        return false
    end
    local layer_ = self:layer()
    if not ((0 <= layer_) and (layer_ <= range)) then
        return false
    end
    return true
    --]]
    --[[ GVLS modified:
    local max_ = self:max()
    local min_ = self:min()
    if not ChordSpace.le_epsilon(max_, (min_ + range)) then
        return false
    end
    local layer_ = self:layer()
    if not (ChordSpace.le_epsilon(0, layer_) and ChordSpace.le_epsilon(layer_, range)) then
        return false
    end
    return true
    --]]
    ----[[ MKG several equivalents of boundary points in domain:
    local max_ = self:max()
    local min_ = self:min()
    if not ChordSpace.le_epsilon(max_, (min_ + range)) then
        return false
    end
    local layer_ = self:layer()
    if not ChordSpace.le_epsilon(0, layer_) then
        return false
    end
    if not ChordSpace.le_epsilon(layer_, range) then
        return false
    end
    return true
    --]]
end

-- Returns whether the chord is within the representative fundamental domain
-- of octave equivalence.
-- EQUIVALENT
function Chord:iseO()
    return self:iseR(ChordSpace.OCTAVE)
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of a range equivalence.
-- EQUIVALENT
function Chord:eR(range)
    --local normal = self:clone()
    --if chord:iseR(range) then
    --    return chord
    --end
    -- The clue here is that at least one voice must be >= 0,
    -- but no voice can be > range.
    -- First, move all pitches inside the interval [0, OCTAVE),
    -- which is not the same as the fundamental domain.
    -- EQUIVALENT to normalize<EQUIVALENCE_RELATION_r> for the octave.
    local normal = self:er(range)
    -- Then, reflect voices that are outside of the fundamental domain
    -- back into it, which will revoice the chord, i.e.
    -- the sum of pitches is in [0, OCTAVE].
    --while chord:layer() > range do
    while not ChordSpace.lt_epsilon(normal:layer(), range) do
        local maximumPitch, maximumVoice = normal:max()
        -- Because no voice is above the range,
        -- any voices that need to be revoiced will now be negative.
        -- chord[maximumVoice] = maximumPitch - ChordSpace.OCTAVE
        normal[maximumVoice] = maximumPitch - range
    end
    return normal
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of octave equivalence.
-- EQUIVALENT
function Chord:eO()
    return self:eR(ChordSpace.OCTAVE)
end

-- Returns whether the chord is within the representative fundamental domain
-- of permutational equivalence.
-- EQUIVALENT
function Chord:iseP()
    for voice = 2, #self do
        --if not (self[voice - 1] <= self[voice]) then
        if not ChordSpace.le_epsilon(self[voice - 1], self[voice]) then
            return false
        end
    end
    return true
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of permutational equivalence.
-- EQUIVALENT if < is le_epsilon.
function Chord:eP()
    clone_ = self:clone()
    table.sort(clone_)
    return clone_
end

-- Returns whether the chord is within the representative fundamental domain
-- of transpositional equivalence.
-- EQUIVALENT
function Chord:iseT()
    ----[[ GVLS:
    local layer_ = self:layer()
    if not ChordSpace.eq_epsilon(layer_, 0) then
        return false
    end
    return true
    --]]
    --[[ MKG:
    g = g or 1
    local ep = self:eP()
    if not (ep == ep:eT(g):eP()) then
        return false
    end
    return true
    --]]
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of transpositonal equivalence.
-- EQUIVALENT
function Chord:eT()
    ----[[ GVLS:
    local layer_ = self:layer()
    local sumPerVoice = layer_ / #self
    return self:T(-sumPerVoice)
    --]]
    --[[ MKG:
    g = g or 1
    local iterator = self
    -- Transpose down to layer 0 or just below.
    while iterator:layer() > 0 do
        iterator = iterator:T(-g)
    end
    -- Transpose up to layer 0 or just above.
    while iterator:layer() < 0 do
        iterator = iterator:T(g)
    end
    return iterator
    --]]
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of transpositonal equivalence and the equal temperament generated
-- by g. I.e., returns the chord transposed such that its layer is 0 or, under
-- transposition, the positive layer closest to 0. NOTE: Does NOT return the
-- result under any other equivalence class.
-- EQUIVALENT
function Chord:eTT(g)
    g = g or 1
    --[[
    local et = self:eT()
    local transposition = math.ceil(et[1]) - et[1]
    local ett = et:T(transposition)
    return ett
    ]]
    local normal = self:eT()
    local ng = math.ceil(normal[1] / g)
    local transposition = (ng * g) - normal[1]
    normal = normal:T(transposition)
    return normal
end

-- Returns whether the chord is within the representative fundamental domain
-- of translational equivalence and the equal temperament generated by g.
-- EQUIVALENT
function Chord:iseTT(g)
    g = g or 1
    local ep = self:eP()
    if not (ep == ep:eTT(g)) then
        return false
    end
    return true
end

-- Returns whether the chord is within the representative fundamental domain
-- of inversional equivalence.
-- EQUIVALENT
function Chord:iseI(inverse)
    --[[ GLVS:
    local chord = self:clone()
    local lowerVoice = 2
    local upperVoice = #chord
    while lowerVoice < upperVoice do
        -- GVLS: tests only 1 interval: x[2] - x[1] <= x[#x] - x[#x - 1]
        local lowerInterval = chord[lowerVoice] - chord[lowerVoice - 1]
        local upperInterval = chord[upperVoice] - chord[upperVoice - 1]
        if lowerInterval < upperInterval then
            return true
        end
        if lowerInterval > upperInterval then
            return false
        end
        lowerVoice = lowerVoice + 1
        upperVoice = upperVoice - 1
    end
    return true
    ]]
    --[[ MKG:
    inverse = inverse or self:I()
    if not self:__le(inverse) then
        return false
    end
    return true
    ]]
    --[[ MKG:
    inverse = self:I()
    if not self:__le(inverse) then
        return false
    end
    return true
    ]]
    local lowerVoice = 2
    local upperVoice = #self
    while lowerVoice < upperVoice do
        local lowerInterval = self[lowerVoice] - self[lowerVoice - 1]
        local upperInterval = self[upperVoice] - self[upperVoice - 1]
        if ChordSpace.lt_epsilon(lowerInterval, upperInterval) then
            return true
        end
        if ChordSpace.gt_epsilon(lowerInterval, upperInterval) then
            return false
        end
        lowerVoice = lowerVoice + 1
        upperVoice = upperVoice - 1
    end
    return true
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of inversional equivalence.
-- EQUIVALENT
function Chord:eI()
    if self:iseI() then
        return self:clone()
    end
    return self:I()
end

-- Returns whether the chord is within the representative fundamental domain
-- of range and permutational equivalence.
-- EQUIVALENT
function Chord:iseRP(range)
    --[[ GVLS:
    for voice = 2, #self do
        if not (self[voice - 1] <= self[voice]) then
            return false
        end
    end
    if not (self[#self] <= (self[1] + range)) then
        return false
    end
    local layer_ = self:layer()
    if not ((0 <= layer_) and (layer_ <= range)) then
        return false
    end
    return true
    --]]
    ----[[ MKG:
    if not self:iseP() then
        return false
    end
    if not self:iseR(range) then
        return false
    end
    return true
    --]]
end
-- Returns whether the chord is within the representative fundamental domain
-- of octave and permutational equivalence.
-- EQUIVALENT
function Chord:iseOP()
    return self:iseRP(ChordSpace.OCTAVE)
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of range and permutational equivalence.
-- EQUIVALENT
function Chord:eRP(range)
    return self:eR(range):eP()
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of octave and permutational equivalence.
-- EQUIVALENT
function Chord:eOP()
    return self:eRP(ChordSpace.OCTAVE)
end

-- Returns a copy of the chord cyclically permuted by a stride, by default 1.
-- The direction of rotation is the same as musicians' first inversion, second
-- inversion, and so on.
-- EQUIVALENT I THINK
function Chord:cycle(stride)
    stride = stride or 1
    local permuted = self:clone()
    if stride < 0 then
        for i = 1, stride do
            local tail = table.remove(permuted)
            table.insert(permuted, 1, tail)
        end
        return permuted
    end
    if stride > 0 then
        for i = 1, math.abs(stride) do
            local head = table.remove(permuted, 1)
            table.insert(permuted, head)
        end
    end
    return permuted
end

-- Returns the permutations of the pitches in a chord. The permutations from
-- each particular permutation are always returned in the same order.
-- EQUIVALENT IF CYCLE IS
function Chord:permutations()
    local permutation = self:clone()
    local permutations_ = {}
    permutations_[1] = permutation
    for i = 2, #self do
        permutation = permutation:cycle()
        permutations_[i] = permutation
    end
    table.sort(permutations_)
    return permutations_
end

-- Returns whether the chord is within the representative fundamental domain
-- of voicing equivalence.
-- EQUIVALENT (REIMPLEMENTED TO MATCH)
function Chord:iseV(range)
    --[[
    local eV = self:eV()
    --print(string.format('chord: %s  eV: %s', tostring(self), tostring(eV)))
    if not (self == eV) then
        return false
    end
    return true
    ]]
    range = range or ChordSpace.OCTAVE
    local outer = self[1] + range - self[#self]
    local isNormal = true
    for voice = 1, #self - 1 do
        local inner = self[voice + 1] - self[voice]
        if not ChordSpace.ge_epsilon(outer, inner) then
            isNormal = false
        end
    end
    return isNormal
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of voicing equivalence.
-- EQUIVALENT (REIMPLENTED)
function Chord:eV(range)
    range = range or ChordSpace.OCTAVE
    local permutations = self:permutations()
    for i = 1, #permutations do
        local permutation = permutations[i]
        if permutation:iseV(range) then
            return permutation
        end
    end
    --[[
    for index, voicing in ipairs(self:permutations()) do
        local wraparound = voicing[1] + ChordSpace.OCTAVE - voicing[#voicing]
        local iseV_ = true
        for voice = 1, #voicing - 1 do
            local inner = voicing[voice + 1] - voicing[voice]
            if not ChordSpace.ge_epsilon(wraparound, inner) then
            --if inner > wraparound then
                iseV_ = false
            end
        end
        if iseV_ then
            return voicing
        end
    end
    --]]
    --[[
    local voicings = self:permutations()
    local distancesForIndexes = {}
    for i = 1, #voicings do
        distancesForIndexes[i] = voicings[i]:distanceToUnisonDiagonal()
    end
    local minimumDistanceToUnisonDiagonal = distancesForIndexes[1]
    for i = 2, #voicings do
        if distancesForIndexes[i] < minimumDistanceToUnisonDiagonal then
            minimumDistanceToUnisonDiagonal = distancesForIndexes[i]
        end
    end
    for i = 1, #voicings do
         if distancesForIndexes[i] == minimumDistanceToUnisonDiagonal then
            return voicings[i]
        end
    end
    --]]
end

-- Returns whether the chord is within the representative fundamental domain
-- of range, permutational, and transpositional equivalence.
-- EQUIVALENT
function Chord:iseRPT(range)
    --[[ GVLS:
    -- GVLS: if not (self[#self] <= self[1] + ChordSpace.OCTAVE) then
    if not (ChordSpace.le_epsilon(self[#self], (self[1] + range))) then
        return false
    end
    local layer_ = self:layer()
    -- GVLS: if not (layer_ == 0) then
    if not ChordSpace.eq_epsilon(layer_, 0) then
        return false
    end
    if #self < 2 then
        return true
    end
    local wraparound = self[1] + range - self[#self]
    for voice = 1, #self - 1  do
        local inner = self[voice + 1] - self[voice]
        if not ChordSpace.le_epsilon(wraparound, inner) then
            return false
        end
    end
    return true
    --]]
    ----[[ MKG:
    if not self:iseR(range) then
        return false
    end
    if not self:iseP() then
        return false
    end
    if not self:iseT() then
        return false
    end
    if not self:iseV() then
        return false
    end
    return true
    --]]
end
-- EQUIVALENT
function Chord:iseRPTT(range)
    if not self:iseR(range) then
        return false
    end
    if not self:iseP() then
        return false
    end
    if not self:iseTT() then
        return false
    end
    if not self:iseV() then
        return false
    end
    return true
end

-- Returns whether the chord is within the representative fundamental domain
-- of octave, permutational, and transpositional equivalence.
-- EQUIVALENT
function Chord:iseOPT()
    return self:iseRPT(ChordSpace.OCTAVE)
end
-- EQUIVALENT
function Chord:iseOPTT()
    return self:iseRPTT(ChordSpace.OCTAVE)
end

-- Returns a copy of the chord 'inverted' in the musician's sense,
-- i.e. revoiced by cyclically permuting the chord and
-- adding (or subtracting) an octave to the highest (or lowest) voice.
-- The revoicing will move the chord up or down in pitch.
-- A positive direction is the same as a musician's first inversion,
-- second inversion, etc.
-- EQUIVALENT
function Chord:v(direction)
    direction = direction or 1
    local chord = self:clone()
    while direction > 0 do
        chord = chord:cycle(1)
        chord[#chord] = chord[#chord] + ChordSpace.OCTAVE
        direction = direction - 1
    end
    while direction < 0 do
        chord = chord:cycle(-1)
        chord[1] = chord[1] - ChordSpace.OCTAVE
        direction = direction + 1
    end
    return chord
end

-- Returns all the 'inversions' (in the musician's sense)
-- or octavewise revoicings of the chord.
-- EQUIVALENT
function Chord:voicings()
    local chord = self:clone()
    local voicings = {}
    voicings[1] = chord
    for i = 2, #self do
        chord = chord:v()
        voicings[i] = chord
    end
    return voicings
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of range, permutational, and transpositional equivalence; the same
-- as set-class type, or chord type.
-- EQUIVALENT I THINK (g?)
function Chord:eRPT(range)
    --[[
    local erp = self:eRP(range)
    local voicings_ = erp:voicings()
    for voice = 1, #voicings_ do
        local voicing = voicings_[voice]:eT()
        if voicing:iseV() then
            return voicing
        end
    end
    print('ERROR: Chord:eRPT() should not come here: ' .. tostring(self))
    --]]
    ----[[
    local erp = self:eRP(range)
    local voicings_ = erp:voicings()
    for voice = 1, #voicings_ do
        local voicing = voicings_[voice]
        if voicing:iseV() then
            return voicing:eT()
        end
    end
    print('ERROR: Chord:eRPT() should not come here: ' .. tostring(self))
    --]]
end
-- EQUIVALENT
function Chord:eRPTT(range)
    local erp = self:eRP(range)
    local voicings_ = erp:voicings()
    for voice = 1, #voicings_ do
        local voicing = voicings_[voice]:eTT()
        if voicing:iseV() then
            return voicing
        end
    end
    print('ERROR: Chord:eRPTT() should not come here: ' .. tostring(self))
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of octave, permutational, and transpositional equivalence.
-- EQUIVALENT
function Chord:eOPT()
    return self:eRPT(ChordSpace.OCTAVE)
end
-- EQUIVALENT
function Chord:eOPTT()
    return self:eRPTT(ChordSpace.OCTAVE)
end

-- Returns whether the chord is within the representative fundamental domain
-- of range, permutational, and inversional equivalence.
-- EQUIVALENT (REIMPLEMENTED)
function Chord:iseRPI(range)
    --[[
    if not self:iseRP(range) then
        return false
    end
    local inverse = self:I():eRP(range)
    assert(inverse, 'Inverse is nil.')
    if not self:iseI(inverse) then
        return false
    end
    return true
    ]]
    if not self:iseRP(range) then
        return false
    end
    local inverse = self:I()
    local inverseRP = inverse:eRP(range)
    assert(inverse, 'Inverse is nil.')
    if self <= inverseRP then
        return true
    end
    return false
end

-- Returns whether the chord is within the representative fundamental domain
-- of octave, permutational, and inversional equivalence.
-- EQUIVALENT
function Chord:iseOPI()
    return self:iseRPI(ChordSpace.OCTAVE)
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of range, permutational, and inversional equivalence.
-- EQUIVALENT (REIMPLEMENTED)
function Chord:eRPI(range)
    --[[
    local erp = self:eRP(range)
    if erp:iseRPI(range) then
        return erp
    end
    return erp:I():eRP(range)
    ]]
    if self:iseRPI(range) then
        return self:clone()
    end
    local normalRP = self:eRP(range)
    local normalRPInverse = normalRP:I()
    local normalRPInverseRP = normalRPInverse:eRP(range)
    if normalRP <= normalRPInverseRP then
        return normalRP
    else
        return normalRPInverseRP
    end
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of octave, permutational, and inversional equivalence.
-- EQUIVALENT
function Chord:eOPI()
    return self:eRPI(ChordSpace.OCTAVE)
end

-- Returns whether the chord is within the representative fundamental domain
-- of range, permutational, transpositional, and inversional equivalence.
-- EQUIVALENT (REIMPLEMENTED)
function Chord:iseRPTI(range)
    --[[ GVLS:
    -- GVLS: if not (self[#self] <= self[1] + ChordSpace.OCTAVE) then
    if not ChordSpace.le_epsilon(self[#self], (self[1] + range)) then
        return false
    end
    local layer_ = self:layer()
    -- GVLS: if not (layer_ == 0) then
    if not ChordSpace.eq_epsilon(layer_, 0) then
        return false
    end
    if #self <= 2 then
        return true
    end
    local wraparound = self[1] + range - self[#self]
    for voice = 1, #self - 1  do
        local inner = self[voice + 1] - self[voice]
        if not ChordSpace.le_epsilon(wraparound, inner) then
            return false
        end
    end
    if not self:iseI() then
        return false
    end
    return true
    --]]
    --[[ MKG:
    if not self:iseRPT(range) then
        return false
    end
    if not self:iseI() then
        return false
    end
    return true
    --]]
    if not self:iseP() then
        return false
    end
    if not self:iseR(range) then
        return false
    end
    if not self:iseT() then
        return false
    end
    if not self:iseV(range) then
        return false
    end
    return true
end
-- EQUIVALENT (REIMPLEMENTED)
function Chord:iseRPTTI(range)
    --[[
    if not self:iseRPTT(range) then
        return false
    end
    if not self:iseI() then
        return false
    end
    return true
    ]]
    if not self:iseRPTT(range) then
        return false
    end
    local inverse = self:I()
    local normalRPTT = inverse:eRPTT(range)
    if self <= normalRPTT then
        return true
    end
    return false
end

-- Returns whether the chord is within the representative fundamental domain
-- of octave, permutational, transpositional, and inversional equivalence.
-- EQUIVALENT
function Chord:iseOPTI()
    return self:iseRPTI(ChordSpace.OCTAVE)
end
-- EQUIVALENT
function Chord:iseOPTTI()
    return self:iseRPTTI(ChordSpace.OCTAVE)
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of range, permutational, transpositional, and inversional
-- equivalence.
-- EQUIVALENT (REIMPLEMENTED)
function Chord:eRPTI(range)
    --[[
    local rpt = self:eRPT(range)
    if rpt:iseI() then
        return rpt
    end
    return rpt:I():eRPT(range)
    ]]
    local normalRPT = self:eRPT(range)
    if normalRPT:iseI() then
        return normalRPT
    else
        local normalI = normalRPT:eRPI(range)
        local normalRPT_ = normalI:eRPT(range)
        return normalRPT_
    end
end
-- EQUIVALENT (REIMPLEMENTED)
function Chord:eRPTTI(range)
    --[[
    local rpt = self:eRPTT(range)
    if rpt:iseRPTTI(range) then
        return rpt
    end
    return rpt:I():eRPTT(range)
    ]]
    local normalRPTT = self:eRPTT(range)
    local inverse = normalRPTT:I()
    local inverseNormalRPTT = inverse:eRPTT(range)
    if normalRPTT <= inverseNormalRPTT then
        return normalRPTT
    end
    return inverseNormalRPTT
end

-- Returns the equivalent of the chord within the representative fundamental
-- domain of range, permutational, transpositional, and inversional
-- equivalence.
-- EQUIVALENT
function Chord:eOPTI()
    return self:eRPTI(ChordSpace.OCTAVE)
end
-- EQUIVALENT
function Chord:eOPTTI()
    return self:eRPTTI(ChordSpace.OCTAVE)
end
-- EQUIVALENT
function Chord:name()
    local chordName = ChordSpace.namesForChords[self:__hash()]
    if chordName == nil then
        chordName = ''
    end
    return chordName
end

-- Returns a formatted string with information about the chord.

function Chord:information()
    local et = self:eT():et()
    local evt = self:eV():et()
    local eopt = self:eOPT():et()
    local epcs = self:epcs():eP()
    local eopti = self:eOPTI():et()
    local eOP = self:eOP()
    chordName = eOP:name()
    return string.format([[pitches:  %s  %s
I:        %s
eO:       %s  iseO:    %s
eP:       %s  iseP:    %s
eT:       %s  iseT:    %s
          %s
eI:       %s  iseI:    %s
eV:       %s  iseV:    %s
          %s
eOP:      %s  iseOP:   %s
pcs:      %s
eOPT:     %s  iseOPT:  %s
eOPTT:    %s
          %s
eOPI:     %s  iseOPI:  %s
eOPTI:    %s  iseOPTI: %s
eOPTTI:   %s
          %s
layer:      %6.2f]],
tostring(self), chordName,
tostring(self:I()),
tostring(self:eO()),    tostring(self:iseO()),
tostring(self:eP()),    tostring(self:iseP()),
tostring(self:eT()),    tostring(self:iseT()),
tostring(et),
tostring(self:eI()),    tostring(self:iseI()),
tostring(self:eV()),    tostring(self:iseV()),
tostring(evt),
tostring(self:eOP()),   tostring(self:iseOP()),
tostring(epcs),
tostring(self:eOPT()),  tostring(self:iseOPT()),
tostring(self:eOPTT()),
tostring(eopt),
tostring(self:eOPI()),  tostring(self:iseOPI()),
tostring(self:eOPTI()), tostring(self:iseOPTI()),
tostring(self:eOPTTI()),
tostring(eopti),
self:layer())
end

function ChordSpace.set(collection)
    local set_ = {}
    for key, value in pairs(collection) do
        set_[value:__hash()] = value
    end
    return set_
end

function ChordSpace.sortedSet(collection)
    local set_ = ChordSpace.set(collection)
    local sortedSet_ = {}
    for key, value in pairs(set_) do
        table.insert(sortedSet_, value)
    end
    table.sort(sortedSet_)
    return sortedSet_
end

function ChordSpace.zeroBasedSet(sortedSet)
    local zeroBasedSet = {}
    for index, value in pairs(sortedSet) do
        zeroBasedSet[index - 1] = value
    end
    return zeroBasedSet
end

function ChordSpace.setContains(setA, x)
    if setA[x:__hash()] == x then
        return true
    end
    return false
end

function ChordSpace.setInsert(setA, x)
    if not ChordSpace.setContains(setA, x) then
        setA[x:__hash()] = x
    end
end

function ChordSpace.sortedEquals(sortedA, sortedB)
    if not (#sortedA == #sortedB) then
        return false
    end
    for i = 1, #sortedA do
        if not (sortedA[i] == sortedB[i]) then
            return false
        end
    end
    return true
end

function ChordSpace.setIntersection(A, setB)
    local result = {}
    for index, value in pairs(A) do
        if ChordSpace.setContains(setB, value) then
            ChordSpace.setInsert(result, value)
        end
    end
    return result
end

function ChordSpace.union(A, B)
    local result = {}
    for index, value in pairs(A) do
        ChordSpace.setInsert(result, value)
    end
    for index, value in pairs(B) do
        ChordSpace.setInsert(result, value)
    end
    return result
end

ChordSpace.pitchClassesForNames = {}

ChordSpace.pitchClassesForNames["C" ] =  0
ChordSpace.pitchClassesForNames["C#"] =  1
ChordSpace.pitchClassesForNames["Db"] =  1
ChordSpace.pitchClassesForNames["D" ] =  2
ChordSpace.pitchClassesForNames["D#"] =  3
ChordSpace.pitchClassesForNames["Eb"] =  3
ChordSpace.pitchClassesForNames["E" ] =  4
ChordSpace.pitchClassesForNames["F" ] =  5
ChordSpace.pitchClassesForNames["F#"] =  6
ChordSpace.pitchClassesForNames["Gb"] =  6
ChordSpace.pitchClassesForNames["G" ] =  7
ChordSpace.pitchClassesForNames["G#"] =  8
ChordSpace.pitchClassesForNames["Ab"] =  8
ChordSpace.pitchClassesForNames["A" ] =  9
ChordSpace.pitchClassesForNames["A#"] = 10
ChordSpace.pitchClassesForNames["Bb"] = 10
ChordSpace.pitchClassesForNames["B" ] = 11

ChordSpace.chordsForNames = {}
ChordSpace.namesForChords = {}

local function fill(rootName, rootPitch, typeName, typePitches)
    local chordName = rootName .. typeName
    local chord = Chord:new()
    local splitPitches = Silencio.split(typePitches)
    chord:resize(#splitPitches)
    for voice, pitchName in ipairs(splitPitches) do
        local pitch = ChordSpace.pitchClassesForNames[pitchName]
        chord[voice] = rootPitch + pitch
    end
    chord = chord:eOP()
    ChordSpace.chordsForNames[chordName] = chord
    ChordSpace.namesForChords[chord:__hash()] = chordName
end

for rootName, rootPitch in pairs(ChordSpace.pitchClassesForNames) do
    fill(rootName, rootPitch, " minor second",     "C  C#                             ")
    fill(rootName, rootPitch, " major second",     "C     D                           ")
    fill(rootName, rootPitch, " minor third",      "C        Eb                       ")
    fill(rootName, rootPitch, " major third",      "C           E                     ")
    fill(rootName, rootPitch, " perfect fourth",   "C              F                  ")
    fill(rootName, rootPitch, " tritone",          "C                 F#              ")
    fill(rootName, rootPitch, " perfect fifth",    "C                    G            ")
    fill(rootName, rootPitch, " augmented fifth",  "C                       G#        ")
    fill(rootName, rootPitch, " sixth",            "C                          A      ")
    fill(rootName, rootPitch, " minor seventh  ",  "C                             Bb  ")
    fill(rootName, rootPitch, " major seventh",    "C                                B")
    -- Scales.
    fill(rootName, rootPitch, " major",            "C     D     E  F     G     A     B")
    fill(rootName, rootPitch, " minor",            "C     D  Eb    F     G  Ab    Bb  ")
    fill(rootName, rootPitch, " natural minor",    "C     D  Eb    F     G  Ab    Bb  ")
    fill(rootName, rootPitch, " harmonic minor",   "C     D  Eb    F     G  Ab       B")
    fill(rootName, rootPitch, " chromatic",        "C  C# D  D# E  F  F# G  G# A  A# B")
    fill(rootName, rootPitch, " whole tone",       "C     D     E     F#    G#    A#  ")
    fill(rootName, rootPitch, " diminished",       "C     D  D#    F  F#    G# A     B")
    fill(rootName, rootPitch, " pentatonic",       "C     D     E        G     A      ")
    fill(rootName, rootPitch, " pentatonic major", "C     D     E        G     A      ")
    fill(rootName, rootPitch, " pentatonic minor", "C        Eb    F     G        Bb  ")
    fill(rootName, rootPitch, " augmented",        "C        Eb E        G  Ab    Bb  ")
    fill(rootName, rootPitch, " Lydian dominant",  "C     D     E     Gb G     A  Bb  ")
    fill(rootName, rootPitch, " 3 semitone",       "C        D#       F#       A      ")
    fill(rootName, rootPitch, " 4 semitone",       "C           E           G#        ")
    fill(rootName, rootPitch, " blues",            "C     D  Eb    F  Gb G        Bb  ")
    fill(rootName, rootPitch, " bebop",            "C     D     E  F     G     A  Bb B")
    -- Major chords.
    fill(rootName, rootPitch, "M",                 "C           E        G            ")
    fill(rootName, rootPitch, "6",                 "C           E        G     A      ")
    fill(rootName, rootPitch, "69",                "C     D     E        G     A      ")
    fill(rootName, rootPitch, "69b5",              "C     D     E     Gb       A      ")
    fill(rootName, rootPitch, "M7",                "C           E        G           B")
    fill(rootName, rootPitch, "M9",                "C     D     E        G           B")
    fill(rootName, rootPitch, "M11",               "C     D     E  F     G           B")
    fill(rootName, rootPitch, "M#11",              "C     D     E  F#    G           B")
    fill(rootName, rootPitch, "M13",               "C     D     E  F     G     A     B")
    -- Minor chords.
    fill(rootName, rootPitch, "m",                 "C        Eb          G            ")
    fill(rootName, rootPitch, "m6",                "C        Eb          G     A      ")
    fill(rootName, rootPitch, "m69",               "C     D  Eb          G     A      ")
    fill(rootName, rootPitch, "m7",                "C        Eb          G        Bb  ")
    fill(rootName, rootPitch, "m#7",               "C        Eb          G           B")
    fill(rootName, rootPitch, "m7b5",              "C        Eb       Gb          Bb  ")
    fill(rootName, rootPitch, "m9",                "C     D  Eb          G        Bb  ")
    fill(rootName, rootPitch, "m9#7",              "C     D  Eb          G           B")
    fill(rootName, rootPitch, "m11",               "C     D  Eb    F     G        Bb  ")
    fill(rootName, rootPitch, "m13",               "C     D  Eb    F     G     A  Bb  ")
    -- Augmented chords.
    fill(rootName, rootPitch, "+",                 "C            E         G#         ")
    fill(rootName, rootPitch, "7#5",               "C            E         G#     Bb  ")
    fill(rootName, rootPitch, "7b9#5",             "C  Db        E         G#     Bb  ")
    fill(rootName, rootPitch, "9#5",               "C     D      E         G#     Bb  ")
    -- Diminished chords.
    fill(rootName, rootPitch, "o",                 "C        Eb       Gb              ")
    fill(rootName, rootPitch, "o7",                "C        Eb       Gb       A      ")
    -- Suspended chords.
    fill(rootName, rootPitch, "6sus",              "C              F     G     A      ")
    fill(rootName, rootPitch, "69sus",             "C     D        F     G     A      ")
    fill(rootName, rootPitch, "7sus",              "C              F     G        Bb  ")
    fill(rootName, rootPitch, "9sus",              "C     D        F     G        Bb  ")
    fill(rootName, rootPitch, "M7sus",             "C              F     G           B")
    fill(rootName, rootPitch, "M9sus",             "C     D        F     G           B")
    -- Dominant chords.
    fill(rootName, rootPitch, "7",                 "C            E       G        Bb  ")
    fill(rootName, rootPitch, "7b5",               "C            E    Gb          Bb  ")
    fill(rootName, rootPitch, "7b9",               "C  Db        E       G        Bb  ")
    fill(rootName, rootPitch, "7b9b5",             "C  Db        E    Gb          Bb  ")
    fill(rootName, rootPitch, "9",                 "C     D      E       G        Bb  ")
    fill(rootName, rootPitch, "9#11",              "C     D      E F#    G        Bb  ")
    fill(rootName, rootPitch, "13",                "C     D      E F     G     A  Bb  ")
    fill(rootName, rootPitch, "13#11",             "C     D      E F#    G     A  Bb  ")
end

table.sort(ChordSpace.chordsForNames)
table.sort(ChordSpace.namesForChords)

-- Increment a chord voicewise through chord space,
-- from a low point on the unison diagonal through a high point
-- on the unison diagonal. g is the generator of transposition.
-- It may be necessary to set the chord to the low point to start.

function ChordSpace.next(odometer, low, high, g)
    local voices = #odometer
    odometer[voices] = odometer[voices] + g
     -- "Carry."
    for voice = voices, 2, -1 do
        if odometer[voice] > high then
            odometer[voice] = low
            odometer[voice - 1] = odometer[voice - 1] + g
        end
    end
    if odometer[1] > high then
        return false
    end
    return true
end

function ChordSpace.allOfEquivalenceClass(voices, equivalence, g)
    g = g or 1
    local equivalenceMapper = nil
    if equivalence == 'OP' then
        equivalenceMapper = Chord.iseOP
    end
    if equivalence == 'OPT' then
        equivalenceMapper = Chord.iseOPT
    end
    if equivalence == 'OPTT' then
        equivalenceMapper = Chord.iseOPTT
    end
    if equivalence == 'OPI' then
        equivalenceMapper = Chord.iseOPI
    end
    if equivalence == 'OPTI' then
        equivalenceMapper = Chord.iseOPTI
    end
    if equivalence == 'OPTTI' then
        equivalenceMapper = Chord.iseOPTTI
    end
    local equivalentChords = {}
    -- Enumerate all chords in [-O, O].
    local iterator = ChordSpace.iterator(voices, -13)
    -- print('iterator:', tostring(iterator))
    while ChordSpace.next(iterator, -13, 13, g) == true do
        if iterator:iseP() == true then
            local eP = iterator:clone()
            if equivalenceMapper(eP) then
                ChordSpace.setInsert(equivalentChords, eP)
            end
        end
    end
    local equivalentChords = ChordSpace.sortedSet(equivalentChords)
    local zeroBasedChords = ChordSpace.zeroBasedSet(equivalentChords)
    return zeroBasedChords, equivalentChords
end

-- Returns a chord with the specified number of voices all set to a first
-- pitch, useful as an iterator.

function ChordSpace.iterator(voices, first)
    local odometer = Chord:new()
    odometer:resize(voices)
    for voice = 1, voices do
        odometer[voice] = first
    end
    return odometer
end

-- Returns a collection of all chords for the specified number of voices in a
-- range, by default the octave. g is the generator of transposition, by default the
-- semitone.

function ChordSpace.allChordsInRange(voices, first, last, g)
    first = first or 0
    last = last or ChordSpace.OCTAVE
    g = g or 1
    -- Enumerate all chords in the range.
    local chordset = {}
    local iterator = ChordSpace.iterator(voices, first)
    while ChordSpace.next(iterator, first, last, g) do
        local chord = iterator:clone()
        chordset[chord:__hash()] = chord
    end
    return ChordSpace.sortedSet(chordset)
end

-- Move 1 voice of the chord,
-- optionally under range equivalence
-- NOTE: Does NOT return the result under any equivalence class.

function Chord:move(voice, interval)
    local chord = self:clone()
    chord[voice] = T(chord[voice], interval)
    return chord
end

-- Performs the neo-Riemannian Lettonwechsel transformation.
-- NOTE: Does NOT return the result under any equivalence class.

function Chord:nrL()
    local cv = self:eV()
    local cvt = self:eV():et()
    if cvt[2] == 4 then
        cv[1] = cv[1] - 1
    else
        if cvt[2] == 3 then
            cv[3] = cv[3] + 1
        end
    end
    return cv
end

-- Performs the neo-Riemannian parallel transformation.
-- NOTE: Does NOT return the result under any equivalence class.

function Chord:nrP()
    local cv = self:eV()
    local cvt = self:eV():et()
    if cvt[2] == 4 then
        cv[2] = cv[2] - 1
    else
        if cvt[2] == 3 then
            cv[2] = cv[2] + 1
        end
    end
    return cv
end

-- Performs the neo-Riemannian relative transformation.
-- NOTE: Does NOT return the result under any equivalence class.

function Chord:nrR()
    local cv = self:eV()
    local cvt = self:eV():et()
    if cvt[2] == 4 then
        cv[3] = cv[3] + 2
    else
        if cvt[2] == 3 then
            cv[1] = cv[1] - 2
        end
    end
    return cv
end

-- Performs the neo-Riemannian dominant transformation.
-- NOTE: Does NOT return the result under any equivalence class.

function Chord:nrD()
    return self:eep():T(-7)
end

-- Returns the chord inverted by the sum of its first two voices.
-- NOTE: Does NOT return the result under any equivalence class.

function Chord:K(range)
    range = range or ChordSpace.OCTAVE
    local chord = self:clone()
    if #chord < 2 then
        return chord
    end
    local ep = chord:eP()
    local x = ep[1] + ep[2]
    return self:I(x)
end

-- Returns whether the chord is a transpositional form of Y with interval size g.
-- Only works in equal temperament.

function Chord:Tform(Y, g)
    local eopx = self:epcs()
    local i = 0
    while i < ChordSpace.OCTAVE do
        local ty = Y:T(i)
        local eopty = ty:epcs()
        if eopx == eopty then
            return true
        end
        i = i + g
    end
    return false
end

-- Returns whether the chord is an inversional form of Y with interval size g.
-- Only works in equal temperament.

function Chord:Iform(Y, g)
    local eopx = self:epcs()
    local i = 0
    while i < ChordSpace.OCTAVE do
        local iy = Y:I(i)
        local eopiy = iy:epcs()
        if eopx == eopiy then
            return true
        end
        i = i + g
    end
    return false
end

-- Returns the contextual transposition of the chord by x with respect to m
-- with minimum interval size g.
-- NOTE: Does NOT return the result under any equivalence class.

function Chord:Q(x, m, g)
    g = g or 1
    if self:Tform(m, g) then
        return self:T(x)
    end
    if self:Iform(m, g) then
        return self:T(-x)
    end
    return self:clone()
end

function Chord:J(n, g, i)
    g = g or 1
    local inversions = {}
    local index = 0
    for I = 1, 11, g do
        local inversion = self:I(I)
        if ChordSpace.invariantPcs(self, inversion) == n then
            index = index + 1
            table.insert(inversions, inversion:eOP())
        end
    end
    local result = ChordSpace.sortedSet(inversions)
    if i ~= nil then
        if i > #result then
            return nil
        else
            return result[i]
        end
    end
    return result
end

-- Returns the voice-leading between chords a and b,
-- i.e. what you have to add to a to get b, as a
-- chord of directed intervals.

function ChordSpace.voiceleading(a, b)
    local voiceleading = a:clone()
    for voice = 1, #voiceleading do
        voiceleading[voice] = b[voice] - a[voice]
    end
    return voiceleading
end

-- Returns whether the voiceleading
-- between chords a and b contains a parallel fifth.

function ChordSpace.parallelFifth(a, b)
    local v = ChordSpace.voiceleading(a, b)
    if v:count(7) > 1 then
        return true
    else
        return false
    end
end

-- Returns the smoothness of the voiceleading between
-- chords a and b by L1 norm.

function ChordSpace.voiceleadingSmoothness(a, b)
    local L1 = 0
    for voice = 1, #a do
        L1 = L1 + math.abs(b[voice] - a[voice])
    end
    return L1
end

-- Returns which of the voiceleadings (source to d1, source to d2)
-- is the smoother (shortest moves), optionally avoiding parallel fifths.

function ChordSpace.voiceleadingSmoother(source, d1, d2, avoidParallels, range)
    range = range or ChordSpace.OCTAVE
    if avoidParallels then
        if ChordSpace.parallelFifth(source, d1) then
            return d2
        end
        if ChordSpace.parallelFifth(source, d2) then
            return d1
        end
    end
    local s1 = ChordSpace.voiceleadingSmoothness(source, d1)
    local s2 = ChordSpace.voiceleadingSmoothness(source, d2)
    if s1 <= s2 then
        return d1
    else
        return d2
    end
end

-- Returns which of the voiceleadings (source to d1, source to d2)
-- is the simpler (fewest moves), optionally avoiding parallel fifths.

function ChordSpace.voiceleadingSimpler(source, d1, d2, avoidParallels)
    avoidParallels = avoidParallels or false
    if avoidParallels then
        if ChordSpace.parallelFifth(source, d1) then
            return d2
        end
        if ChordSpace.parallelFifth(source, d2) then
            return d1
        end
    end
    local v1 = ChordSpace.voiceleading(source, d1):eP()
    local v2 = ChordSpace.voiceleading(source, d2):eP()
    for voice = #v1, 1, -1 do
        if v1[voice] < v2[voice] then
            return d1
        end
        if v2[voice] < v1[voice] then
            return d2
        end
    end
    return d1
end

-- Returns which of the voiceleadings (source to d1, source to d2)
-- is the closer (first smoother, then simpler), optionally avoiding parallel fifths.

function ChordSpace.voiceleadingCloser(source, d1, d2, avoidParallels)
    avoidParallels = avoidParallels or false
    if avoidParallels then
        if ChordSpace.parallelFifth(source, d1) then
            return d2
        end
        if ChordSpace.parallelFifth(source, d2) then
            return d1
        end
    end
    local s1 = ChordSpace.voiceleadingSmoothness(source, d1)
    local s2 = ChordSpace.voiceleadingSmoothness(source, d2)
    if s1 < s2 then
        return d1
    end
    if s1 > s2 then
        return d2
    end
    return ChordSpace.voiceleadingSimpler(source, d1, d2, avoidParallels)
end

-- Returns the voicing of the destination which has the closest voice-leading
-- from the source within the range, optionally avoiding parallel fifths.

function ChordSpace.voiceleadingClosestRange(source, destination, range, avoidParallels)
    local destinationeOP = destination:eOP()
    local d = destinationeOP:clone()
    local odometer = source:origin()
    while ChordSpace.next(odometer, 0, range, ChordSpace.OCTAVE) == true do
        local revoicing = odometer:clone()
        for voice = 1, #revoicing do
            revoicing[voice] = revoicing[voice] + destinationeOP[voice]
        end
        d = ChordSpace.voiceleadingCloser(source, d, revoicing, avoidParallels)
    end
    return d
end

-- Creates a complete Silencio "note on" event for the
-- indicated voice of the chord. The other parameters are used
-- if the internal duration, channel, velocity, and pan of the
-- chord are nil.

function Chord:note(voice_, time_, duration_, channel_, velocity_, pan_)
    local note_ = Event:new()
    note_[TIME] = time_
    note_[DURATION] = duration_ or self.duration[voice_]
    note_[CHANNEL] = channel_ or self.channel[voice_]
    note_[KEY] = self[voice_]
    note_[VELOCITY] = velocity_ or self.velocity[voice_]
    note_[PAN] = pan_ or self.pan[voice_]
    return note_
end

-- Returns an individual note for each voice of the chord.
-- The chord's duration, instrument, and loudness are used if present,
-- if not the specified values are used.

function Chord:notes(time_, duration_, channel_, velocity_, pan_)
    local notes_ = Score:new()
    for voice, key in ipairs(self) do
        table.insert(notes_, self:note(voice, time_, duration_, channel_, velocity_, pan_))
    end
    return notes_
end

function Chord:toScore(score, time_, duration_, channel_, velocity_, pan_)
    for voice, key in ipairs(self) do
        score:append(self:note(voice, time_, duration_, channel_, velocity_, pan_))
    end
    return score
end

-- Move the pitch to the closest pitch-class of the chord.

function ChordSpace.conformPitchToChord(pitch, chord)
    local pitchClass = pitch % ChordSpace.OCTAVE
    local octave = pitch - pitchClass
    local chordPitchClass = chord[1] % ChordSpace.OCTAVE
    local distance = math.abs(chordPitchClass - pitchClass)
    local closestPitchClass = chordPitchClass
    local minimumDistance = distance
    for voice = 2, #chord do
        chordPitchClass = chord[voice] % ChordSpace.OCTAVE
        distance = math.abs(chordPitchClass - pitchClass)
        if distance < minimumDistance then
            minimumDistance = distance
            closestPitchClass = chordPitchClass
        end
    end
    pitch = octave + closestPitchClass
    return pitch
end

-- If the event is a note, moves its pitch
-- to the closest pitch of the chord.
-- If octaveEquivalence is true (the default),
-- the pitch-class of the note is moved to the closest pitch-class
-- of the chord; otherwise, the pitch of the note is moved to the closest
-- absolute pitch of the chord.

function ChordSpace.conformToChord(event, chord, octaveEquivalence)
    octaveEquivalence = octaveEquivalence or true
    if event[STATUS] ~= 144 then
        return
    else
        local pitch = event[KEY]
        if octaveEquivalence then
            pitch = ChordSpace.conformPitchToChord(pitch, chord)
            event[KEY] = pitch
        else
            local chordPitch = chord[1]
            local distance = math.abs(chordPitch - pitch)
            local closestPitch = chordPitch
            local minimumDistance = distance
            for voice = 2, #chord do
                chordPitch = chord[voice]
                distance = math.abs(chordPitch - pitch)
                if distance < minimumDistance then
                    minimumDistance = distance
                    closestPitch = chordPitch
                end
            end
            event[KEY] = closestPitch
        end
    end
end

-- Inserts the notes of the chord into the score at the specified time.
-- The internal duration, instrument, and loudness are used if present,
-- if not the specified values are used.

function ChordSpace.insert(score, chord, time_, duration, channel, velocity, pan)
    -- print(score, chord, time_, duration, channel, velocity, pan)
    for voice = 1, #chord do
        local event = chord:note(voice, time_, duration, channel, velocity, pan)
        table.insert(score, event)
    end
end

-- For all the notes in the score
-- beginning at or later than the start time,
-- and up to but not including the end time,
-- moves the pitch of the note to belong to the chord, using the
-- conformToChord function.

function ChordSpace.apply(score, chord, start, end_, octaveEquivalence)
    octaveEquivalence = octaveEquivalence or true
    local s = score:slice(start, end_)
    if #slice > 0 then
        for index, event in ipairs(s) do
            ChordSpace.conformToChord(event, chord, octaveEquivalence)
        end
    end
end

-- Returns a chord containing all the pitches of the score
-- beginning at or later than the start time,
-- and up to but not including the end time.

function gather(score, start, end_)
    local chord = Chord:new()
    local slice = score:slice(start, end_)
    for index, event in ipairs(slice) do
        local pitch = event[KEY]
        if not chord:contains(pitch) then
            table.insert(chord, pitch)
        end
    end
    return chord
end

-- Orthogonal additive groups for unordered chords of given arity under range
-- equivalence (RP): prime form or P, inversion or I, transposition or T, and
-- voicing or V. P x I x T = OP, P x I x T x V = RP. Therefore, an
-- operation on P, I, T, or V may be used to independently transform the
-- respective symmetry of any chord. Some of these operations will reflect
-- in RP.

ChordSpaceGroup = {}

-- N is the number of voices in the chord space, g is the generator of
-- transposition, range is the size of chord space,
-- optis is an ordered table of all OPTI chords for g,
-- voicings is an ordered table of all octavewise permutations in RP.

function ChordSpaceGroup:new(o)
    local o = o or {optisForIndexes = {}, indexesForOptis = {}, voicingsForIndexes = {}, indexesForVoicings = {}}
    setmetatable(o, self)
    self.__index = self
    return o
end

function ChordSpace.octavewiseRevoicings(chord, range)
    range = range or ChordSpace.OCTAVE
    local voices = #chord
    local odometer = chord:origin()
    -- Enumerate the permutations.
    -- iterator[1] is the most significant voice, and
    -- iterator[N] is the least significant voice.
    local voicings = 0
    while ChordSpace.next(odometer, 0, range, ChordSpace.OCTAVE) == true do
        voicings = voicings + 1
    end
    return voicings
end

function ChordSpace.octavewiseRevoicing(chord, index, range)
    local voices = #chord
    local odometer = chord:origin()
    local eop = chord:eOP()
    -- Enumerate the permutations.
    -- iterator[1] is the most significant voice, and
    -- iterator[N] is the least significant voice.
    local voicings = 0
    for v = 1, index do
        ChordSpace.next(odometer, 0, range, ChordSpace.OCTAVE)
        -- Wrap around?
        if odometer[1] > range then
            odometer = chord:origin()
        end
        voicings = voicings + 1
    end
    for voice = 1, #chord do
        odometer[voice] = odometer[voice] + eop[voice]
    end
    return odometer
end

-- Returns the ith arpeggiation, current voice, and corresponding revoicing
-- of the chord. Positive arpeggiations start with the lowest voice of the
-- chord and revoice up; negative arpeggiations start with the highest voice
-- of the chord and revoice down.

function Chord:a(arpeggiation)
    local chord = self:v(arpeggiation)
    if arpeggiation < 0 then
        return chord[#chord], #chord, chord
    end
    return chord[1], 1, chord
end

function ChordSpaceGroup:initialize(voices, range, g)
    self.voices = voices or 3
    self.range = range or 60
    self.g = g or 1
    self.countP = 0
    self.countI = 2
    self.countT = ChordSpace.OCTAVE / self.g
    local chord = Chord:new()
    chord:resize(voices)
    self.countV = ChordSpace.octavewiseRevoicings(chord, self.range)
    self.indexesForOptis = {}
    self.optisForIndexes = ChordSpace.allOfEquivalenceClass(voices, 'OPTTI')
    for index, optti in pairs(self.optisForIndexes) do
        self.indexesForOptis[optti:__hash()] = index
        self.countP = self.countP + 1
    end
    self:list()
end

function ChordSpace.createFilename(voices, range, g, extension)
    extension = extension or '.lua'
    local gstring = string.format('g%.6f', g)
    gstring = string.gsub(gstring, '%.', '_')
    local filename = string.format('ChordSpaceGroup_V%d_R%d_%s%s', voices, range, gstring, extension)
    return filename
end

-- Loads the group if found, creates and saves it otherwise.

function ChordSpace.createChordSpaceGroup(voices, range, g)
    local filename = ChordSpace.createFilename(voices, range, 1)
    local file, message, error = io.open(filename, 'r')
    if file == nil then
        print(string.format('File "%s" not found, creating...', filename))
        chordSpaceGroup = ChordSpaceGroup:new()
        chordSpaceGroup:initialize(voices, range, g)
        chordSpaceGroup:save()
        return chordSpaceGroup
    else
        print(string.format('Loading ChordSpaceGroup from file "%s"...', filename))
        return ChordSpace.load(voices, range, g)
    end
end

function ChordSpace.load(voices, range, g)
    local filename = ChordSpace.createFilename(voices, range, 1)
    print('Loading:', filename)
    local deserialized = ChordSpaceGroup.load(filename)
    return deserialized
end

function ChordSpaceGroup:save(filename)
    filename = filename or ChordSpace.createFilename(self.voices, self.range, self.g, '.lua')
    local text = serialize(self)
    local writer = io.open(filename, 'w+')
    writer:write(text)
    writer:close()
end

function ChordSpaceGroup.load(filename)
    local reader = io.open(filename)
    local text = reader:read("*all")
    -- What's deserialized is a function, which needs to be called.
    local object = loadstring(text)()
    -- Fix up metatable.
    local chordSpaceGroup = ChordSpaceGroup:new(object)
    -- Fix up metatables of chords too.
    for index, opti in pairs(chordSpaceGroup.optisForIndexes) do
        chordSpaceGroup.optisForIndexes[index] = Chord:new(opti)
    end
    return chordSpaceGroup
end

-- Returns the chord for the indices of prime form, inversion,
-- transposition, and voicing. The chord is not in RP; rather, each voice of
-- the chord's OP may have zero or more octaves added to it.

function ChordSpaceGroup:toChord(P, I, T, V, printme)
    printme = printme or false
    P = P % self.countP
    I = I % 2
    T = T % ChordSpace.OCTAVE
    V = V % self.countV
    if printme then
        print('toChord:             ', P, I, T, V)
    end
    local optti = self.optisForIndexes[P]
    if printme then
        print('toChord:   optti:    ', optti, optti:__hash())
    end
    local optt = nil
    if I == 0 then
        optt = optti
    else
        optt = optti:I():eOPTT()
    end
    if printme then
        print('toChord:   optt:     ', optt)
    end
    local optt_t = optt:T(T)
    if printme then
        print('toChord:   optt_t:   ', optt_t)
    end
    local op = optt_t:eOP()
    if printme then
        print('toChord:   op:       ', op)
    end
    V = V % self.countV
    local revoicing = ChordSpace.octavewiseRevoicing(op, V, self.range)
    if printme then
        print('toChord:   revoicing:', revoicing)
    end
    return revoicing, opti, op
end

-- Returns the indices of prime form, inversion, transposition,
-- and voicing for a chord.

function ChordSpaceGroup:fromChord(chord, printme)
    printme = printme or false
    if printme then
        print('fromChord: chord:    ', chord, chord:iseOP())
    end
    local op = nil
    if chord:iseOP() then
        op = chord:clone()
    else
        op = chord:eOP()
    end
    if printme then
        print('fromChord: op:       ', op)
    end
    local optt = chord:eOPTT()
    if printme then
        print('fromChord: optt:     ', optt)
    end
    local T = 0
    for t = 0, ChordSpace.OCTAVE - 1, self.g do
        local optt_t = optt:T(t):eOP()
        if printme then
            print('fromChord: optt_t:   ', optt_t, t)
        end
        if optt_t:__eq_epsilon(op) == true then
            if printme then
                print('equals')
            end
            T = t
            break
        end
    end
    local optti = chord:eOPTTI()
    if printme then
        print('fromChord: optti:    ', optti, optti:__hash())
    end
    local P = self.indexesForOptis[optti:__hash()]
    local I = 0
    if optti ~= optt then
        I = 1
        local optt_i_optt = optt:I():eOPTT()
        if optt_i_optt ~= optti then
            print("Error: OPTT(I(OPTT)) must equal OPTTI.")
            print('optt_i_optt:', optt_i_optt:information())
            print('optti:      ', optti:information())
            os.exit()
        end
    end
    local voicing = ChordSpace.voiceleading(op, chord)
    V = self.indexesForVoicings[voicing:__hash()]
    if printme then
        print('fromChord: voicing:  ', voicing, V)
        print('fromChord:           ', P, I, T, V)
    end
    return P, I, T, V
end

function ChordSpaceGroup:list(listheader, listopttis, listvoicings)
    listheader = listheader or true
    listopttis = listopttis or false
    listvoicings = listvoicings or false
    if listheader then
        print(string.format('ChordSpaceGroup.voices: %8d', self.voices))
        print(string.format('ChordSpaceGroup.range : %8d', self.range))
        print(string.format('ChordSpaceGroup.g     : %13.4f', self.g))
        print(string.format('ChordSpaceGroup.countP: %8d', self.countP))
        print(string.format('ChordSpaceGroup.countI: %8d', self.countI))
        print(string.format('ChordSpaceGroup.countT: %8d', self.countT))
        print(string.format('ChordSpaceGroup.countV: %8d', self.countV))
    end
    if listopttis then
        for index, opti in pairs(self.optisForIndexes) do
            print(string.format('index: %5d  opti: %s  index from opti: %s', index, tostring(opti), self.indexesForOptis[opti:__hash()]))
        end
        for index = 0, #self.optisForIndexes - 1 do
            print(string.format('opti from index: %s  index:  %5d', tostring(self.optisForIndexes[index]), index))
        end
    end
    if listvoicings then
        for index, voicing in pairs(self.voicingsForIndexes) do
            print(string.format('voicing index: %5d  voicing: %s  index from voicing: %5d', index, tostring(voicing), self.indexesForVoicings[voicing:__hash()]))
        end
    end
end

function ChordSpaceGroup:printChords()
    for index, opti in pairs(self.optisForIndexes) do
        print(string.format('index: %5d  opti: %s %s', index, tostring(opti), opti:name()))
    end
end

function ChordSpaceGroup:printNamedChords()
    for index, opti in pairs(self.optisForIndexes) do
        local name = opti:name()
        if name ~= '' then
            print(string.format('index: %5d  opti: %s %s', index, tostring(opti), opti:name()))
        end
    end
end

return ChordSpace
