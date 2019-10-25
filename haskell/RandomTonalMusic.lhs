Random music with tonality
Donya Quick

> module RandomTonalMusic where
> import Euterpea
> import System.Random
> import Csound
> import Text.RawString.QQ

The theme of this example is going to be stochastic selection
from lists of items, since this is a key part of controlling 
tonality when a high degree of randomness is involved. We'll 
start with a function called "choose" that leverages the 
System.Random library to accomplish the task. Given a list,
(which MUST be finite!) and a random generator, an item from
the list is selected and returned at random.

> choose :: [a] -> StdGen -> (a, StdGen)
> choose [] g = error "Nothing to choose from!"
> choose xs g = 
>     let (i,g') = next g
>     in  (xs !! (i `mod` length xs), g')

Now, we use this to create a function for generating random
melodies. The randomMel function will take a list of acceptable
pitches, a list of acceptable durations, a volume threshold, 
and a generator. At each call of this recursive function, either 
a note or rest will be added. It will be a rest if the randomly
generated volume is below the threshold, and a note if above. 
In the note case, the pitch and duration are selected randomly
from their respective lists.

> randomMel :: [AbsPitch] -> [Dur] -> Volume -> StdGen -> Music (AbsPitch, Volume)
> randomMel pitches durs thresh g0 = 
>     let (p, g1) = choose pitches g0 
>         (d, g2) = choose durs g1 
>         (v, g3) = randomR (0,127) g2
>         x = if v < thresh then rest d else note d (p,v) 
>     in  x :+: randomMel pitches durs thresh g3

We now define two lists of pitches: one for a regular melody 
over about 1.5 octaves of the C-minor scale and one using only
a few pitches from one octave of C-minor to be used for a 
bassline. This restriction to certain key pitches for the bass
will help to retain a sense of C-minor (vs. Eb-major or sounding
tonally ambiguous). 

> pitches1, pitches2 :: [AbsPitch]
> pitches1 = [60,62,63,65,67,68,70,72,74,75,77,79] -- C-minor
> pitches2 = [36,43,46,48] -- also C-minor (root, 5th, 7th, root)

Now let's make some music! The two parts, mel1 and mel2, are 
combined in parallel to create a 2-part piece, or duet. If you 
load this file in GHCi, you can either use "main" or "play duet"
to hear the result. Use Ctrl+C to stop (you may need to press 
it a few times on some computers).

> mel1, mel2, duet :: Music (AbsPitch, Volume)
> mel1 = randomMel pitches1 [qn,en,en,en] 40 (mkStdGen 500)
> mel2 = randomMel pitches2 [hn,qn,qn,qn] 20 (mkStdGen 501)
> duet = tempo 1.75 (instrument Celesta mel1 :=: instrument AcousticBass mel2)
> main = playCsound duet [(Celesta, (9, 0.0, 0.3)), (AcousticBass, (17, 0.0, 0.7))] csoundVST 0
