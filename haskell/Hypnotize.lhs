Hypnotize: an inifinte, algorithmic jazz composition
Donya Quickan

"Hypnotize" is an algorithmic composition using the models 
in JazzTypes.lhs. It utilizes random lead sheet generation.
The music is slow and somewhat ambient.

To hear the piece, load this file in GHCi and then run 
"play hypnotize" to hear it! It's an infintely long piece 
of music, so use Ctrl+C to stop it (you may need to press it 
more than once on some computers).

The implementation here uses type Music (AbsPitch,Volume) to 
achieve more diverse textures (in Music AbsPitch, the volumes
are all constant). 

> module Hypnotize where
> import Csound
> import Jazzkell
> import Jazzkell.Utils
> import Euterpea
> import System.Random
> import Data.List (sort, nub)
> import Control.DeepSeq

State definitions. Only the bass is keeping track of state here.
A data type definition was used to show how state information can 
be extracted from more complex types in the future.

> data HypnoState = HypnoState{
>     nextBassPitch::AbsPitch}
>     deriving (Eq, Show)
     
> nullState = HypnoState (-1) -- we use -1 to indicate no pitch

Hypnotize has three parts: a celesta, a piano, and a bass. Both 
the celesta and piano are chord-based generative strategies, although
the celesta arpeggiates its chord over a longer time to give the 
effect of a melody. 

> celestaSpace = [60..85]

> celestaFun :: PartFun (AbsPitch, Volume) HypnoState
> celestaFun inState seg1 seg2 hist g = -- seg2 does not affect anything (doesn't matter if Nothing)
>     let s = scale $ chordCtxt seg1
>         d = segDur seg1
>         (g2, newChord) = pickChord s celestaSpace g
>         (g3, newChord') = permute g2 newChord
>         (g4, x) = stagger g3 d newChord'
>         m = removeZeros $ cut d x
>     in  (g4, inState, trimTo seg1 m)

The bassFun is similar to a walking bass, but plays relatively few 
pitches per segment.

> bassRange = [36..50]

> bassFun :: PartFun (AbsPitch, Volume) HypnoState
> bassFun inState seg1 Nothing hist g0 = undefined
> bassFun inState seg1 (Just seg2) hist g0 = 
>     let nbr = nextBassPitch inState
>         thisS = scale $ chordCtxt seg1
>         nextS = scale $ chordCtxt seg2
>         (g1, rbr) = choose g0 $ filter (\p -> p `mod` 12 == head thisS) bassRange
>         thisRoot = if nbr > 0 then nbr else rbr 
>         (g2, nextRoot) = choose g1 $ filter (\p -> p `mod` 12 == head nextS) bassRange
>         fifthsA = filter (\p -> p `mod` 12 == thisS !! 4)  bassRange
>         fifthsB = filter (\p -> p < max thisRoot nextRoot && p > min thisRoot nextRoot) fifthsA
>         (g3, thisFifth) = choose g2 $ if null fifthsB then fifthsA else fifthsB
>         (r, g4) = randomR (0.0::Double, 1.0) g3
>         d = segDur seg1
>         nrDown = if nextRoot-1 < 36 then nextRoot+1 else nextRoot-1
>         fDown = if thisFifth-1 < 36 then thisFifth+1 else thisFifth
>         pat1 = note (d-hn) (thisRoot,100) :+: note hn (thisFifth,100)
>         pat2 = note (d-hn) (thisRoot,100) :+: note hn (thisRoot+1,100)
>         pat3 = note (d-hn) (thisRoot,100) :+: note hn (nrDown,100)
>         pat4 = note (d-hn) (thisRoot,100) :+: note hn (nextRoot+1,100)
>         pat5 = note (d-hn) (thisRoot,100) :+: note hn (nrDown,100)
>         pat6 = note (d-tn) (thisRoot,100) :+: note tn (nextRoot+1,60)
>         pat7 = note (d-tn) (thisRoot,100) :+: note tn (nrDown,60)
>         pat8 = note (d-hn-tn) (thisRoot,100) :+: note tn (fDown,60) :+: note hn (thisFifth,100)
>         pat9 = note (d-hn-tn) (thisRoot,100) :+: note tn (fDown,60) :+: note hn (thisFifth,100)
>         outState = inState{nextBassPitch=nextRoot}
>         pats = if d > wn then [pat1, pat2, pat3, pat4] 
>                else [pat1, pat2, pat3, pat4, pat5, pat6, pat7, pat8, pat9]
>         (g5, m) = choose g4 pats
>     in  (g5, outState, trimTo seg1 m)

The chordFun function defines the behavior for the grand piano. It creates 
arpeggiated chords.
     
> chordFun :: PartFun (AbsPitch, Volume) HypnoState
> chordFun inState seg1 seg2 hist g = 
>     let s = scale $ chordCtxt seg1
>         (g1, pcs) = pickChordPCs s g
>         d = segDur seg1
>         (g2, pcs') = permute g1 pcs
>         ps = map (+60) $ pcs'
>         mPat1 = chord $ zipWith (\p i -> rest (i*sn) :+: note d (p,80)) ps [0..]
>         mPat2 = rest en :+: mPat1
>         mPat3 = rest den :+: mPat1
>         (g3, mPat) = choose g2 [mPat1, mPat2, mPat3]
>         m = trimTo seg1 mPat
>     in  (g3, inState, m) where
>     mkC d ps = chord $ map (\p -> note d (60+p, 80)) ps

The randomLeadSheet function generates a randomized lead sheet
where there are contiguous groups of segements in the same 
randomly chosen key. Within each group, the chords are random
(at the Roman numeral level).

> randomLeadSheet :: StdGen -> [Segment a]
> randomLeadSheet g0 = 
>     let (g1, n) = choose g0 [1..4]
>         (g2, r) = choose g1 [0..11]
>         (g3,g4) = split g2
>         segs = take n $ randomSegGroup g3 r
>     in  segs ++ randomLeadSheet g4 where
>     modes :: [[PCNum]]
>     modes = take 7 $ modeRec [0,2,4,5,7,9,11] where 
>     modeRec s@(x:xs) = s : modeRec (xs++[x])
>     randomSegGroup :: StdGen -> AbsPitch -> [Segment a]
>     randomSegGroup g0 r = 
>         let (g1, i) = choose g0 [0..5] -- omitting locrian, because yuck
>             mo = modes !! 0 !! i 
>             s' = map ((`mod` 12). (+r) . (+mo)) (modes !! i)
>             cctxt = ChordCtxt (show s') s'
>             (g2, d) = choose g1 [wn, wn, wn, wn, wn, wn, 2*wn]
>             seg = Segment cctxt Regular [] (0,0) d (TimeSig 4 4)
>         in  seg : randomSegGroup g2 r

Now we can generate a lead sheet with this function and 
run the jazz band on it.

> rls = randomLeadSheet $ mkStdGen 6

> myJBP = [JazzPart Harmony Celesta celestaFun nullState,
>          JazzPart Bass AcousticBass bassFun nullState,
>          JazzPart Harmony AcousticGrandPiano chordFun nullState] 

> hypnotize120bpm = runBand myJBP [] rls (mkStdGen 18)
> hypnotize = tempo 0.6 $ hypnotize120bpm -- slow it down to a better pace

Use "play hypnotize" in GHCi to hear it!


================================
UTILITY FUNCTIONS

Trim a music value to the duration fo a segment.

> trimTo :: Segment a -> Music a -> Music a
> trimTo seg m = removeZeros $ 
>     cut (segDur seg) $ remove (snd $ segOnset seg) m

Permute a list of items.

> permute :: (Eq a) => StdGen -> [a] -> (StdGen, [a])
> permute g [] = (g, [])
> permute g xs = 
>      let (g1, x) = choose g xs
>          (g2, xs') = permute g1 $ filter (/=x) xs
>      in  (g2, x:xs')

Stagger/arpeggiate a chord with stochastic volumes and stochastic
addition of ornaments between notes.

> stagger :: StdGen -> Dur -> [AbsPitch] -> (StdGen, Music (AbsPitch,Volume))
> stagger g td ps = 
>     let (g1,g2) = split g
>         vs = map (\v -> 60 + (v `mod` 40)) $ randoms g1 
>         pvs = zip ps vs
>         (g3, ms) = randDursR g2 td pvs
>         (g4, ms') = ornaments g3 ms
>     in  (g4, ms')

Add ornaments to a list of musical values (used in stagger).

> ornaments :: StdGen -> [Music (AbsPitch, Volume)] -> (StdGen, Music (AbsPitch, Volume))
> ornaments g [] = (g, rest 0)
> ornaments g [x] = (g, x)
> ornaments g (x1:x2:xs) = 
>     case x2 of 
>         Prim(Note d (p,v)) -> 
>             let (g1,r) = choose g [True, False, False, False, False, False, False]
>                 (g2,p2) = choose g1 [p-1, p+1]
>                 v2 = 50
>                 (g3, xs') = ornaments g2 xs
>                 newX = if r && dur x1 >= en 
>                        then chDur (-tn) x1 :+: note tn (p2,v2) :+: x2 
>                        else x1 :+: x2
>             in (g3, newX :+: xs')
>         _ -> 
>             let (g2, xs') = ornaments g (x2:xs)
>             in  (g2, x1 :+: xs')

Add duration to a note or rest Music value.

> chDur d' (Prim (Note d x)) = note (d+d') x
> chDur d' (Prim (Rest d)) = rest (d+d')
> chDur d' x = x

Add random durations to a list of "a" types for music. These 
can be either pitches (AbsPitch) or pitch volume pairs.

> randDursR :: StdGen -> Dur -> [a] -> (StdGen, [Music a])
> randDursR g totalDur xs = 
>     let durs = filter (<=totalDur - (fromIntegral (length xs) *en)) [0, en, qn]
>         (g1, rDur) = choose g durs
>         (g2, m) = randDurs g1 (totalDur - rDur) xs
>     in  (g2, if rDur<=0 then m else rest rDur : m) where
>     randDurs :: StdGen -> Dur -> [a] -> (StdGen, [Music a])
>     randDurs g totalDur [] = (g, [rest 0])
>     randDurs g totalDur [x] = (g, [note totalDur x])
>     randDurs g totalDur (x:xs) = 
>         let durs = filter (<=totalDur - (fromIntegral (length xs) *en)) [en, qn, dqn, hn]
>             (g1, d) = choose g $ if null durs then [totalDur] else durs
>             (g2, ms) = randDurs g1 (totalDur - d) xs
>         in  (g2, note d x : ms)

Pick jazzy chord pitch classes from a scale.

> pickChordPCs :: Scale -> StdGen -> (StdGen, [PCNum])
> pickChordPCs scale g =
>     let (g0, basePCInds) = choose g [[1,2,4,6], [2,3,4], [0,3,4], [0,2,4,6]]
>         (g1, i) = choose g [1..5] -- choose an extra note to add
>     in  (g1, sort $ map (scale !!) (i : basePCInds))

Another way of picking jazzy chord pitch classes.

> pickChordPCs2 :: Scale -> StdGen -> (StdGen, [PCNum])
> pickChordPCs2 scale g =
>     let (n,g1) = random g
>         n' = 4 + (n `mod` 10) 
>         (g2, inds) = chooseN g1 n' [0,0,0,1,2,2,3,4,4,4,5,5,6]
>         pcs = map (scale !!) inds
>         (g3, pcs') = permute g2 (pcs) -- ensure root and fifth are included
>     in  (g3, pcs')

Find all combinations of pitches in a pitch space adhering to
a particular list of pitch classes.

> allPitchCombos :: PitchSpace -> [PCNum] -> [[AbsPitch]]
> allPitchCombos pSpace [] = [[]]
> allPitchCombos pSpace (pc:pcs) =
>     let xs = filter (\x -> x `mod` 12 == pc) pSpace
>         ys = allPitchCombos pSpace pcs
>     in  [(x:y) | x<-xs, y<-ys]

Given a scale and a pitch space, pick a chord of concrete pitches.

> pickChord :: Scale -> PitchSpace -> StdGen -> (StdGen, [AbsPitch])
> pickChord scale pSpace g = 
>     let (g1, chordPCs) = pickChordPCs2 scale g
>         allPossibleChords = allPitchCombos pSpace chordPCs 
>     in  choose g allPossibleChords

hypnotize = tempo 0.6 $ hypnotize120bpm -- slow it down to a better pace

> main = playCsound hypnotize [(AcousticGrandPiano, (42, 40.0, 0.5)),
>                              (Celesta, (56, 30.0, 0.5)),
>                              (AcousticBass, (2, -10.0, 0.75))] csoundVST 0



