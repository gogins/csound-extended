Code for Euterpea-made portion of "Blue Lambda"
Donya Quick
(Full composition: https://soundcloud.com/donyaquick/blue-lambda)

> module BlueLambda where
> import Euterpea
> import Control.DeepSeq
> import Csound

A simple composition in four lines of code:

> x1 = c 4 en :+: g 4 en :+: c 5 en :+: g 5 en
> x2 = x1 :+: transpose 3 x1
> x3 = x2 :+: x2 :+: invert x2 :+: retro x2
> x4 = forever x3 :=: forever (tempo (2/3) x3)

This variation using a half note offset also sounds interesting:

> x4' = forever x3 :=: (rest hn :+: forever (tempo (2/3) x3))

Both x4 and x4' will play infinitely when using Euterpea's play
function. To stop it, use Ctrl+C.

This alternate playback function will merge overlapping notes, 
which makes for a cleaner sound on some synthesizers:

> playX :: (NFData a, ToMusic1 a) => Music a -> IO ()
> playX = playC defParams{perfAlg = eventMerge . perform} where 
>     eventMerge :: Performance -> Performance
>     eventMerge (e1:e2:es) = 
>         let e1' = e1{eDur = eTime e2 + eDur e2 - eTime e1}
>         in  if ePitch e1 == ePitch e2 then eventMerge (e1':es) 
>             else e1 : eventMerge (e2:es)
>     eventMerge e = e

> main = playCsound x4' [(AcousticGrandPiano, (27, 0.0, 0.5))] csoundVST 0
