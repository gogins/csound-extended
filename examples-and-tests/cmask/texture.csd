<CsoundSynthesizer>
<CsOptions>
-odac -m35 -d
</CsOptions>
<CsScore>
f 0 24
</CsScore>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 10

Score cmask {{ 
{
f1 0 8193 8 0 4096 1 4096 0
f2 0 8193 10 1 .5 .3 .2 .1
}

f 0 10 	;field 1:  shepard grains

p1 const 1

p2 range .001 .005 

p3 range .02 .03

p4 rnd tri
mask 200 800
quant [200 50] .95 [0 150]

p5 range .5 .6

p6 rnd uni
mask (0 0 5 .8 10 0) (0 .2 5 1 10 .2)


f 4 6 

p1 const 1

p2 range .001 .005

p3 range .04 .08

p4 rnd tri
mask [2000 1000] [2010 3000]

p5 range .3 .4

p6 range 0 .2


f 6.5 9.5 

p1 const 1

p2 rnd uni
mask [.001 .1] [.005 .2] map 1

p3 range .04 .08

p4 rnd tri
mask [4000 2000] [8000 3000] map 1

p5 rnd uni
mask [.3 .5] [.4 .8]

p6 range  .8 1
}}

instr 1

k1	oscil	8000*p5,1/p3,1
a1	oscil	k1,p4,2
	outs	a1*(1-p6),a1*p6
	
endin	

</CsInstruments>
</CsoundSynthesizer>