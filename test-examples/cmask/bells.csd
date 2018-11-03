<CsoundSynthesizer>
<CsOptions>
-odac -m35 -d
</CsOptions>
<CsScore>
i 2 1 12

</CsScore>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 10

instr 1

;p2 onset
;p3 duration
;p4 base frequency
;p5 fm index
;p6 pan (L=0, R=1)

kenv	expon		1,p3,0.001
kindx	expon		p5,p3,.4
a1	foscil	kenv*10000,p4,1,1.143,kindx,1
	outs		a1*(1-p6),a1*p6
		
endin	

instr 2
Score cmask {{ 
{
f1 0 65536 10 1
}

f 0 20  

p1 const 1

p2				;decreasing density
rnd uni
mask [.03 .5 ipl 3] [.08 1 ipl 3] map 1
prec 2


p3 				;increasing duration
rnd uni
mask [.2 3 ipl 1] [.4 5 ipl 1]
prec 2


p4				;narrowing frequency grid
rnd uni
mask [3000 90 ipl 1] [5000 150 ipl 1] map 1
quant [400 50] .95
prec 2

p5 
rnd uni
mask [2 4] [4 7]
prec 2

p6 range 0 1 
prec 2
}}
endin
</CsInstruments>
</CsoundSynthesizer>