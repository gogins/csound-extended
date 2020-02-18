<CsoundSynthesizer>
<CsOptions>
-odac -m35 -d
</CsOptions>
<CsScore>
</CsScore>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 10

Score cmask {{ 
{
;f1 0 262144 1 "whisp.aif" 0 4 1
f1 0 2621442 1 "tigre.aif" 0 0 1
f2 0 8192 19 1 1 270 1
f4 0 8192 9 .25 1 0	
}

f 0 60

p1 const 1

p2 
mask (0 .0005 37 .007 60 .003) (0 .003 37 .15 60 .005) 

p3
;mask [.3 .02] [.7 .04]
mask [.3 .2] [.7 .4]

p4
seg [0 5.9]

p5 range 0 1

p6
mask (0 .3 25 1 40 .7) (0 2 4 1 25 1.2)
quant .3 (0 0 25 .9 30 0 45 .9 55 0) (40 0 45 1.5 55 0)


}}

instr 1

ipanl	table	1-p5 ,4,1
ipanr	table	p5 ,4,1

andx	line	p4,p3,p4+p3*p6
asig	tablei	andx*sr,1
kamp	oscil	8000,1/p3,2
		outs	asig*kamp*ipanl, asig*kamp*ipanr  
	
endin	

</CsInstruments>
</CsoundSynthesizer>