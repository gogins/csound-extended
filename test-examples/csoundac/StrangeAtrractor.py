# Copyright (c) 2002, 2003 by Michael Gogins. All rights reserved.
# Tutorial demonstrating a MusicModel composition
# based on translating the orbit of a chaotic attractor to a score.

import CsoundAC
import time

CsoundAC.Random_seed(int(time.time()))

model = CsoundAC.MusicModel()
csound.setPythonMessageCallback()
strangeAttractor = CsoundAC.StrangeAttractor()
strangeAttractor.reset()
strangeAttractor.setDimensionCount(4)
strangeAttractor.setAttractorType(3)
strangeAttractor.setIterationCount(2000)
while strangeAttractor.searchForAttractor():
        pass
print "ScoreType = ", strangeAttractor.getScoreType()
#strangeAttractor.setCode('OMPDPTWXBMXDRRWRJCLXRVRTPWRYBMFPJBFFVGGNAKAGUIJFAKLAIAIEYNTHKFCUGRVDCYILYPTDNUITMFVGESBHSRKYWEGWTNVGNQKUMUJULEDMKRBGAFNIXVDMCLJIITUVQNNBXYLMCJYARPSACEYKFGHBVYXIWLCYJEBJIUCNFLKKNUXRHGWUONWFPMOALWLODJQTKLGFBYGXQYMKPPTYANECOPBJSDLLYJYERXEOBOWFKKYNXGHNCCYWTINDFIJXAUEEQXBEQLORVORIGISBC')
#strangeAttractor.setCode('OMPDPTWXBMXDRRWRJCLXRVRTPWRYBMFPJBFFVGGNAKAGUIJFAKLAIAIEYNTHKFCUGRVDCYILYPTDNUITMFVGESBHSRKYWEGWTNVGNQKUMUJULEDMKRBGAFNIXVDMCLJIITUVQNNBXYLMCJYARPSACEYKFGHBVYXIWLCYJEBJIUCNFLKKNUXRHGWUONWFPMOALWLODJQTKLGFBYGXQYMKPPTYANECOPBJSDLLYJYERXEOBOWFKKYNXGHNCCYWTINDFIJXAUEEQXBEQLORVORIGISBC')
#strangeAttractor.setCode('SJEOSGBEDTSYIUJLGICPVYJPNDRYXLUDFHBSMFMNEBWKWJPXLOWIARKQBEXJDBSYVQHHACDSVATCJELUKHHBUEJUXNKEMNARCJBMBVSBKS')
#strangeAttractor.setCode('WVBEKJFDCWESFBQVBQAISKEKDYLCVBXTSWATPPGCNCLVTXHJWEETRGIHRRMFVVPPBYNEHKXBSYKYVTYINHCNVFCNOGOYCMQANLBLBCJQJCUWRQMIIWSSIEQCIIOHEXQNMDAFGSWQRKCGOSYTTHYAONPBUCDVAPMAYUCBOWJYSUHUTVUKEISHDEIENUXJVHNLQNRNKGIHOKLXFSMGMTSNJMSYTSECNJHLBDWEJJCETRSDOKVFDIQDSMWWMYVIANAIANOXDDPKLOJFOUMWBQFQADWBE')
#strangeAttractor.setCode('TPKRWTCCXRDACSDWAAXJATYTBGEWBYGLUCGENDPQMCTRVJLNNFKWNEJOQVVUCWXDPUQFDFMJGMLRVDESPKXXSIXVNWJOAIDHFUPVBQJIMCBTAHJRKHDPFCHOKSEBOKMQKSWQXSXFVAJNTXFLTGGVVUEOADTMTAPOIRWUMAHPJ')
print "Code = ", strangeAttractor.getCode()
strangeAttractor.generate()
print "Generated events = ", len(strangeAttractor.getScore())

# Place the Lindenmayer node inside a Random node to randomize velocity and pan,
# place the Random node inside a Rescale node,
# and place the Rescale node inside the MusicModel.

random = CsoundAC.Random()
print 'random:', random
random.createDistribution("uniform_01")
random.setElement(6, 11, 1)
random.setElement(8, 11, 1)

rescale = CsoundAC.Rescale()
print 'rescale:', rescale
rescale.setRescale( 0, True, True,  0,     300)
rescale.setRescale( 1, True, False,  2,       4)
rescale.setRescale( 3, True, True, 10,       4)
rescale.setRescale( 4, True, True, 36,      60)
rescale.setRescale( 5, True, True, 75,      12)
rescale.setRescale( 7, True, True, -0.5,     1)
rescale.setRescale(10, True, True,  0,    4095)
random.addChild(strangeAttractor)
rescale.addChild(random)

# Add these nodes to the builtin MusicModel instance.
model.addChild(rescale)
model.setTonesPerOctave(12.0)
print 'generating...'
model.generate()
print 'finished.'

model.cppsoundLoad('../csound-vst/CsoundAC.csd')
model.setCsoundCommand("--0dbfs=1 -m195 -RWdfo StrangeAttractor.py.wav")
score = model.getScore()
print 'Events in generated score:', len(score)
duration = score.getDuration()
print 'Duration: %9.4f' % (duration)
score.arrange(0, 7)
score.arrange(1, 5)
score.arrange(2, 13)
score.arrange(3, 10)
score.arrange(4, 14)
score.arrange(5, 7)
score.arrange(6, 5)
score.arrange(7, 9)
print "Score: ", model.getScore()
print "Command:", model.getCsoundCommand()
model.perform()








