'''
Demonstrates generating scores in athenaCL and processing them with CsoundAC.
'''
import os
import sys
import traceback
import CsoundAC
from athenaCL.libATH import command
from athenaCL.libATH import athenaObj
from athenaCL.libATH import eventList

interpreter = athenaObj.Interpreter()

script = '''
emo cn
pin a d3,e3,g3,a3,b3,d4,e4,g4,a4,b4,d5,e5,g5,a5,b5
tmo ha
tin a 6 27
tie r pt,(c,16),(ig,(bg,rc,(1,2,3,5,7)),(bg,rc,(3,6,9,12))),(c,1)
tie a om,(ls,e,9,(ru,.2,1),(ru,.2,1)),(wp,e,23,0,0,1)
tie d0 c,0
tie d1 n,100,2,0,14
tie d2 c,1
tie d3 c,1
tie d3 ru,1,4
'''

def toCsoundAC(ao):
    print("toScore...")
    print(ao)
    #~ eln = command.ELn(ao)
    #~ # This definitely works fine, but how in hell to get the actual data 
    #~ # without reading the file?
    #~ # Score writing happens in subclasses of _OutputEngine, e.g. 
    #~ # CsoundNative. But we only care about the score, not the orchestra.
    #~ eln.do()
    #~ #print(eln.result())
    #~ print(eln.display())
    cmdObj = command.ELn(ao)
    ok, msg = cmdObj.do()
    print("Eln:", ok, msg)
    cmdObj = command.ELr(ao)
    ok, msg = cmdObj.do()
    print("Elr:", ok, msg)
    cmdObj = command.ELh(ao)
    ok, msg = cmdObj.do()
    print("ELh:", ok, msg)

    """
    >>> from athenaCL.libATH import athenaObj
    >>> ao = athenaObj.AthenaObject()
    >>> a = EngineCsoundNative('generalMidi', {}, ao)
    """
    #~ # Parameters are: emoObj, map of filenames for output types, the athenaObj.
    #~ engine = eventList.EngineCsoundNative('csoundNative', {'csoundNative': 'junk.csd'}, ao)    
    #~ print(engine)
    #~ engine._translatePoly()
    #~ return engine.polySeqStr
    print("toScore.")
    
for line in script.split("\n"):
    if len(line) > 1:
        print("command:", line)
        print(interpreter.cmd(line))
performer = eventList.Performer()
performer.flattenAll(interpreter.ao)
print(performer.polySeq['a']['esObj'].getArray('ps'))