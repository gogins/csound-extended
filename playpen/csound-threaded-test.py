import CsoundThreaded
csound = CsoundThreaded.CsoundThread()
csound.CompileCsd("xanadu.csd")
csound.Start()
csound.Perform()
csound.Join()
