/*
* C S O U N D   T H R E A D E D
*
* Copyright (c) 2001-2003 by Michael Gogins. All rights reserved.
*
* JSC Csound is a WebExtension for WebKit2 based Web browsers and
* embedded HTML renderers. The JSC Csound library embeds native
* Csound, or at least a useful subset of the C++ Csound API, in
* such browsers.
*
* L I C E N S E
*
* This software is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This software is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this software; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/
#if defined(SWIG)

%begin %{
#ifdef _MSC_VER
#define SWIG_PYTHON_INTERPRETER_NO_DEBUG
#endif
%}

%apply int { size_t };
%include "typemaps.i"

%typemap(in) char ** {
  /* Check if is a list */
  if (PyList_Check($input)) {
    int size = PyList_Size($input);
    int i = 0;
    $1 = (char **) malloc((size+1)*sizeof(char *));
    for (i = 0; i < size; i++) {
      PyObject *o = PyList_GetItem($input,i);
      if (PyString_Check(o))
        $1[i] = PyString_AsString(PyList_GetItem($input,i));
      else {
        PyErr_SetString(PyExc_TypeError,"list must contain strings");
        free($1);
        return NULL;
      }
    }
    $1[i] = 0;
  } else {
    PyErr_SetString(PyExc_TypeError,"not a list");
    return NULL;
  }
}

%typemap(freearg) char ** {
  free((char *) $1);
}


%module CsoundThreaded
%{

#include <string>
#include <vector>
#include <csound.h>
#include <csound.hpp>
#include <csound_threaded.hpp>

%}
%inline 
%{

/** 
 * This is a clean, simple interface to all the parts of the Csound API that I 
 * actually find useful. All types exposed here are elementary C types.
 * This interface does not use varargs or callbacks. Thus, this interface 
 * should work for any language supported by SWIG.
 *
 * The Csound performance loop always runs in a separate thread. This enables 
 * event-driven applications to continue running without being blocked by the 
 * Csound performance. Clients of this class can use its Join method to wait 
 * for the performance thread to end.
 */
class CsoundThread {
protected:
    CsoundThreaded csound_;
public:
    CsoundThread() {};
    CsoundThread(void *csound_ptr) {
        csound_.SetCsound((CSOUND *)csound_ptr);
    };
    virtual ~CsoundThread() {};
    virtual int Cleanup() {
        return csound_.Cleanup();
    }
    virtual int CompileCsd(const char *csd_filepath) {
        return csound_.CompileCsd(csd_filepath);
    }
    virtual int CompileCsdText(const char *csd_text) {
        return csound_.CompileCsdText(csd_text);
    }
    virtual int CompileOrc(const char *orc_text) {
        return csound_.CompileOrc(orc_text);
    }
    virtual void CreateMessageBuffer(int to_stdout) {
        csound_.CreateMessageBuffer(to_stdout);
    }
    virtual double EvalCode(const char *orc_text) {
        return csound_.EvalCode(orc_text);
    }
    virtual double GetControlChannel(const char *name) {
        return csound_.GetControlChannel(name);
    }
    virtual const char* GetFirstMessage() {
        return csound_.GetFirstMessage();
    }
    virtual int GetKsmps() {
        return csound_.GetKsmps();
    }
    virtual int GetMessageCnt() {
        //std::printf("GetMessageCnt: ");
        int count = csound_.GetMessageCnt();
        //std::printf("%d\n", count);
        return count;
    }
    virtual double GetScoreTime() {
        return csound_.GetScoreTime();
    }
    virtual int GetSr() {
        return csound_.GetSr();
    }
    // Probably does not work in Python; the channel value is copied into the 
    // value string, which must be pre-allocated.
    virtual void GetStringChannel(const char *name, char *value) {
        csound_.GetStringChannel(name, value);
    }
    virtual int GetVersion() {
        return csound_.GetVersion();
    }
    virtual void InputMessage(const char *text) {
        csound_.InputMessage(text);
    }
    virtual bool IsPlaying() {
        return csound_.IsPlaying();
    }
    virtual void Join() {
        csound_.Join();
    }
    virtual bool IsScorePending() {
        return csound_.IsScorePending();
    }
    virtual void Message(const char *text) {
        return csound_.Message(text);
    }
    /**
     * Always runs in a separate thread. To wait for it to end,
     * call the Join method.
     */
    virtual int Perform() {
        return csound_.Perform();
    }
    /**
     * Does not run in a separate thread; performs one buffer's worth of audio 
     * output.
     */
    virtual int PerformBuffer() {
        return csound_.PerformBuffer();
    }
    /**
     * Does not run in a separate thread; performs one kperiod's worth of 
     * audio output.
     */
    virtual int PerformKsmps() {
        return csound_.PerformKsmps();
    }
    virtual void PopFirstMessage() {
        csound_.PopFirstMessage();
    }
    virtual int ReadScore(const char *score_text) {
        return csound_.ReadScore(score_text);
    }
    /**
     * Compiles the CSD text, starts Csound, 
     * and performs in a separate thread. To wait for it to end, 
     * call the Join method.
     */
    virtual int Render(const char *csd_text) {
        int result = csound_.CompileCsdText(csd_text);
        result = result + csound_.Start();
        result = result + csound_.Perform();
        return result;
    }
    virtual void Reset() {
        csound_.Reset();
    }
    virtual void RewindScore() {
        csound_.RewindScore();
    }
    virtual int ScoreEvent(char opcode, double *pfields, int pfield_count) {
        return csound_.ScoreEvent(opcode, pfields, pfield_count);
    }
    virtual void SetChannel(const char *name, double value) {
        csound_.SetChannel(name, value);
    }
    virtual void SetControlChannel(const char *name, double value) {
        csound_.SetChannel(name, value);
    }
    virtual void SetStringChannel(const char *name, char *value) {
        csound_.SetStringChannel(name, value);
    }
    virtual void SetOption(const char *option) {
        csound_.SetOption(option);
    }
    virtual void SetScorePending(int pending) {
        csound_.SetScorePending(pending);
    }
    virtual int Start() {
        return csound_.Start();
    }
    virtual void Stop() {
        csound_.Stop();
    }
};

%}

#endif
