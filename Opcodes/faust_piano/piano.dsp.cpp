/* ------------------------------------------------------------
author: "Romain Michon (rmichon@ccrma.stanford.edu)"
copyright: "Romain Michon"
name: "piano"
version: "1.0"
Code generated with Faust 2.20.2 (https://faust.grame.fr)
Compilation options: -lang cpp -double -ftz 0
------------------------------------------------------------ */

#ifndef  __mydsp_H__
#define  __mydsp_H__

/************************************************************************
 IMPORTANT NOTE : this file contains two clearly delimited sections :
 the ARCHITECTURE section (in two parts) and the USER section. Each section
 is governed by its own copyright and license. Please check individually
 each section for license and copyright information.
 *************************************************************************/

/*******************BEGIN ARCHITECTURE SECTION (part 1/2)****************/

/************************************************************************
 FAUST Architecture File
 Copyright (C) 2010-2019 V. Lazzarini and GRAME
 ---------------------------------------------------------------------
 This Architecture section is free software; you can redistribute it
 and/or modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 3 of
 the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; If not, see <http://www.gnu.org/licenses/>.

 EXCEPTION : As a special exception, you may create a larger work
 that contains this FAUST architecture section and distribute
 that work under terms of your choice, so long as this FAUST
 architecture section is not modified.

 ************************************************************************
 ************************************************************************/

//==============================================================================
//
//          CSOUND6 architecture file for FAUST
//          Y. Orlarey & V. Lazzarini
//
//          Usage : faust -uim -a csound.cpp <myfx>.dsp -o <myfx>.cpp
//                  g++ -O3 -DOPCODE_NAME=<myfx> -c <myfx>.cpp -o <myfx>.o
//                  ld -E --shared <myfx>.o -o <myfx>.so
//
//          History :
//          - 28/04/09 : first version
//          - 29/04/09 : dynamic allocation
//          - 10/07/14 : updated to csound6 (Paul Batchelor)
//
//==============================================================================

#include <new>
#include <vector>
#include "csdl.h"                       /* CSOUND plugin API header */

// used to transform a symbol in a string
#define sym(name) xsym(name)
#define xsym(name) #name

// make sure we use csound floats
#define FAUSTFLOAT MYFLT

// we require macro declarations
#define FAUST_UIMACROS

// but we will ignore most of them
#define FAUST_ADDBUTTON(l,f)
#define FAUST_ADDCHECKBOX(l,f)
#define FAUST_ADDVERTICALSLIDER(l,f,i,a,b,s)
#define FAUST_ADDHORIZONTALSLIDER(l,f,i,a,b,s)
#define FAUST_ADDNUMENTRY(l,f,i,a,b,s)
#define FAUST_ADDVERTICALBARGRAPH(l,f,a,b)
#define FAUST_ADDHORIZONTALBARGRAPH(l,f,a,b)

/************************** BEGIN misc.h **************************/
/************************************************************************
 FAUST Architecture File
 Copyright (C) 2003-2017 GRAME, Centre National de Creation Musicale
 ---------------------------------------------------------------------
 This Architecture section is free software; you can redistribute it
 and/or modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 3 of
 the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; If not, see <http://www.gnu.org/licenses/>.
 
 EXCEPTION : As a special exception, you may create a larger work
 that contains this FAUST architecture section and distribute
 that work under terms of your choice, so long as this FAUST
 architecture section is not modified.
 ************************************************************************/

#ifndef __misc__
#define __misc__

#include <algorithm>
#include <map>
#include <cstdlib>
#include <string.h>
#include <fstream>
#include <string>

/************************** BEGIN meta.h **************************/
/************************************************************************
 FAUST Architecture File
 Copyright (C) 2003-2017 GRAME, Centre National de Creation Musicale
 ---------------------------------------------------------------------
 This Architecture section is free software; you can redistribute it
 and/or modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 3 of
 the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; If not, see <http://www.gnu.org/licenses/>.
 
 EXCEPTION : As a special exception, you may create a larger work
 that contains this FAUST architecture section and distribute
 that work under terms of your choice, so long as this FAUST
 architecture section is not modified.
 ************************************************************************/

#ifndef __meta__
#define __meta__

struct Meta
{
    virtual ~Meta() {};
    virtual void declare(const char* key, const char* value) = 0;
    
};

#endif
/**************************  END  meta.h **************************/

using std::max;
using std::min;

struct XXXX_Meta : std::map<const char*, const char*>
{
    void declare(const char* key, const char* value) { (*this)[key] = value; }
};

struct MY_Meta : Meta, std::map<const char*, const char*>
{
    void declare(const char* key, const char* value) { (*this)[key] = value; }
};

static int lsr(int x, int n) { return int(((unsigned int)x) >> n); }

static int int2pow2(int x) { int r = 0; while ((1<<r) < x) r++; return r; }

static long lopt(char* argv[], const char* name, long def)
{
    for (int i = 0; argv[i]; i++) if (!strcmp(argv[i], name)) return std::atoi(argv[i+1]);
    return def;
}

static long lopt1(int argc, char* argv[], const char* longname, const char* shortname, long def)
{
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i-1], shortname) == 0 || strcmp(argv[i-1], longname) == 0) {
            return atoi(argv[i]);
        }
    }
    return def;
}

static const char* lopts(char* argv[], const char* name, const char* def)
{
    for (int i = 0; argv[i]; i++) if (!strcmp(argv[i], name)) return argv[i+1];
    return def;
}

static const char* lopts1(int argc, char* argv[], const char* longname, const char* shortname, const char* def)
{
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i-1], shortname) == 0 || strcmp(argv[i-1], longname) == 0) {
            return argv[i];
        }
    }
    return def;
}

static bool isopt(char* argv[], const char* name)
{
    for (int i = 0; argv[i]; i++) if (!strcmp(argv[i], name)) return true;
    return false;
}

static std::string pathToContent(const std::string& path)
{
    std::ifstream file(path.c_str(), std::ifstream::binary);
    
    file.seekg(0, file.end);
    int size = int(file.tellg());
    file.seekg(0, file.beg);
    
    // And allocate buffer to that a single line can be read...
    char* buffer = new char[size + 1];
    file.read(buffer, size);
    
    // Terminate the string
    buffer[size] = 0;
    std::string result = buffer;
    file.close();
    delete [] buffer;
    return result;
}

#endif

/**************************  END  misc.h **************************/
/************************** BEGIN dsp.h **************************/
/************************************************************************
 FAUST Architecture File
 Copyright (C) 2003-2017 GRAME, Centre National de Creation Musicale
 ---------------------------------------------------------------------
 This Architecture section is free software; you can redistribute it
 and/or modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 3 of
 the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; If not, see <http://www.gnu.org/licenses/>.
 
 EXCEPTION : As a special exception, you may create a larger work
 that contains this FAUST architecture section and distribute
 that work under terms of your choice, so long as this FAUST
 architecture section is not modified.
 ************************************************************************/

#ifndef __dsp__
#define __dsp__

#include <string>
#include <vector>

#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif

class UI;
struct Meta;

/**
 * DSP memory manager.
 */

struct dsp_memory_manager {
    
    virtual ~dsp_memory_manager() {}
    
    virtual void* allocate(size_t size) = 0;
    virtual void destroy(void* ptr) = 0;
    
};

/**
* Signal processor definition.
*/

class dsp {

    public:

        dsp() {}
        virtual ~dsp() {}

        /* Return instance number of audio inputs */
        virtual int getNumInputs() = 0;
    
        /* Return instance number of audio outputs */
        virtual int getNumOutputs() = 0;
    
        /**
         * Trigger the ui_interface parameter with instance specific calls
         * to 'addBtton', 'addVerticalSlider'... in order to build the UI.
         *
         * @param ui_interface - the user interface builder
         */
        virtual void buildUserInterface(UI* ui_interface) = 0;
    
        /* Returns the sample rate currently used by the instance */
        virtual int getSampleRate() = 0;
    
        /**
         * Global init, calls the following methods:
         * - static class 'classInit': static tables initialization
         * - 'instanceInit': constants and instance state initialization
         *
         * @param sample_rate - the sampling rate in Hertz
         */
        virtual void init(int sample_rate) = 0;

        /**
         * Init instance state
         *
         * @param sample_rate - the sampling rate in Hertz
         */
        virtual void instanceInit(int sample_rate) = 0;

        /**
         * Init instance constant state
         *
         * @param sample_rate - the sampling rate in Hertz
         */
        virtual void instanceConstants(int sample_rate) = 0;
    
        /* Init default control parameters values */
        virtual void instanceResetUserInterface() = 0;
    
        /* Init instance state (delay lines...) */
        virtual void instanceClear() = 0;
 
        /**
         * Return a clone of the instance.
         *
         * @return a copy of the instance on success, otherwise a null pointer.
         */
        virtual dsp* clone() = 0;
    
        /**
         * Trigger the Meta* parameter with instance specific calls to 'declare' (key, value) metadata.
         *
         * @param m - the Meta* meta user
         */
        virtual void metadata(Meta* m) = 0;
    
        /**
         * DSP instance computation, to be called with successive in/out audio buffers.
         *
         * @param count - the number of frames to compute
         * @param inputs - the input audio buffers as an array of non-interleaved FAUSTFLOAT samples (eiher float, double or quad)
         * @param outputs - the output audio buffers as an array of non-interleaved FAUSTFLOAT samples (eiher float, double or quad)
         *
         */
        virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) = 0;
    
        /**
         * DSP instance computation: alternative method to be used by subclasses.
         *
         * @param date_usec - the timestamp in microsec given by audio driver.
         * @param count - the number of frames to compute
         * @param inputs - the input audio buffers as an array of non-interleaved FAUSTFLOAT samples (either float, double or quad)
         * @param outputs - the output audio buffers as an array of non-interleaved FAUSTFLOAT samples (either float, double or quad)
         *
         */
        virtual void compute(double /*date_usec*/, int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) { compute(count, inputs, outputs); }
       
};

/**
 * Generic DSP decorator.
 */

class decorator_dsp : public dsp {

    protected:

        dsp* fDSP;

    public:

        decorator_dsp(dsp* dsp = nullptr):fDSP(dsp) {}
        virtual ~decorator_dsp() { delete fDSP; }

        virtual int getNumInputs() { return fDSP->getNumInputs(); }
        virtual int getNumOutputs() { return fDSP->getNumOutputs(); }
        virtual void buildUserInterface(UI* ui_interface) { fDSP->buildUserInterface(ui_interface); }
        virtual int getSampleRate() { return fDSP->getSampleRate(); }
        virtual void init(int sample_rate) { fDSP->init(sample_rate); }
        virtual void instanceInit(int sample_rate) { fDSP->instanceInit(sample_rate); }
        virtual void instanceConstants(int sample_rate) { fDSP->instanceConstants(sample_rate); }
        virtual void instanceResetUserInterface() { fDSP->instanceResetUserInterface(); }
        virtual void instanceClear() { fDSP->instanceClear(); }
        virtual decorator_dsp* clone() { return new decorator_dsp(fDSP->clone()); }
        virtual void metadata(Meta* m) { fDSP->metadata(m); }
        // Beware: subclasses usually have to overload the two 'compute' methods
        virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) { fDSP->compute(count, inputs, outputs); }
        virtual void compute(double date_usec, int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) { fDSP->compute(date_usec, count, inputs, outputs); }
    
};

/**
 * DSP factory class.
 */

class dsp_factory {
    
    protected:
    
        // So that to force sub-classes to use deleteDSPFactory(dsp_factory* factory);
        virtual ~dsp_factory() {}
    
    public:
    
        virtual std::string getName() = 0;
        virtual std::string getSHAKey() = 0;
        virtual std::string getDSPCode() = 0;
        virtual std::string getCompileOptions() = 0;
        virtual std::vector<std::string> getLibraryList() = 0;
        virtual std::vector<std::string> getIncludePathnames() = 0;
    
        virtual dsp* createDSPInstance() = 0;
    
        virtual void setMemoryManager(dsp_memory_manager* manager) = 0;
        virtual dsp_memory_manager* getMemoryManager() = 0;
    
};

/**
 * On Intel set FZ (Flush to Zero) and DAZ (Denormals Are Zero)
 * flags to avoid costly denormals.
 */

#ifdef __SSE__
    #include <xmmintrin.h>
    #ifdef __SSE2__
        #define AVOIDDENORMALS _mm_setcsr(_mm_getcsr() | 0x8040)
    #else
        #define AVOIDDENORMALS _mm_setcsr(_mm_getcsr() | 0x8000)
    #endif
#else
    #define AVOIDDENORMALS
#endif

#endif
/**************************  END  dsp.h **************************/
/************************** BEGIN UI.h **************************/
/************************************************************************
 FAUST Architecture File
 Copyright (C) 2003-2017 GRAME, Centre National de Creation Musicale
 ---------------------------------------------------------------------
 This Architecture section is free software; you can redistribute it
 and/or modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 3 of
 the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; If not, see <http://www.gnu.org/licenses/>.
 
 EXCEPTION : As a special exception, you may create a larger work
 that contains this FAUST architecture section and distribute
 that work under terms of your choice, so long as this FAUST
 architecture section is not modified.
 ************************************************************************/

#ifndef __UI_H__
#define __UI_H__

#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif

/*******************************************************************************
 * UI : Faust DSP User Interface
 * User Interface as expected by the buildUserInterface() method of a DSP.
 * This abstract class contains only the method that the Faust compiler can
 * generate to describe a DSP user interface.
 ******************************************************************************/

struct Soundfile;

template <typename REAL>
class UIReal
{
    
    public:
        
        UIReal() {}
        virtual ~UIReal() {}
        
        // -- widget's layouts
        
        virtual void openTabBox(const char* label) = 0;
        virtual void openHorizontalBox(const char* label) = 0;
        virtual void openVerticalBox(const char* label) = 0;
        virtual void closeBox() = 0;
        
        // -- active widgets
        
        virtual void addButton(const char* label, REAL* zone) = 0;
        virtual void addCheckButton(const char* label, REAL* zone) = 0;
        virtual void addVerticalSlider(const char* label, REAL* zone, REAL init, REAL min, REAL max, REAL step) = 0;
        virtual void addHorizontalSlider(const char* label, REAL* zone, REAL init, REAL min, REAL max, REAL step) = 0;
        virtual void addNumEntry(const char* label, REAL* zone, REAL init, REAL min, REAL max, REAL step) = 0;
        
        // -- passive widgets
        
        virtual void addHorizontalBargraph(const char* label, REAL* zone, REAL min, REAL max) = 0;
        virtual void addVerticalBargraph(const char* label, REAL* zone, REAL min, REAL max) = 0;
        
        // -- soundfiles
        
        virtual void addSoundfile(const char* label, const char* filename, Soundfile** sf_zone) = 0;
        
        // -- metadata declarations
        
        virtual void declare(REAL* zone, const char* key, const char* val) {}
};

class UI : public UIReal<FAUSTFLOAT>
{

    public:

        UI() {}
        virtual ~UI() {}
};

#endif
/**************************  END  UI.h **************************/

/**
 * A UI that simply collects the active zones in a vector
 * and provides a method to copy the csound controls
 */
class CSUI : public UI
{
    std::vector<FAUSTFLOAT*>  vZone;

public:

    // -- widget's layouts

    virtual void openTabBox(const char* label)                                                          {}
    virtual void openHorizontalBox(const char* label)                                                   {}
    virtual void openVerticalBox(const char* label)                                                     {}
    virtual void closeBox()                                                                             {}

    // -- active widgets

    virtual void addButton(const char* label, FAUSTFLOAT* zone)                                                          { vZone.push_back(zone); }
    virtual void addCheckButton(const char* label, FAUSTFLOAT* zone)                                                     { vZone.push_back(zone); }
    virtual void addVerticalSlider(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)    { vZone.push_back(zone); }
    virtual void addHorizontalSlider(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)  { vZone.push_back(zone); }
    virtual void addNumEntry(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)          { vZone.push_back(zone); }

    // -- passive widgets

    virtual void addHorizontalBargraph(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT min, FAUSTFLOAT max) {}
    virtual void addVerticalBargraph(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT min, FAUSTFLOAT max)   {}

    virtual void addSoundfile(const char* label, const char* filename, Soundfile** sf_zone) {}

    void copyfrom(MYFLT* mem[]) {
        for (unsigned int i = 0; i < vZone.size(); i++) {
            if (*(mem[i]) != FL(-1.)) {
                *vZone[i] = *(mem[i]);
            }
        }
    }

    int size() { return vZone.size(); }
};

/******************************************************************************
 *******************************************************************************
 
 VECTOR INTRINSICS
 
 *******************************************************************************
 *******************************************************************************/


/********************END ARCHITECTURE SECTION (part 1/2)****************/

/**************************BEGIN USER SECTION **************************/

#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif 

/* link with : "" */
#include <algorithm>
#include <cmath>
#include <math.h>
#include <piano.h>

static double mydsp_faustpower2_f(double value) {
	return (value * value);
}

#ifndef FAUSTCLASS 
#define FAUSTCLASS mydsp
#endif

#ifdef __APPLE__ 
#define exp10f __exp10f
#define exp10 __exp10
#endif

class mydsp : public dsp {
	
 public:
	
	int fSampleRate;
	double fConst0;
	double fConst1;
	double fConst2;
	double fConst3;
	double fConst4;
	FAUSTFLOAT fHslider0;
	double fConst5;
	double fConst6;
	double fConst7;
	double fConst8;
	double fRec11[2];
	double fRec10[2];
	int IOTA;
	double fVec0[32768];
	double fConst9;
	int iConst10;
	FAUSTFLOAT fHslider1;
	FAUSTFLOAT fHslider2;
	double fRec12[2];
	double fConst11;
	FAUSTFLOAT fEntry0;
	double fConst12;
	FAUSTFLOAT fHslider3;
	double fConst13;
	FAUSTFLOAT fHslider4;
	FAUSTFLOAT fButton0;
	double fRec20[2];
	FAUSTFLOAT fHslider5;
	int iRec26[2];
	double fRec28[2];
	double fConst14;
	FAUSTFLOAT fEntry1;
	double fRec27[2];
	double fConst15;
	double fConst16;
	FAUSTFLOAT fHslider6;
	double fConst17;
	double fConst18;
	double fRec29[2];
	double fRec25[2];
	double fRec24[2];
	double fRec23[2];
	double fRec22[2];
	double fRec21[2];
	double fVec1[2];
	double fRec19[2];
	double fRec18[2];
	double fRec17[8192];
	double fVec2[2];
	double fRec33[2];
	double fRec32[2];
	double fRec31[8192];
	double fVec3[2];
	double fRec30[2];
	double fRec14[2];
	double fRec15[2];
	double fConst19;
	double fRec13[3];
	double fConst20;
	double fVec4[2];
	double fVec5[2];
	double fRec43[2];
	double fRec42[2];
	double fRec41[2];
	double fRec40[2];
	double fRec39[2];
	double fRec38[3];
	double fRec37[3];
	double fRec36[3];
	double fRec35[3];
	double fRec34[2];
	double fVec6[8192];
	double fConst21;
	FAUSTFLOAT fHslider7;
	double fVec7[4096];
	int iConst22;
	double fVec8[2048];
	int iConst23;
	double fRec8[2];
	double fConst24;
	double fConst25;
	double fConst26;
	double fConst27;
	double fRec47[2];
	double fRec46[2];
	double fVec9[32768];
	double fConst28;
	int iConst29;
	double fVec10[4096];
	int iConst30;
	double fRec44[2];
	double fConst31;
	double fConst32;
	double fConst33;
	double fConst34;
	double fRec51[2];
	double fRec50[2];
	double fVec11[16384];
	double fConst35;
	int iConst36;
	double fVec12[4096];
	int iConst37;
	double fRec48[2];
	double fConst38;
	double fConst39;
	double fConst40;
	double fConst41;
	double fRec55[2];
	double fRec54[2];
	double fVec13[32768];
	double fConst42;
	int iConst43;
	double fVec14[4096];
	int iConst44;
	double fRec52[2];
	double fConst45;
	double fConst46;
	double fConst47;
	double fConst48;
	double fRec59[2];
	double fRec58[2];
	double fVec15[16384];
	double fConst49;
	int iConst50;
	double fVec16[4096];
	double fVec17[2048];
	int iConst51;
	double fRec56[2];
	double fConst52;
	double fConst53;
	double fConst54;
	double fConst55;
	double fRec63[2];
	double fRec62[2];
	double fVec18[16384];
	double fConst56;
	int iConst57;
	double fVec19[4096];
	int iConst58;
	double fRec60[2];
	double fConst59;
	double fConst60;
	double fConst61;
	double fConst62;
	double fRec67[2];
	double fRec66[2];
	double fVec20[16384];
	double fConst63;
	int iConst64;
	double fVec21[4096];
	int iConst65;
	double fRec64[2];
	double fConst66;
	double fConst67;
	double fConst68;
	double fConst69;
	double fRec71[2];
	double fRec70[2];
	double fVec22[16384];
	double fConst70;
	int iConst71;
	double fVec23[2048];
	int iConst72;
	double fRec68[2];
	double fRec0[3];
	double fRec1[3];
	double fRec2[3];
	double fRec3[3];
	double fRec4[3];
	double fRec5[3];
	double fRec6[3];
	double fRec7[3];
	
 public:
	
	void metadata(Meta* m) { 
		m->declare("author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("basics.lib/name", "Faust Basic Element Library");
		m->declare("basics.lib/version", "0.1");
		m->declare("copyright", "Romain Michon");
		m->declare("delays.lib/name", "Faust Delay Library");
		m->declare("delays.lib/version", "0.1");
		m->declare("description", "WaveGuide Commuted Piano");
		m->declare("filename", "piano.dsp");
		m->declare("filters.lib/allpass_comb:author", "Julius O. Smith III");
		m->declare("filters.lib/allpass_comb:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/allpass_comb:license", "MIT-style STK-4.3 license");
		m->declare("filters.lib/lowpass0_highpass1", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/lowpass0_highpass1:author", "Julius O. Smith III");
		m->declare("filters.lib/lowpass:author", "Julius O. Smith III");
		m->declare("filters.lib/lowpass:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/lowpass:license", "MIT-style STK-4.3 license");
		m->declare("filters.lib/name", "Faust Filters Library");
		m->declare("filters.lib/tf1:author", "Julius O. Smith III");
		m->declare("filters.lib/tf1:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/tf1:license", "MIT-style STK-4.3 license");
		m->declare("filters.lib/tf1s:author", "Julius O. Smith III");
		m->declare("filters.lib/tf1s:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/tf1s:license", "MIT-style STK-4.3 license");
		m->declare("instruments.lib/author", "Romain Michon (rmichon@ccrma.stanford.edu)");
		m->declare("instruments.lib/copyright", "Romain Michon");
		m->declare("instruments.lib/licence", "STK-4.3");
		m->declare("instruments.lib/name", "Faust-STK Tools Library");
		m->declare("instruments.lib/version", "1.0");
		m->declare("licence", "STK-4.3");
		m->declare("maths.lib/author", "GRAME");
		m->declare("maths.lib/copyright", "GRAME");
		m->declare("maths.lib/license", "LGPL with exception");
		m->declare("maths.lib/name", "Faust Math Library");
		m->declare("maths.lib/version", "2.1");
		m->declare("name", "piano");
		m->declare("noises.lib/name", "Faust Noise Generator Library");
		m->declare("noises.lib/version", "0.0");
		m->declare("reverbs.lib/name", "Faust Reverb Library");
		m->declare("reverbs.lib/version", "0.0");
		m->declare("routes.lib/name", "Faust Signal Routing Library");
		m->declare("routes.lib/version", "0.1");
		m->declare("signals.lib/name", "Faust Signal Routing Library");
		m->declare("signals.lib/version", "0.0");
		m->declare("version", "1.0");
	}

	virtual int getNumInputs() {
		return 0;
	}
	virtual int getNumOutputs() {
		return 2;
	}
	virtual int getInputRate(int channel) {
		int rate;
		switch ((channel)) {
			default: {
				rate = -1;
				break;
			}
		}
		return rate;
	}
	virtual int getOutputRate(int channel) {
		int rate;
		switch ((channel)) {
			case 0: {
				rate = 1;
				break;
			}
			case 1: {
				rate = 1;
				break;
			}
			default: {
				rate = -1;
				break;
			}
		}
		return rate;
	}
	
	static void classInit(int sample_rate) {
	}
	
	virtual void instanceConstants(int sample_rate) {
		fSampleRate = sample_rate;
		fConst0 = std::min<double>(192000.0, std::max<double>(1.0, double(fSampleRate)));
		fConst1 = std::cos((37699.111843077517 / fConst0));
		fConst2 = std::floor(((0.21999099999999999 * fConst0) + 0.5));
		fConst3 = ((0.0 - (6.9077552789821377 * fConst2)) / fConst0);
		fConst4 = (0.5 * fConst3);
		fConst5 = (0.33333333333333331 * fConst3);
		fConst6 = (1.0 / std::tan((628.31853071795865 / fConst0)));
		fConst7 = (1.0 / (fConst6 + 1.0));
		fConst8 = (1.0 - fConst6);
		fConst9 = std::floor(((0.019123000000000001 * fConst0) + 0.5));
		iConst10 = int(std::min<double>(16384.0, std::max<double>(0.0, (fConst2 - fConst9))));
		fConst11 = (1.0 / mydsp_faustpower2_f(fConst0));
		fConst12 = (0.15915494309189535 * fConst0);
		fConst13 = (6.2831853071795862 / fConst0);
		fConst14 = (7.0 / fConst0);
		fConst15 = std::exp((0.0 - (5.0 / fConst0)));
		fConst16 = (10.0 / fConst0);
		fConst17 = (0.10000000000000001 * fConst0);
		fConst18 = std::exp((0.0 - (0.5 / fConst0)));
		fConst19 = (2.0 / fConst0);
		fConst20 = (0.050000000000000003 / fConst0);
		fConst21 = (0.5 * fConst0);
		iConst22 = int(std::min<double>(8192.0, std::max<double>(0.0, (0.02 * fConst0))));
		iConst23 = int(std::min<double>(1024.0, std::max<double>(0.0, (fConst9 + -1.0))));
		fConst24 = std::floor(((0.25689099999999998 * fConst0) + 0.5));
		fConst25 = ((0.0 - (6.9077552789821377 * fConst24)) / fConst0);
		fConst26 = (0.5 * fConst25);
		fConst27 = (0.33333333333333331 * fConst25);
		fConst28 = std::floor(((0.027333 * fConst0) + 0.5));
		iConst29 = int(std::min<double>(16384.0, std::max<double>(0.0, (fConst24 - fConst28))));
		iConst30 = int(std::min<double>(2048.0, std::max<double>(0.0, (fConst28 + -1.0))));
		fConst31 = std::floor(((0.192303 * fConst0) + 0.5));
		fConst32 = ((0.0 - (6.9077552789821377 * fConst31)) / fConst0);
		fConst33 = (0.5 * fConst32);
		fConst34 = (0.33333333333333331 * fConst32);
		fConst35 = std::floor(((0.029291000000000001 * fConst0) + 0.5));
		iConst36 = int(std::min<double>(8192.0, std::max<double>(0.0, (fConst31 - fConst35))));
		iConst37 = int(std::min<double>(2048.0, std::max<double>(0.0, (fConst35 + -1.0))));
		fConst38 = std::floor(((0.21038899999999999 * fConst0) + 0.5));
		fConst39 = ((0.0 - (6.9077552789821377 * fConst38)) / fConst0);
		fConst40 = (0.5 * fConst39);
		fConst41 = (0.33333333333333331 * fConst39);
		fConst42 = std::floor(((0.024421000000000002 * fConst0) + 0.5));
		iConst43 = int(std::min<double>(16384.0, std::max<double>(0.0, (fConst38 - fConst42))));
		iConst44 = int(std::min<double>(2048.0, std::max<double>(0.0, (fConst42 + -1.0))));
		fConst45 = std::floor(((0.125 * fConst0) + 0.5));
		fConst46 = ((0.0 - (6.9077552789821377 * fConst45)) / fConst0);
		fConst47 = (0.5 * fConst46);
		fConst48 = (0.33333333333333331 * fConst46);
		fConst49 = std::floor(((0.013457999999999999 * fConst0) + 0.5));
		iConst50 = int(std::min<double>(8192.0, std::max<double>(0.0, (fConst45 - fConst49))));
		iConst51 = int(std::min<double>(1024.0, std::max<double>(0.0, (fConst49 + -1.0))));
		fConst52 = std::floor(((0.12783700000000001 * fConst0) + 0.5));
		fConst53 = ((0.0 - (6.9077552789821377 * fConst52)) / fConst0);
		fConst54 = (0.5 * fConst53);
		fConst55 = (0.33333333333333331 * fConst53);
		fConst56 = std::floor(((0.031604 * fConst0) + 0.5));
		iConst57 = int(std::min<double>(8192.0, std::max<double>(0.0, (fConst52 - fConst56))));
		iConst58 = int(std::min<double>(2048.0, std::max<double>(0.0, (fConst56 + -1.0))));
		fConst59 = std::floor(((0.17471300000000001 * fConst0) + 0.5));
		fConst60 = ((0.0 - (6.9077552789821377 * fConst59)) / fConst0);
		fConst61 = (0.5 * fConst60);
		fConst62 = (0.33333333333333331 * fConst60);
		fConst63 = std::floor(((0.022904000000000001 * fConst0) + 0.5));
		iConst64 = int(std::min<double>(8192.0, std::max<double>(0.0, (fConst59 - fConst63))));
		iConst65 = int(std::min<double>(2048.0, std::max<double>(0.0, (fConst63 + -1.0))));
		fConst66 = std::floor(((0.15312899999999999 * fConst0) + 0.5));
		fConst67 = ((0.0 - (6.9077552789821377 * fConst66)) / fConst0);
		fConst68 = (0.5 * fConst67);
		fConst69 = (0.33333333333333331 * fConst67);
		fConst70 = std::floor(((0.020346 * fConst0) + 0.5));
		iConst71 = int(std::min<double>(8192.0, std::max<double>(0.0, (fConst66 - fConst70))));
		iConst72 = int(std::min<double>(1024.0, std::max<double>(0.0, (fConst70 + -1.0))));
	}
	
	virtual void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.71999999999999997);
		fHslider1 = FAUSTFLOAT(0.59999999999999998);
		fHslider2 = FAUSTFLOAT(0.13700000000000001);
		fEntry0 = FAUSTFLOAT(440.0);
		fHslider3 = FAUSTFLOAT(0.28000000000000003);
		fHslider4 = FAUSTFLOAT(0.10000000000000001);
		fButton0 = FAUSTFLOAT(0.0);
		fHslider5 = FAUSTFLOAT(0.0);
		fEntry1 = FAUSTFLOAT(1.0);
		fHslider6 = FAUSTFLOAT(0.10000000000000001);
		fHslider7 = FAUSTFLOAT(0.5);
	}
	
	virtual void instanceClear() {
		for (int l0 = 0; (l0 < 2); l0 = (l0 + 1)) {
			fRec11[l0] = 0.0;
		}
		for (int l1 = 0; (l1 < 2); l1 = (l1 + 1)) {
			fRec10[l1] = 0.0;
		}
		IOTA = 0;
		for (int l2 = 0; (l2 < 32768); l2 = (l2 + 1)) {
			fVec0[l2] = 0.0;
		}
		for (int l3 = 0; (l3 < 2); l3 = (l3 + 1)) {
			fRec12[l3] = 0.0;
		}
		for (int l4 = 0; (l4 < 2); l4 = (l4 + 1)) {
			fRec20[l4] = 0.0;
		}
		for (int l5 = 0; (l5 < 2); l5 = (l5 + 1)) {
			iRec26[l5] = 0;
		}
		for (int l6 = 0; (l6 < 2); l6 = (l6 + 1)) {
			fRec28[l6] = 0.0;
		}
		for (int l7 = 0; (l7 < 2); l7 = (l7 + 1)) {
			fRec27[l7] = 0.0;
		}
		for (int l8 = 0; (l8 < 2); l8 = (l8 + 1)) {
			fRec29[l8] = 0.0;
		}
		for (int l9 = 0; (l9 < 2); l9 = (l9 + 1)) {
			fRec25[l9] = 0.0;
		}
		for (int l10 = 0; (l10 < 2); l10 = (l10 + 1)) {
			fRec24[l10] = 0.0;
		}
		for (int l11 = 0; (l11 < 2); l11 = (l11 + 1)) {
			fRec23[l11] = 0.0;
		}
		for (int l12 = 0; (l12 < 2); l12 = (l12 + 1)) {
			fRec22[l12] = 0.0;
		}
		for (int l13 = 0; (l13 < 2); l13 = (l13 + 1)) {
			fRec21[l13] = 0.0;
		}
		for (int l14 = 0; (l14 < 2); l14 = (l14 + 1)) {
			fVec1[l14] = 0.0;
		}
		for (int l15 = 0; (l15 < 2); l15 = (l15 + 1)) {
			fRec19[l15] = 0.0;
		}
		for (int l16 = 0; (l16 < 2); l16 = (l16 + 1)) {
			fRec18[l16] = 0.0;
		}
		for (int l17 = 0; (l17 < 8192); l17 = (l17 + 1)) {
			fRec17[l17] = 0.0;
		}
		for (int l18 = 0; (l18 < 2); l18 = (l18 + 1)) {
			fVec2[l18] = 0.0;
		}
		for (int l19 = 0; (l19 < 2); l19 = (l19 + 1)) {
			fRec33[l19] = 0.0;
		}
		for (int l20 = 0; (l20 < 2); l20 = (l20 + 1)) {
			fRec32[l20] = 0.0;
		}
		for (int l21 = 0; (l21 < 8192); l21 = (l21 + 1)) {
			fRec31[l21] = 0.0;
		}
		for (int l22 = 0; (l22 < 2); l22 = (l22 + 1)) {
			fVec3[l22] = 0.0;
		}
		for (int l23 = 0; (l23 < 2); l23 = (l23 + 1)) {
			fRec30[l23] = 0.0;
		}
		for (int l24 = 0; (l24 < 2); l24 = (l24 + 1)) {
			fRec14[l24] = 0.0;
		}
		for (int l25 = 0; (l25 < 2); l25 = (l25 + 1)) {
			fRec15[l25] = 0.0;
		}
		for (int l26 = 0; (l26 < 3); l26 = (l26 + 1)) {
			fRec13[l26] = 0.0;
		}
		for (int l27 = 0; (l27 < 2); l27 = (l27 + 1)) {
			fVec4[l27] = 0.0;
		}
		for (int l28 = 0; (l28 < 2); l28 = (l28 + 1)) {
			fVec5[l28] = 0.0;
		}
		for (int l29 = 0; (l29 < 2); l29 = (l29 + 1)) {
			fRec43[l29] = 0.0;
		}
		for (int l30 = 0; (l30 < 2); l30 = (l30 + 1)) {
			fRec42[l30] = 0.0;
		}
		for (int l31 = 0; (l31 < 2); l31 = (l31 + 1)) {
			fRec41[l31] = 0.0;
		}
		for (int l32 = 0; (l32 < 2); l32 = (l32 + 1)) {
			fRec40[l32] = 0.0;
		}
		for (int l33 = 0; (l33 < 2); l33 = (l33 + 1)) {
			fRec39[l33] = 0.0;
		}
		for (int l34 = 0; (l34 < 3); l34 = (l34 + 1)) {
			fRec38[l34] = 0.0;
		}
		for (int l35 = 0; (l35 < 3); l35 = (l35 + 1)) {
			fRec37[l35] = 0.0;
		}
		for (int l36 = 0; (l36 < 3); l36 = (l36 + 1)) {
			fRec36[l36] = 0.0;
		}
		for (int l37 = 0; (l37 < 3); l37 = (l37 + 1)) {
			fRec35[l37] = 0.0;
		}
		for (int l38 = 0; (l38 < 2); l38 = (l38 + 1)) {
			fRec34[l38] = 0.0;
		}
		for (int l39 = 0; (l39 < 8192); l39 = (l39 + 1)) {
			fVec6[l39] = 0.0;
		}
		for (int l40 = 0; (l40 < 4096); l40 = (l40 + 1)) {
			fVec7[l40] = 0.0;
		}
		for (int l41 = 0; (l41 < 2048); l41 = (l41 + 1)) {
			fVec8[l41] = 0.0;
		}
		for (int l42 = 0; (l42 < 2); l42 = (l42 + 1)) {
			fRec8[l42] = 0.0;
		}
		for (int l43 = 0; (l43 < 2); l43 = (l43 + 1)) {
			fRec47[l43] = 0.0;
		}
		for (int l44 = 0; (l44 < 2); l44 = (l44 + 1)) {
			fRec46[l44] = 0.0;
		}
		for (int l45 = 0; (l45 < 32768); l45 = (l45 + 1)) {
			fVec9[l45] = 0.0;
		}
		for (int l46 = 0; (l46 < 4096); l46 = (l46 + 1)) {
			fVec10[l46] = 0.0;
		}
		for (int l47 = 0; (l47 < 2); l47 = (l47 + 1)) {
			fRec44[l47] = 0.0;
		}
		for (int l48 = 0; (l48 < 2); l48 = (l48 + 1)) {
			fRec51[l48] = 0.0;
		}
		for (int l49 = 0; (l49 < 2); l49 = (l49 + 1)) {
			fRec50[l49] = 0.0;
		}
		for (int l50 = 0; (l50 < 16384); l50 = (l50 + 1)) {
			fVec11[l50] = 0.0;
		}
		for (int l51 = 0; (l51 < 4096); l51 = (l51 + 1)) {
			fVec12[l51] = 0.0;
		}
		for (int l52 = 0; (l52 < 2); l52 = (l52 + 1)) {
			fRec48[l52] = 0.0;
		}
		for (int l53 = 0; (l53 < 2); l53 = (l53 + 1)) {
			fRec55[l53] = 0.0;
		}
		for (int l54 = 0; (l54 < 2); l54 = (l54 + 1)) {
			fRec54[l54] = 0.0;
		}
		for (int l55 = 0; (l55 < 32768); l55 = (l55 + 1)) {
			fVec13[l55] = 0.0;
		}
		for (int l56 = 0; (l56 < 4096); l56 = (l56 + 1)) {
			fVec14[l56] = 0.0;
		}
		for (int l57 = 0; (l57 < 2); l57 = (l57 + 1)) {
			fRec52[l57] = 0.0;
		}
		for (int l58 = 0; (l58 < 2); l58 = (l58 + 1)) {
			fRec59[l58] = 0.0;
		}
		for (int l59 = 0; (l59 < 2); l59 = (l59 + 1)) {
			fRec58[l59] = 0.0;
		}
		for (int l60 = 0; (l60 < 16384); l60 = (l60 + 1)) {
			fVec15[l60] = 0.0;
		}
		for (int l61 = 0; (l61 < 4096); l61 = (l61 + 1)) {
			fVec16[l61] = 0.0;
		}
		for (int l62 = 0; (l62 < 2048); l62 = (l62 + 1)) {
			fVec17[l62] = 0.0;
		}
		for (int l63 = 0; (l63 < 2); l63 = (l63 + 1)) {
			fRec56[l63] = 0.0;
		}
		for (int l64 = 0; (l64 < 2); l64 = (l64 + 1)) {
			fRec63[l64] = 0.0;
		}
		for (int l65 = 0; (l65 < 2); l65 = (l65 + 1)) {
			fRec62[l65] = 0.0;
		}
		for (int l66 = 0; (l66 < 16384); l66 = (l66 + 1)) {
			fVec18[l66] = 0.0;
		}
		for (int l67 = 0; (l67 < 4096); l67 = (l67 + 1)) {
			fVec19[l67] = 0.0;
		}
		for (int l68 = 0; (l68 < 2); l68 = (l68 + 1)) {
			fRec60[l68] = 0.0;
		}
		for (int l69 = 0; (l69 < 2); l69 = (l69 + 1)) {
			fRec67[l69] = 0.0;
		}
		for (int l70 = 0; (l70 < 2); l70 = (l70 + 1)) {
			fRec66[l70] = 0.0;
		}
		for (int l71 = 0; (l71 < 16384); l71 = (l71 + 1)) {
			fVec20[l71] = 0.0;
		}
		for (int l72 = 0; (l72 < 4096); l72 = (l72 + 1)) {
			fVec21[l72] = 0.0;
		}
		for (int l73 = 0; (l73 < 2); l73 = (l73 + 1)) {
			fRec64[l73] = 0.0;
		}
		for (int l74 = 0; (l74 < 2); l74 = (l74 + 1)) {
			fRec71[l74] = 0.0;
		}
		for (int l75 = 0; (l75 < 2); l75 = (l75 + 1)) {
			fRec70[l75] = 0.0;
		}
		for (int l76 = 0; (l76 < 16384); l76 = (l76 + 1)) {
			fVec22[l76] = 0.0;
		}
		for (int l77 = 0; (l77 < 2048); l77 = (l77 + 1)) {
			fVec23[l77] = 0.0;
		}
		for (int l78 = 0; (l78 < 2); l78 = (l78 + 1)) {
			fRec68[l78] = 0.0;
		}
		for (int l79 = 0; (l79 < 3); l79 = (l79 + 1)) {
			fRec0[l79] = 0.0;
		}
		for (int l80 = 0; (l80 < 3); l80 = (l80 + 1)) {
			fRec1[l80] = 0.0;
		}
		for (int l81 = 0; (l81 < 3); l81 = (l81 + 1)) {
			fRec2[l81] = 0.0;
		}
		for (int l82 = 0; (l82 < 3); l82 = (l82 + 1)) {
			fRec3[l82] = 0.0;
		}
		for (int l83 = 0; (l83 < 3); l83 = (l83 + 1)) {
			fRec4[l83] = 0.0;
		}
		for (int l84 = 0; (l84 < 3); l84 = (l84 + 1)) {
			fRec5[l84] = 0.0;
		}
		for (int l85 = 0; (l85 < 3); l85 = (l85 + 1)) {
			fRec6[l85] = 0.0;
		}
		for (int l86 = 0; (l86 < 3); l86 = (l86 + 1)) {
			fRec7[l86] = 0.0;
		}
	}
	
	virtual void init(int sample_rate) {
		classInit(sample_rate);
		instanceInit(sample_rate);
	}
	virtual void instanceInit(int sample_rate) {
		instanceConstants(sample_rate);
		instanceResetUserInterface();
		instanceClear();
	}
	
	virtual mydsp* clone() {
		return new mydsp();
	}
	
	virtual int getSampleRate() {
		return fSampleRate;
	}
	
	virtual void buildUserInterface(UI* ui_interface) {
		ui_interface->openVerticalBox("piano");
		ui_interface->openHorizontalBox("Basic_Parameters");
		ui_interface->declare(&fEntry0, "1", "");
		ui_interface->declare(&fEntry0, "tooltip", "Tone frequency");
		ui_interface->declare(&fEntry0, "unit", "Hz");
		ui_interface->addNumEntry("freq", &fEntry0, 440.0, 20.0, 20000.0, 1.0);
		ui_interface->declare(&fEntry1, "1", "");
		ui_interface->declare(&fEntry1, "tooltip", "Gain (value between 0 and 1)");
		ui_interface->addNumEntry("gain", &fEntry1, 1.0, 0.0, 1.0, 0.01);
		ui_interface->declare(&fButton0, "1", "");
		ui_interface->declare(&fButton0, "tooltip", "noteOn = 1, noteOff = 0");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Physical_Parameters");
		ui_interface->declare(&fHslider5, "2", "");
		ui_interface->declare(&fHslider5, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Brightness_Factor", &fHslider5, 0.0, 0.0, 1.0, 0.01);
		ui_interface->declare(&fHslider4, "2", "");
		ui_interface->declare(&fHslider4, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Detuning_Factor", &fHslider4, 0.10000000000000001, 0.0, 1.0, 0.01);
		ui_interface->declare(&fHslider6, "2", "");
		ui_interface->declare(&fHslider6, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Hammer_Hardness", &fHslider6, 0.10000000000000001, 0.0, 1.0, 0.01);
		ui_interface->declare(&fHslider3, "2", "");
		ui_interface->declare(&fHslider3, "tooltip", "A value between 0 and 1");
		ui_interface->addHorizontalSlider("Stiffness_Factor", &fHslider3, 0.28000000000000003, 0.0, 1.0, 0.01);
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Reverb");
		ui_interface->addHorizontalSlider("reverbGain", &fHslider2, 0.13700000000000001, 0.0, 1.0, 0.01);
		ui_interface->addHorizontalSlider("roomSize", &fHslider0, 0.71999999999999997, 0.01, 2.0, 0.01);
		ui_interface->closeBox();
		ui_interface->openVerticalBox("Spat");
		ui_interface->addHorizontalSlider("pan angle", &fHslider1, 0.59999999999999998, 0.0, 1.0, 0.01);
		ui_interface->addHorizontalSlider("spatial width", &fHslider7, 0.5, 0.0, 1.0, 0.01);
		ui_interface->closeBox();
		ui_interface->closeBox();
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		double fSlow0 = double(fHslider0);
		double fSlow1 = std::exp((fConst4 / fSlow0));
		double fSlow2 = mydsp_faustpower2_f(fSlow1);
		double fSlow3 = (1.0 - (fConst1 * fSlow2));
		double fSlow4 = (1.0 - fSlow2);
		double fSlow5 = (fSlow3 / fSlow4);
		double fSlow6 = std::sqrt(std::max<double>(0.0, ((mydsp_faustpower2_f(fSlow3) / mydsp_faustpower2_f(fSlow4)) + -1.0)));
		double fSlow7 = (fSlow5 - fSlow6);
		double fSlow8 = (fSlow1 * (fSlow6 + (1.0 - fSlow5)));
		double fSlow9 = ((std::exp((fConst5 / fSlow0)) / fSlow1) + -1.0);
		double fSlow10 = double(fHslider1);
		double fSlow11 = (12.0 * fSlow10);
		double fSlow12 = (0.0010000000000000009 * double(fHslider2));
		double fSlow13 = double(fEntry0);
		int iSlow14 = int(((17.312340490667559 * (std::log(fSlow13) + -6.0867747269123065)) + 69.5));
		double fSlow15 = double(getValueEQBandWidthFactor(double(iSlow14)));
		double fSlow16 = (fConst11 * (mydsp_faustpower2_f(fSlow13) * mydsp_faustpower2_f(fSlow15)));
		double fSlow17 = (0.5 * (1.0 - fSlow16));
		double fSlow18 = double(getValueEQGain(double(iSlow14)));
		double fSlow19 = double(fHslider3);
		double fSlow20 = double(getValueStiffnessCoefficient(double(iSlow14)));
		double fSlow21 = (13.690000000000001 * (mydsp_faustpower2_f(fSlow19) * mydsp_faustpower2_f(fSlow20)));
		double fSlow22 = (fSlow21 + -1.0);
		double fSlow23 = (5.0 * (double(fHslider4) * double(getValueDetuningHz(double(iSlow14)))));
		double fSlow24 = (fSlow13 + fSlow23);
		double fSlow25 = (fConst13 * fSlow24);
		double fSlow26 = std::sin(fSlow25);
		double fSlow27 = (fSlow19 * fSlow20);
		double fSlow28 = (7.4000000000000004 * fSlow27);
		double fSlow29 = (fSlow21 + 1.0);
		double fSlow30 = std::cos(fSlow25);
		double fSlow31 = (3.0 * std::atan2((fSlow22 * fSlow26), (fSlow28 + (fSlow29 * fSlow30))));
		double fSlow32 = std::pow(10.0, (0.050000000000000003 * (double(getValueSingleStringDecayRate(double(iSlow14))) / fSlow13)));
		double fSlow33 = double(getValueSingleStringZero(double(iSlow14)));
		double fSlow34 = double(getValueSingleStringPole(double(iSlow14)));
		double fSlow35 = (1.0 - fSlow34);
		double fSlow36 = ((fSlow32 * fSlow33) * fSlow35);
		double fSlow37 = (1.0 - fSlow33);
		double fSlow38 = (fSlow34 * fSlow37);
		double fSlow39 = (3.0 * fSlow38);
		double fSlow40 = (fSlow36 - fSlow39);
		double fSlow41 = (fSlow38 - fSlow36);
		double fSlow42 = (4.0 * fSlow41);
		double fSlow43 = (fSlow40 + fSlow42);
		double fSlow44 = (fSlow32 * fSlow35);
		double fSlow45 = ((3.0 * fSlow37) - fSlow44);
		double fSlow46 = (((fSlow30 * fSlow40) / fSlow45) + 1.0);
		double fSlow47 = ((fSlow33 + fSlow44) + -1.0);
		double fSlow48 = (4.0 * fSlow47);
		double fSlow49 = (((fSlow48 + (fSlow30 * fSlow43)) / fSlow45) + 1.0);
		double fSlow50 = (mydsp_faustpower2_f(fSlow26) * fSlow40);
		double fSlow51 = mydsp_faustpower2_f(fSlow45);
		double fSlow52 = (fConst12 * (((fSlow31 + std::atan2((((0.0 - ((fSlow26 * fSlow43) / fSlow45)) * fSlow46) + (((fSlow26 * fSlow40) * fSlow49) / fSlow45)), ((fSlow46 * fSlow49) + ((fSlow50 * fSlow43) / fSlow51)))) + 6.2831853071795862) / fSlow24));
		double fSlow53 = std::floor(fSlow52);
		double fSlow54 = (fSlow52 - fSlow53);
		double fSlow55 = double(fButton0);
		double fSlow56 = (0.0 - (fSlow55 + -1.0));
		double fSlow57 = (0.0010000000000000009 * ((0.99960000000000004 * fSlow55) + (0.90000000000000002 * (fSlow56 * double(getValueReleaseLoopGain(double(iSlow14)))))));
		double fSlow58 = double(getValueDCBa1(double(iSlow14)));
		double fSlow59 = (1.0 - fSlow58);
		double fSlow60 = (0.5 * fSlow59);
		double fSlow61 = (0.25 * double(fHslider5));
		double fSlow62 = double(getValueLoudPole(double(iSlow14)));
		double fSlow63 = ((fSlow61 + (0.97999999999999998 - fSlow62)) * double(getValueLoudGain(double(iSlow14))));
		double fSlow64 = (1.3969838625737391e-09 * (fSlow63 * double((iSlow14 < 88))));
		int iSlow65 = (fSlow55 > 0.0);
		double fSlow66 = std::exp((0.0 - (fConst14 / (double(fEntry1) * double(getValueDryTapAmpT60(double(iSlow14)))))));
		int iSlow67 = (fSlow55 < 1.0);
		double fSlow68 = (fConst15 * fSlow56);
		double fSlow69 = double(fHslider6);
		double fSlow70 = std::exp((0.0 - (fConst16 / fSlow69)));
		double fSlow71 = (fConst17 * fSlow69);
		double fSlow72 = (0.20000000000000001 * double(getValueSustainPedalLevel(double(iSlow14))));
		double fSlow73 = (fSlow62 + (0.02 - fSlow61));
		double fSlow74 = (0.0 - fSlow60);
		double fSlow75 = (3.7000000000000002 * fSlow27);
		double fSlow76 = ((fSlow36 + fSlow42) - fSlow39);
		double fSlow77 = (((fSlow48 + (fSlow30 * fSlow76)) / fSlow45) + 1.0);
		int iSlow78 = int((fConst12 * (((fSlow31 + std::atan2((0.0 - ((fSlow26 * ((fSlow76 * fSlow46) - (fSlow40 * fSlow77))) / fSlow45)), ((fSlow46 * fSlow77) + ((fSlow50 * fSlow76) / fSlow51)))) + 6.2831853071795862) / fSlow24)));
		int iSlow79 = std::min<int>(4097, std::max<int>(0, (iSlow78 + 1)));
		double fSlow80 = (1.0 / fSlow45);
		double fSlow81 = (fSlow13 - fSlow23);
		double fSlow82 = (fConst13 * fSlow81);
		double fSlow83 = std::sin(fSlow82);
		double fSlow84 = std::cos(fSlow82);
		double fSlow85 = (3.0 * std::atan2((fSlow22 * fSlow83), (fSlow28 + (fSlow29 * fSlow84))));
		double fSlow86 = (((fSlow84 * fSlow40) / fSlow45) + 1.0);
		double fSlow87 = ((((fSlow84 * fSlow43) + fSlow48) / fSlow45) + 1.0);
		double fSlow88 = (mydsp_faustpower2_f(fSlow83) * fSlow40);
		double fSlow89 = (fConst12 * (((fSlow85 + std::atan2((((0.0 - ((fSlow83 * fSlow43) / fSlow45)) * fSlow86) + (((fSlow83 * fSlow40) * fSlow87) / fSlow45)), ((fSlow86 * fSlow87) + ((fSlow88 * fSlow43) / fSlow51)))) + 6.2831853071795862) / fSlow81));
		double fSlow90 = std::floor(fSlow89);
		double fSlow91 = (fSlow90 + (1.0 - fSlow89));
		double fSlow92 = (((fSlow48 + (fSlow84 * fSlow76)) / fSlow45) + 1.0);
		int iSlow93 = int((fConst12 * (((fSlow85 + std::atan2((0.0 - ((fSlow83 * ((fSlow86 * fSlow76) - (fSlow40 * fSlow92))) / fSlow45)), ((fSlow86 * fSlow92) + ((fSlow88 * fSlow76) / fSlow51)))) + 6.2831853071795862) / fSlow81)));
		int iSlow94 = std::min<int>(4097, std::max<int>(0, iSlow93));
		double fSlow95 = (fSlow89 - fSlow90);
		int iSlow96 = std::min<int>(4097, std::max<int>(0, (iSlow93 + 1)));
		double fSlow97 = (fSlow53 + (1.0 - fSlow52));
		int iSlow98 = std::min<int>(4097, std::max<int>(0, iSlow78));
		double fSlow99 = ((0.0 - (fConst19 * (fSlow13 * fSlow15))) * std::cos((fConst13 * (fSlow13 / double(getValueStrikePosition(double(iSlow14)))))));
		double fSlow100 = std::cos((fConst13 * fSlow13));
		double fSlow101 = std::pow(10.0, (0.050000000000000003 * double(getValueSecondStageAmpRatio(double(iSlow14)))));
		double fSlow102 = std::pow(10.0, (fConst20 * double(getValuer1_1db(double(iSlow14)))));
		double fSlow103 = std::pow(10.0, (fConst20 * double(getValuer1_2db(double(iSlow14)))));
		double fSlow104 = (1.0 - fSlow101);
		double fSlow105 = (0.0 - (2.0 * ((fSlow101 * fSlow102) + (fSlow103 * fSlow104))));
		double fSlow106 = double(getValueBq4_gEarBalled(double(iSlow14)));
		double fSlow107 = (2.0 * fSlow106);
		double fSlow108 = double((iSlow14 >= 88));
		double fSlow109 = (2.3283064376228985e-10 * fSlow108);
		double fSlow110 = (1.1641532188114492e-10 * fSlow108);
		double fSlow111 = std::pow(10.0, (fConst20 * double(getValuer3db(double(iSlow14)))));
		double fSlow112 = (std::cos((fConst13 * (fSlow13 * double(getValueThirdPartialFactor(double(iSlow14)))))) * (0.0 - (2.0 * fSlow111)));
		double fSlow113 = mydsp_faustpower2_f(fSlow111);
		double fSlow114 = std::pow(10.0, (fConst20 * double(getValuer2db(double(iSlow14)))));
		double fSlow115 = (std::cos((fConst13 * (fSlow13 * double(getValueSecondPartialFactor(double(iSlow14)))))) * (0.0 - (2.0 * fSlow114)));
		double fSlow116 = mydsp_faustpower2_f(fSlow114);
		double fSlow117 = (fSlow100 * (0.0 - (2.0 * fSlow102)));
		double fSlow118 = mydsp_faustpower2_f(fSlow102);
		double fSlow119 = (0.0 - (2.0 * fSlow103));
		double fSlow120 = mydsp_faustpower2_f(fSlow103);
		double fSlow121 = ((fSlow101 * fSlow118) + (fSlow120 * fSlow104));
		int iSlow122 = int(std::min<double>(4096.0, std::max<double>(0.0, (fConst21 * (double(fHslider7) / fSlow13)))));
		double fSlow123 = std::exp((fConst26 / fSlow0));
		double fSlow124 = mydsp_faustpower2_f(fSlow123);
		double fSlow125 = (1.0 - (fConst1 * fSlow124));
		double fSlow126 = (1.0 - fSlow124);
		double fSlow127 = (fSlow125 / fSlow126);
		double fSlow128 = std::sqrt(std::max<double>(0.0, ((mydsp_faustpower2_f(fSlow125) / mydsp_faustpower2_f(fSlow126)) + -1.0)));
		double fSlow129 = (fSlow127 - fSlow128);
		double fSlow130 = (fSlow123 * (fSlow128 + (1.0 - fSlow127)));
		double fSlow131 = ((std::exp((fConst27 / fSlow0)) / fSlow123) + -1.0);
		double fSlow132 = std::exp((fConst33 / fSlow0));
		double fSlow133 = mydsp_faustpower2_f(fSlow132);
		double fSlow134 = (1.0 - (fConst1 * fSlow133));
		double fSlow135 = (1.0 - fSlow133);
		double fSlow136 = (fSlow134 / fSlow135);
		double fSlow137 = std::sqrt(std::max<double>(0.0, ((mydsp_faustpower2_f(fSlow134) / mydsp_faustpower2_f(fSlow135)) + -1.0)));
		double fSlow138 = (fSlow136 - fSlow137);
		double fSlow139 = (fSlow132 * (fSlow137 + (1.0 - fSlow136)));
		double fSlow140 = ((std::exp((fConst34 / fSlow0)) / fSlow132) + -1.0);
		double fSlow141 = std::exp((fConst40 / fSlow0));
		double fSlow142 = mydsp_faustpower2_f(fSlow141);
		double fSlow143 = (1.0 - (fConst1 * fSlow142));
		double fSlow144 = (1.0 - fSlow142);
		double fSlow145 = (fSlow143 / fSlow144);
		double fSlow146 = std::sqrt(std::max<double>(0.0, ((mydsp_faustpower2_f(fSlow143) / mydsp_faustpower2_f(fSlow144)) + -1.0)));
		double fSlow147 = (fSlow145 - fSlow146);
		double fSlow148 = (fSlow141 * (fSlow146 + (1.0 - fSlow145)));
		double fSlow149 = ((std::exp((fConst41 / fSlow0)) / fSlow141) + -1.0);
		double fSlow150 = std::exp((fConst47 / fSlow0));
		double fSlow151 = mydsp_faustpower2_f(fSlow150);
		double fSlow152 = (1.0 - (fConst1 * fSlow151));
		double fSlow153 = (1.0 - fSlow151);
		double fSlow154 = (fSlow152 / fSlow153);
		double fSlow155 = std::sqrt(std::max<double>(0.0, ((mydsp_faustpower2_f(fSlow152) / mydsp_faustpower2_f(fSlow153)) + -1.0)));
		double fSlow156 = (fSlow154 - fSlow155);
		double fSlow157 = (fSlow150 * (fSlow155 + (1.0 - fSlow154)));
		double fSlow158 = ((std::exp((fConst48 / fSlow0)) / fSlow150) + -1.0);
		double fSlow159 = (12.0 * (1.0 - fSlow10));
		double fSlow160 = std::exp((fConst54 / fSlow0));
		double fSlow161 = mydsp_faustpower2_f(fSlow160);
		double fSlow162 = (1.0 - (fConst1 * fSlow161));
		double fSlow163 = (1.0 - fSlow161);
		double fSlow164 = (fSlow162 / fSlow163);
		double fSlow165 = std::sqrt(std::max<double>(0.0, ((mydsp_faustpower2_f(fSlow162) / mydsp_faustpower2_f(fSlow163)) + -1.0)));
		double fSlow166 = (fSlow164 - fSlow165);
		double fSlow167 = (fSlow160 * (fSlow165 + (1.0 - fSlow164)));
		double fSlow168 = ((std::exp((fConst55 / fSlow0)) / fSlow160) + -1.0);
		double fSlow169 = std::exp((fConst61 / fSlow0));
		double fSlow170 = mydsp_faustpower2_f(fSlow169);
		double fSlow171 = (1.0 - (fConst1 * fSlow170));
		double fSlow172 = (1.0 - fSlow170);
		double fSlow173 = (fSlow171 / fSlow172);
		double fSlow174 = std::sqrt(std::max<double>(0.0, ((mydsp_faustpower2_f(fSlow171) / mydsp_faustpower2_f(fSlow172)) + -1.0)));
		double fSlow175 = (fSlow173 - fSlow174);
		double fSlow176 = (fSlow169 * (fSlow174 + (1.0 - fSlow173)));
		double fSlow177 = ((std::exp((fConst62 / fSlow0)) / fSlow169) + -1.0);
		double fSlow178 = std::exp((fConst68 / fSlow0));
		double fSlow179 = mydsp_faustpower2_f(fSlow178);
		double fSlow180 = (1.0 - (fConst1 * fSlow179));
		double fSlow181 = (1.0 - fSlow179);
		double fSlow182 = (fSlow180 / fSlow181);
		double fSlow183 = std::sqrt(std::max<double>(0.0, ((mydsp_faustpower2_f(fSlow180) / mydsp_faustpower2_f(fSlow181)) + -1.0)));
		double fSlow184 = (fSlow182 - fSlow183);
		double fSlow185 = (fSlow178 * (fSlow183 + (1.0 - fSlow182)));
		double fSlow186 = ((std::exp((fConst69 / fSlow0)) / fSlow178) + -1.0);
		for (int i = 0; (i < count); i = (i + 1)) {
			fRec11[0] = (0.0 - (fConst7 * ((fConst8 * fRec11[1]) - (fRec7[1] + fRec7[2]))));
			fRec10[0] = ((fSlow7 * fRec10[1]) + (fSlow8 * (fRec7[1] + (fSlow9 * fRec11[0]))));
			fVec0[(IOTA & 32767)] = ((0.35355339059327373 * fRec10[0]) + 9.9999999999999995e-21);
			fRec12[0] = (fSlow12 + (0.999 * fRec12[1]));
			fRec20[0] = (fSlow57 + (0.999 * fRec20[1]));
			iRec26[0] = ((1103515245 * iRec26[1]) + 12345);
			fRec28[0] = ((fSlow55 * fRec28[1]) + 1.0);
			double fTemp0 = (fRec28[0] + -1.0);
			double fTemp1 = double(((fTemp0 < 2.0) & iSlow65));
			double fTemp2 = ((0.030197383422318501 * fTemp1) + (fSlow66 * double(((fTemp0 >= 2.0) | iSlow67))));
			fRec27[0] = ((fRec27[1] * fTemp2) + (0.14999999999999999 * (fTemp1 * (1.0 - fTemp2))));
			int iTemp3 = (fTemp0 < fSlow71);
			double fTemp4 = (fSlow55 * ((fSlow70 * double(iTemp3)) + (fConst18 * double((fTemp0 >= fSlow71)))));
			fRec29[0] = ((fRec29[1] * (fSlow68 + fTemp4)) + (fSlow72 * (((1.0 - fTemp4) - fSlow68) * double((iTemp3 & iSlow65)))));
			double fTemp5 = (double(iRec26[0]) * (fRec27[0] + fRec29[0]));
			fRec25[0] = ((fSlow64 * fTemp5) + (fSlow73 * fRec25[1]));
			fRec24[0] = ((fSlow63 * fRec25[0]) + (fSlow73 * fRec24[1]));
			fRec23[0] = ((fSlow63 * fRec24[0]) + (fSlow73 * fRec23[1]));
			fRec22[0] = ((fSlow63 * fRec23[0]) + (fSlow73 * fRec22[1]));
			fRec21[0] = (((fSlow60 * fRec22[0]) + (fSlow74 * fRec22[1])) - (fSlow58 * fRec21[1]));
			double fTemp6 = (fRec20[0] * (fRec21[0] + fRec14[1]));
			fVec1[0] = fTemp6;
			fRec19[0] = (fVec1[1] + (fSlow75 * (fTemp6 - fRec19[1])));
			fRec18[0] = (fRec19[1] + (fSlow75 * (fRec19[0] - fRec18[1])));
			fRec17[(IOTA & 8191)] = (fRec18[1] + (fSlow75 * (fRec18[0] - fRec17[((IOTA - 1) & 8191)])));
			double fTemp7 = (fSlow54 * fRec17[((IOTA - iSlow79) & 8191)]);
			double fTemp8 = (fRec21[0] + (fRec20[0] * fRec15[1]));
			fVec2[0] = fTemp8;
			fRec33[0] = (fVec2[1] + (fSlow75 * (fTemp8 - fRec33[1])));
			fRec32[0] = (fRec33[1] + (fSlow75 * (fRec33[0] - fRec32[1])));
			fRec31[(IOTA & 8191)] = (fRec32[1] + (fSlow75 * (fRec32[0] - fRec31[((IOTA - 1) & 8191)])));
			double fTemp9 = (fSlow91 * fRec31[((IOTA - iSlow94) & 8191)]);
			double fTemp10 = (fSlow95 * fRec31[((IOTA - iSlow96) & 8191)]);
			double fTemp11 = (fSlow97 * fRec17[((IOTA - iSlow98) & 8191)]);
			double fTemp12 = (fTemp7 + ((fTemp9 + fTemp10) + fTemp11));
			fVec3[0] = fTemp12;
			fRec30[0] = (fSlow80 * ((2.0 * ((fSlow47 * fTemp12) + (fSlow41 * fVec3[1]))) - (fSlow40 * fRec30[1])));
			fRec14[0] = (fTemp7 + (fRec30[0] + fTemp11));
			fRec15[0] = (fTemp10 + (fRec30[0] + fTemp9));
			double fRec16 = fTemp12;
			fRec13[0] = ((fSlow18 * fRec16) - ((fSlow99 * fRec13[1]) + (fSlow16 * fRec13[2])));
			fVec4[0] = (fSlow109 * fTemp5);
			double fTemp13 = (0.0 - ((0.5 * fVec4[1]) + (fSlow110 * fTemp5)));
			fVec5[0] = fTemp13;
			fRec43[0] = (((fSlow60 * fTemp13) + (fSlow74 * fVec5[1])) - (fSlow58 * fRec43[1]));
			fRec42[0] = ((fSlow63 * fRec43[0]) + (fSlow73 * fRec42[1]));
			fRec41[0] = ((fSlow63 * fRec42[0]) + (fSlow73 * fRec41[1]));
			fRec40[0] = ((fSlow63 * fRec41[0]) + (fSlow73 * fRec40[1]));
			fRec39[0] = ((fSlow63 * fRec40[0]) + (fSlow73 * fRec39[1]));
			fRec38[0] = ((fSlow106 * (fRec39[0] - fRec39[1])) - ((fSlow112 * fRec38[1]) + (fSlow113 * fRec38[2])));
			fRec37[0] = ((fSlow107 * fRec38[0]) - ((fSlow115 * fRec37[1]) + (fSlow116 * fRec37[2])));
			fRec36[0] = (fRec37[0] - ((fSlow117 * fRec36[1]) + (fSlow118 * fRec36[2])));
			fRec35[0] = (((fSlow100 * ((fSlow105 * fRec36[1]) - (fSlow119 * fRec35[1]))) + (fRec36[0] + (fSlow121 * fRec36[2]))) - (fSlow120 * fRec35[2]));
			fRec34[0] = ((fSlow59 * fRec35[0]) - (fSlow58 * fRec34[1]));
			double fTemp14 = ((fSlow17 * (fRec13[0] - fRec13[2])) + (fRec16 + fRec34[0]));
			fVec6[(IOTA & 8191)] = fTemp14;
			double fTemp15 = fVec6[((IOTA - iSlow122) & 8191)];
			fVec7[(IOTA & 4095)] = (fSlow11 * (fRec12[0] * fTemp15));
			double fTemp16 = (0.29999999999999999 * fVec7[((IOTA - iConst22) & 4095)]);
			double fTemp17 = (((0.59999999999999998 * fRec8[1]) + fVec0[((IOTA - iConst10) & 32767)]) - fTemp16);
			fVec8[(IOTA & 2047)] = fTemp17;
			fRec8[0] = fVec8[((IOTA - iConst23) & 2047)];
			double fRec9 = (0.0 - (0.59999999999999998 * fTemp17));
			fRec47[0] = (0.0 - (fConst7 * ((fConst8 * fRec47[1]) - (fRec3[1] + fRec3[2]))));
			fRec46[0] = ((fSlow129 * fRec46[1]) + (fSlow130 * (fRec3[1] + (fSlow131 * fRec47[0]))));
			fVec9[(IOTA & 32767)] = ((0.35355339059327373 * fRec46[0]) + 9.9999999999999995e-21);
			double fTemp18 = (((0.59999999999999998 * fRec44[1]) + fVec9[((IOTA - iConst29) & 32767)]) - fTemp16);
			fVec10[(IOTA & 4095)] = fTemp18;
			fRec44[0] = fVec10[((IOTA - iConst30) & 4095)];
			double fRec45 = (0.0 - (0.59999999999999998 * fTemp18));
			fRec51[0] = (0.0 - (fConst7 * ((fConst8 * fRec51[1]) - (fRec5[1] + fRec5[2]))));
			fRec50[0] = ((fSlow138 * fRec50[1]) + (fSlow139 * (fRec5[1] + (fSlow140 * fRec51[0]))));
			fVec11[(IOTA & 16383)] = ((0.35355339059327373 * fRec50[0]) + 9.9999999999999995e-21);
			double fTemp19 = (fVec11[((IOTA - iConst36) & 16383)] + (fTemp16 + (0.59999999999999998 * fRec48[1])));
			fVec12[(IOTA & 4095)] = fTemp19;
			fRec48[0] = fVec12[((IOTA - iConst37) & 4095)];
			double fRec49 = (0.0 - (0.59999999999999998 * fTemp19));
			fRec55[0] = (0.0 - (fConst7 * ((fConst8 * fRec55[1]) - (fRec1[1] + fRec1[2]))));
			fRec54[0] = ((fSlow147 * fRec54[1]) + (fSlow148 * (fRec1[1] + (fSlow149 * fRec55[0]))));
			fVec13[(IOTA & 32767)] = ((0.35355339059327373 * fRec54[0]) + 9.9999999999999995e-21);
			double fTemp20 = (fTemp16 + ((0.59999999999999998 * fRec52[1]) + fVec13[((IOTA - iConst43) & 32767)]));
			fVec14[(IOTA & 4095)] = fTemp20;
			fRec52[0] = fVec14[((IOTA - iConst44) & 4095)];
			double fRec53 = (0.0 - (0.59999999999999998 * fTemp20));
			fRec59[0] = (0.0 - (fConst7 * ((fConst8 * fRec59[1]) - (fRec6[1] + fRec6[2]))));
			fRec58[0] = ((fSlow156 * fRec58[1]) + (fSlow157 * (fRec6[1] + (fSlow158 * fRec59[0]))));
			fVec15[(IOTA & 16383)] = ((0.35355339059327373 * fRec58[0]) + 9.9999999999999995e-21);
			fVec16[(IOTA & 4095)] = (fSlow159 * (fRec12[0] * fTemp14));
			double fTemp21 = (0.29999999999999999 * fVec16[((IOTA - iConst22) & 4095)]);
			double fTemp22 = (fVec15[((IOTA - iConst50) & 16383)] - (fTemp21 + (0.59999999999999998 * fRec56[1])));
			fVec17[(IOTA & 2047)] = fTemp22;
			fRec56[0] = fVec17[((IOTA - iConst51) & 2047)];
			double fRec57 = (0.59999999999999998 * fTemp22);
			fRec63[0] = (0.0 - (fConst7 * ((fConst8 * fRec63[1]) - (fRec2[1] + fRec2[2]))));
			fRec62[0] = ((fSlow166 * fRec62[1]) + (fSlow167 * (fRec2[1] + (fSlow168 * fRec63[0]))));
			fVec18[(IOTA & 16383)] = ((0.35355339059327373 * fRec62[0]) + 9.9999999999999995e-21);
			double fTemp23 = (fVec18[((IOTA - iConst57) & 16383)] - (fTemp21 + (0.59999999999999998 * fRec60[1])));
			fVec19[(IOTA & 4095)] = fTemp23;
			fRec60[0] = fVec19[((IOTA - iConst58) & 4095)];
			double fRec61 = (0.59999999999999998 * fTemp23);
			fRec67[0] = (0.0 - (fConst7 * ((fConst8 * fRec67[1]) - (fRec4[1] + fRec4[2]))));
			fRec66[0] = ((fSlow175 * fRec66[1]) + (fSlow176 * (fRec4[1] + (fSlow177 * fRec67[0]))));
			fVec20[(IOTA & 16383)] = ((0.35355339059327373 * fRec66[0]) + 9.9999999999999995e-21);
			double fTemp24 = ((fTemp21 + fVec20[((IOTA - iConst64) & 16383)]) - (0.59999999999999998 * fRec64[1]));
			fVec21[(IOTA & 4095)] = fTemp24;
			fRec64[0] = fVec21[((IOTA - iConst65) & 4095)];
			double fRec65 = (0.59999999999999998 * fTemp24);
			fRec71[0] = (0.0 - (fConst7 * ((fConst8 * fRec71[1]) - (fRec0[1] + fRec0[2]))));
			fRec70[0] = ((fSlow184 * fRec70[1]) + (fSlow185 * (fRec0[1] + (fSlow186 * fRec71[0]))));
			fVec22[(IOTA & 16383)] = ((0.35355339059327373 * fRec70[0]) + 9.9999999999999995e-21);
			double fTemp25 = ((fVec22[((IOTA - iConst71) & 16383)] + fTemp21) - (0.59999999999999998 * fRec68[1]));
			fVec23[(IOTA & 2047)] = fTemp25;
			fRec68[0] = fVec23[((IOTA - iConst72) & 2047)];
			double fRec69 = (0.59999999999999998 * fTemp25);
			double fTemp26 = (fRec69 + fRec65);
			double fTemp27 = (fRec57 + (fRec61 + fTemp26));
			fRec0[0] = (fRec8[1] + (fRec44[1] + (fRec48[1] + (fRec52[1] + (fRec56[1] + (fRec60[1] + (fRec64[1] + (fRec68[1] + (fRec9 + (fRec45 + (fRec49 + (fRec53 + fTemp27))))))))))));
			fRec1[0] = ((fRec56[1] + (fRec60[1] + (fRec64[1] + (fRec68[1] + fTemp27)))) - (fRec8[1] + (fRec44[1] + (fRec48[1] + (fRec52[1] + (fRec9 + (fRec45 + (fRec53 + fRec49))))))));
			double fTemp28 = (fRec61 + fRec57);
			fRec2[0] = ((fRec48[1] + (fRec52[1] + (fRec64[1] + (fRec68[1] + (fRec49 + (fRec53 + fTemp26)))))) - (fRec8[1] + (fRec44[1] + (fRec56[1] + (fRec60[1] + (fRec9 + (fRec45 + fTemp28)))))));
			fRec3[0] = ((fRec8[1] + (fRec44[1] + (fRec64[1] + (fRec68[1] + (fRec9 + (fRec45 + fTemp26)))))) - (fRec48[1] + (fRec52[1] + (fRec56[1] + (fRec60[1] + (fRec49 + (fRec53 + fTemp28)))))));
			double fTemp29 = (fRec69 + fRec61);
			double fTemp30 = (fRec65 + fRec57);
			fRec4[0] = ((fRec44[1] + (fRec52[1] + (fRec60[1] + (fRec68[1] + (fRec45 + (fRec53 + fTemp29)))))) - (fRec8[1] + (fRec48[1] + (fRec56[1] + (fRec64[1] + (fRec9 + (fRec49 + fTemp30)))))));
			fRec5[0] = ((fRec8[1] + (fRec48[1] + (fRec60[1] + (fRec68[1] + (fRec9 + (fRec49 + fTemp29)))))) - (fRec44[1] + (fRec52[1] + (fRec56[1] + (fRec64[1] + (fRec45 + (fRec53 + fTemp30)))))));
			double fTemp31 = (fRec69 + fRec57);
			double fTemp32 = (fRec65 + fRec61);
			fRec6[0] = ((fRec8[1] + (fRec52[1] + (fRec56[1] + (fRec68[1] + (fRec9 + (fRec53 + fTemp31)))))) - (fRec44[1] + (fRec48[1] + (fRec60[1] + (fRec64[1] + (fRec45 + (fRec49 + fTemp32)))))));
			fRec7[0] = ((fRec44[1] + (fRec48[1] + (fRec56[1] + (fRec68[1] + (fRec45 + (fRec49 + fTemp31)))))) - (fRec8[1] + (fRec52[1] + (fRec60[1] + (fRec64[1] + (fRec9 + (fRec53 + fTemp32)))))));
			double fTemp33 = (1.0 - fRec12[0]);
			output0[i] = FAUSTFLOAT(((0.37 * (fRec1[0] + fRec2[0])) + (fSlow159 * (fTemp33 * fTemp14))));
			output1[i] = FAUSTFLOAT(((0.37 * (fRec1[0] - fRec2[0])) + (fSlow11 * (fTemp33 * fTemp15))));
			fRec11[1] = fRec11[0];
			fRec10[1] = fRec10[0];
			IOTA = (IOTA + 1);
			fRec12[1] = fRec12[0];
			fRec20[1] = fRec20[0];
			iRec26[1] = iRec26[0];
			fRec28[1] = fRec28[0];
			fRec27[1] = fRec27[0];
			fRec29[1] = fRec29[0];
			fRec25[1] = fRec25[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			fRec22[1] = fRec22[0];
			fRec21[1] = fRec21[0];
			fVec1[1] = fVec1[0];
			fRec19[1] = fRec19[0];
			fRec18[1] = fRec18[0];
			fVec2[1] = fVec2[0];
			fRec33[1] = fRec33[0];
			fRec32[1] = fRec32[0];
			fVec3[1] = fVec3[0];
			fRec30[1] = fRec30[0];
			fRec14[1] = fRec14[0];
			fRec15[1] = fRec15[0];
			fRec13[2] = fRec13[1];
			fRec13[1] = fRec13[0];
			fVec4[1] = fVec4[0];
			fVec5[1] = fVec5[0];
			fRec43[1] = fRec43[0];
			fRec42[1] = fRec42[0];
			fRec41[1] = fRec41[0];
			fRec40[1] = fRec40[0];
			fRec39[1] = fRec39[0];
			fRec38[2] = fRec38[1];
			fRec38[1] = fRec38[0];
			fRec37[2] = fRec37[1];
			fRec37[1] = fRec37[0];
			fRec36[2] = fRec36[1];
			fRec36[1] = fRec36[0];
			fRec35[2] = fRec35[1];
			fRec35[1] = fRec35[0];
			fRec34[1] = fRec34[0];
			fRec8[1] = fRec8[0];
			fRec47[1] = fRec47[0];
			fRec46[1] = fRec46[0];
			fRec44[1] = fRec44[0];
			fRec51[1] = fRec51[0];
			fRec50[1] = fRec50[0];
			fRec48[1] = fRec48[0];
			fRec55[1] = fRec55[0];
			fRec54[1] = fRec54[0];
			fRec52[1] = fRec52[0];
			fRec59[1] = fRec59[0];
			fRec58[1] = fRec58[0];
			fRec56[1] = fRec56[0];
			fRec63[1] = fRec63[0];
			fRec62[1] = fRec62[0];
			fRec60[1] = fRec60[0];
			fRec67[1] = fRec67[0];
			fRec66[1] = fRec66[0];
			fRec64[1] = fRec64[0];
			fRec71[1] = fRec71[0];
			fRec70[1] = fRec70[0];
			fRec68[1] = fRec68[0];
			fRec0[2] = fRec0[1];
			fRec0[1] = fRec0[0];
			fRec1[2] = fRec1[1];
			fRec1[1] = fRec1[0];
			fRec2[2] = fRec2[1];
			fRec2[1] = fRec2[0];
			fRec3[2] = fRec3[1];
			fRec3[1] = fRec3[0];
			fRec4[2] = fRec4[1];
			fRec4[1] = fRec4[0];
			fRec5[2] = fRec5[1];
			fRec5[1] = fRec5[0];
			fRec6[2] = fRec6[1];
			fRec6[1] = fRec6[0];
			fRec7[2] = fRec7[1];
			fRec7[1] = fRec7[0];
		}
	}

};

#ifdef FAUST_UIMACROS
	#define FAUST_INPUTS 0
	#define FAUST_OUTPUTS 2
	#define FAUST_ACTIVES 11
	#define FAUST_PASSIVES 0
	FAUST_ADDNUMENTRY("Basic_Parameters/freq", fEntry0, 440.0, 20.0, 20000.0, 1.0);
	FAUST_ADDNUMENTRY("Basic_Parameters/gain", fEntry1, 1.0, 0.0, 1.0, 0.01);
	FAUST_ADDBUTTON("Basic_Parameters/gate", fButton0);
	FAUST_ADDHORIZONTALSLIDER("Physical_Parameters/Brightness_Factor", fHslider5, 0.0, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("Physical_Parameters/Detuning_Factor", fHslider4, 0.10000000000000001, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("Physical_Parameters/Hammer_Hardness", fHslider6, 0.10000000000000001, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("Physical_Parameters/Stiffness_Factor", fHslider3, 0.28000000000000003, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("Reverb/reverbGain", fHslider2, 0.13700000000000001, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("Reverb/roomSize", fHslider0, 0.71999999999999997, 0.01, 2.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("Spat/pan angle", fHslider1, 0.59999999999999998, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("Spat/spatial width", fHslider7, 0.5, 0.0, 1.0, 0.01);
#endif
/***************************END USER SECTION ***************************/

/*******************BEGIN ARCHITECTURE SECTION (part 2/2)***************/

struct dataspace {
    OPDS      h;                          /* basic attributes  */
#if (FAUST_OUTPUTS > 0)                   /* omit 0 size array */
    MYFLT*    aout[FAUST_OUTPUTS];        /* output buffers    */
#endif
#if (FAUST_INPUTS > 0)                    /* omit 0 size array */
    MYFLT*    ain[FAUST_INPUTS];          /* input buffers     */
#endif
#if (FAUST_ACTIVES > 0)                   /* omit 0 size array */
    MYFLT*    ktl[FAUST_ACTIVES];         /* controls          */
#endif
    dsp*      DSP;                        /* the Faust generated object */
    CSUI*     interface;                  /* do the mapping between CSound controls and DSP fields */
    AUXCH     dspmem;                     /* aux memory allocated once to store the DSP object */
    AUXCH     intmem;                     /* aux memory allocated once to store the interface object */
/* Dummies to satisfy the compiler for "zero sized" arrays. */
#if (FAUST_OUTPUTS == 0)
    MYFLT*    aout[1];
#endif
#if (FAUST_INPUTS == 0)
    MYFLT*    ain[1];
#endif
#if (FAUST_ACTIVES == 0)
    MYFLT*    ktl[1];
#endif
};

/**
 * Creates a "aaakkkk" CSound description string. Note that
 * these string will never be released. Potential memory leak
 */
static char* makeDescription(int numa, int numk = 0)
{
    char* str = (char*)malloc(numa+numk+1); // NEED TO BE CHANGED ?
    if (str) {
        for (int i = 0; i < numa; i++) str[i] = 'a';
        for (int i = 0; i < numk; i++) str[numa+i] = 'J';
        str[numa+numk] = 0;
    }
    return str;
}

/**
 * CSOUND callback that allocates and initializes
 * the FAUST generated DSP object and it's CSound interface
 */
static int init(CSOUND* csound, dataspace* p)
{
    if (p->dspmem.auxp == NULL)
        csound->AuxAlloc(csound, sizeof(mydsp), &p->dspmem);

    if (p->intmem.auxp == NULL)
        csound->AuxAlloc(csound, sizeof(CSUI), &p->intmem);

    p->DSP = new (p->dspmem.auxp) mydsp;
    p->interface = new (p->intmem.auxp) CSUI;

    if ((p->DSP == 0) | (p->interface == 0)) return NOTOK;

    p->DSP->init((int)csound->GetSr(csound));
    p->DSP->buildUserInterface(p->interface);

    return OK;
}

/**
 * CSound callback that process the samples by updating
 * the controls values and calling the compute() method
 * of the DSP object. (Assume MYFLT = FAUSTFLOAT)
 */
static int process32bits(CSOUND* csound, dataspace* p)
{
    AVOIDDENORMALS;

    // update all the control values
    p->interface->copyfrom(p->ktl);

    p->DSP->compute(csound->GetKsmps(csound), p->ain, p->aout);
    return OK;
}

extern "C" {
    static OENTRY localops[] = {
        {(char*)sym(OPCODE_NAME), sizeof(dataspace), 0, 3, makeDescription(FAUST_OUTPUTS), makeDescription(FAUST_INPUTS, FAUST_ACTIVES),
            (SUBR)init, (SUBR)process32bits, NULL }
    };
    LINKAGE
}

/********************END ARCHITECTURE SECTION (part 2/2)****************/

#endif
