<CsoundSyntheizer>
<CsLicense>
This .csd file tests the new JIT compiler opcode for Csound.
</CsLicense>
<CsOptions>
-m195 -otest.wav
</CsOptions>
<CsInstruments>

prints "I'm about to try compiling a simple test C++ module.\n"

gS_guitar_source_code = {{

/* ------------------------------------------------------------
copyright: "(c)Romain Michon & John Granzow, CCRMA (Stanford University), GRAME, University of Michigan"
license: "MIT"
name: "ModularInterpInstrMidi"
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
#include <csound/csdl.h>                       /* CSOUND plugin API header */

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

#include "faust/misc.h"
#include "faust/dsp/dsp.h"
#include "faust/gui/meta.h"
#include "faust/gui/UI.h"

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

#include <algorithm>
#include <cmath>
#include <math.h>

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
	
	FAUSTFLOAT fHslider0;
	int iRec14[2];
	int fSampleRate;
	double fConst0;
	double fConst1;
	double fConst2;
	FAUSTFLOAT fHslider1;
	double fRec30[2];
	FAUSTFLOAT fHslider2;
	FAUSTFLOAT fHslider3;
	FAUSTFLOAT fButton0;
	double fVec0[2];
	FAUSTFLOAT fHslider4;
	double fRec31[2];
	double fRec27[2];
	double fRec32[2];
	double fRec34[4];
	int IOTA;
	double fRec35[2048];
	double fVec1[2];
	FAUSTFLOAT fHslider5;
	int iRec37[2];
	double fConst3;
	double fRec36[3];
	int iRec38[2];
	double fConst4;
	double fVec2[2];
	double fRec33[2048];
	double fRec24[2];
	double fRec21[2048];
	double fRec23[2];
	double fRec20[4];
	FAUSTFLOAT fButton1;
	double fVec3[2];
	double fVec4[2];
	double fConst5;
	FAUSTFLOAT fHslider6;
	FAUSTFLOAT fHslider7;
	double fConst6;
	double fRec18[3];
	double fRec39[3];
	double fRec40[3];
	double fRec41[3];
	double fRec42[3];
	double fRec43[3];
	double fRec44[3];
	double fRec45[3];
	double fRec46[3];
	double fRec47[3];
	double fRec48[3];
	double fRec49[3];
	double fRec50[3];
	double fRec51[3];
	double fRec52[3];
	double fRec53[3];
	double fRec54[3];
	double fRec55[3];
	double fRec56[3];
	double fRec57[3];
	double fRec58[3];
	double fRec59[3];
	double fRec60[3];
	double fRec61[3];
	double fRec62[3];
	double fRec63[3];
	double fRec64[3];
	double fRec65[3];
	double fRec66[3];
	double fRec67[3];
	double fRec68[3];
	double fRec69[3];
	double fRec70[3];
	double fRec71[3];
	double fRec72[3];
	double fRec73[3];
	double fRec74[3];
	double fRec75[3];
	double fRec76[3];
	double fRec77[3];
	int iRec10[2];
	int iRec6[2];
	double fRec2[2048];
	double fRec0[2];
	
 public:
	
	void metadata(Meta* m) { 
		m->declare("basics.lib/name", "Faust Basic Element Library");
		m->declare("basics.lib/version", "0.1");
		m->declare("copyright", "(c)Romain Michon & John Granzow, CCRMA (Stanford University), GRAME, University of Michigan");
		m->declare("delays.lib/name", "Faust Delay Library");
		m->declare("delays.lib/version", "0.1");
		m->declare("description", "String instrument with a modular body");
		m->declare("envelopes.lib/ar:author", "Yann Orlarey, Stéphane Letz");
		m->declare("envelopes.lib/author", "GRAME");
		m->declare("envelopes.lib/copyright", "GRAME");
		m->declare("envelopes.lib/license", "LGPL with exception");
		m->declare("envelopes.lib/name", "Faust Envelope Library");
		m->declare("envelopes.lib/version", "0.0");
		m->declare("filename", "modularInterpInstrMIDI.dsp");
		m->declare("filters.lib/fir:author", "Julius O. Smith III");
		m->declare("filters.lib/fir:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/fir:license", "MIT-style STK-4.3 license");
		m->declare("filters.lib/iir:author", "Julius O. Smith III");
		m->declare("filters.lib/iir:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/iir:license", "MIT-style STK-4.3 license");
		m->declare("filters.lib/lowpass0_highpass1", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/lowpass0_highpass1:author", "Julius O. Smith III");
		m->declare("filters.lib/lowpass:author", "Julius O. Smith III");
		m->declare("filters.lib/lowpass:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/lowpass:license", "MIT-style STK-4.3 license");
		m->declare("filters.lib/name", "Faust Filters Library");
		m->declare("filters.lib/tf2:author", "Julius O. Smith III");
		m->declare("filters.lib/tf2:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/tf2:license", "MIT-style STK-4.3 license");
		m->declare("filters.lib/tf2s:author", "Julius O. Smith III");
		m->declare("filters.lib/tf2s:copyright", "Copyright (C) 2003-2019 by Julius O. Smith III <jos@ccrma.stanford.edu>");
		m->declare("filters.lib/tf2s:license", "MIT-style STK-4.3 license");
		m->declare("license", "MIT");
		m->declare("maths.lib/author", "GRAME");
		m->declare("maths.lib/copyright", "GRAME");
		m->declare("maths.lib/license", "LGPL with exception");
		m->declare("maths.lib/name", "Faust Math Library");
		m->declare("maths.lib/version", "2.1");
		m->declare("name", "ModularInterpInstrMidi");
		m->declare("noises.lib/name", "Faust Noise Generator Library");
		m->declare("noises.lib/version", "0.0");
		m->declare("routes.lib/name", "Faust Signal Routing Library");
		m->declare("routes.lib/version", "0.1");
		m->declare("signals.lib/name", "Faust Signal Routing Library");
		m->declare("signals.lib/version", "0.0");
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
		fConst1 = (0.0088235294117647058 * fConst0);
		fConst2 = (0.0014705882352941176 * fConst0);
		fConst3 = (15.707963267948966 / fConst0);
		fConst4 = (0.002 * fConst0);
		fConst5 = (1.0 / fConst0);
		fConst6 = (6.2831853071795862 / fConst0);
	}
	
	virtual void instanceResetUserInterface() {
		fHslider0 = FAUSTFLOAT(0.5);
		fHslider1 = FAUSTFLOAT(0.80000000000000004);
		fHslider2 = FAUSTFLOAT(440.0);
		fHslider3 = FAUSTFLOAT(0.0);
		fButton0 = FAUSTFLOAT(0.0);
		fHslider4 = FAUSTFLOAT(0.0);
		fHslider5 = FAUSTFLOAT(0.80000000000000004);
		fButton1 = FAUSTFLOAT(0.0);
		fHslider6 = FAUSTFLOAT(0.0);
		fHslider7 = FAUSTFLOAT(0.0);
	}
	
	virtual void instanceClear() {
		for (int l0 = 0; (l0 < 2); l0 = (l0 + 1)) {
			iRec14[l0] = 0;
		}
		for (int l1 = 0; (l1 < 2); l1 = (l1 + 1)) {
			fRec30[l1] = 0.0;
		}
		for (int l2 = 0; (l2 < 2); l2 = (l2 + 1)) {
			fVec0[l2] = 0.0;
		}
		for (int l3 = 0; (l3 < 2); l3 = (l3 + 1)) {
			fRec31[l3] = 0.0;
		}
		for (int l4 = 0; (l4 < 2); l4 = (l4 + 1)) {
			fRec27[l4] = 0.0;
		}
		for (int l5 = 0; (l5 < 2); l5 = (l5 + 1)) {
			fRec32[l5] = 0.0;
		}
		for (int l6 = 0; (l6 < 4); l6 = (l6 + 1)) {
			fRec34[l6] = 0.0;
		}
		IOTA = 0;
		for (int l7 = 0; (l7 < 2048); l7 = (l7 + 1)) {
			fRec35[l7] = 0.0;
		}
		for (int l8 = 0; (l8 < 2); l8 = (l8 + 1)) {
			fVec1[l8] = 0.0;
		}
		for (int l9 = 0; (l9 < 2); l9 = (l9 + 1)) {
			iRec37[l9] = 0;
		}
		for (int l10 = 0; (l10 < 3); l10 = (l10 + 1)) {
			fRec36[l10] = 0.0;
		}
		for (int l11 = 0; (l11 < 2); l11 = (l11 + 1)) {
			iRec38[l11] = 0;
		}
		for (int l12 = 0; (l12 < 2); l12 = (l12 + 1)) {
			fVec2[l12] = 0.0;
		}
		for (int l13 = 0; (l13 < 2048); l13 = (l13 + 1)) {
			fRec33[l13] = 0.0;
		}
		for (int l14 = 0; (l14 < 2); l14 = (l14 + 1)) {
			fRec24[l14] = 0.0;
		}
		for (int l15 = 0; (l15 < 2048); l15 = (l15 + 1)) {
			fRec21[l15] = 0.0;
		}
		for (int l16 = 0; (l16 < 2); l16 = (l16 + 1)) {
			fRec23[l16] = 0.0;
		}
		for (int l17 = 0; (l17 < 4); l17 = (l17 + 1)) {
			fRec20[l17] = 0.0;
		}
		for (int l18 = 0; (l18 < 2); l18 = (l18 + 1)) {
			fVec3[l18] = 0.0;
		}
		for (int l19 = 0; (l19 < 2); l19 = (l19 + 1)) {
			fVec4[l19] = 0.0;
		}
		for (int l20 = 0; (l20 < 3); l20 = (l20 + 1)) {
			fRec18[l20] = 0.0;
		}
		for (int l21 = 0; (l21 < 3); l21 = (l21 + 1)) {
			fRec39[l21] = 0.0;
		}
		for (int l22 = 0; (l22 < 3); l22 = (l22 + 1)) {
			fRec40[l22] = 0.0;
		}
		for (int l23 = 0; (l23 < 3); l23 = (l23 + 1)) {
			fRec41[l23] = 0.0;
		}
		for (int l24 = 0; (l24 < 3); l24 = (l24 + 1)) {
			fRec42[l24] = 0.0;
		}
		for (int l25 = 0; (l25 < 3); l25 = (l25 + 1)) {
			fRec43[l25] = 0.0;
		}
		for (int l26 = 0; (l26 < 3); l26 = (l26 + 1)) {
			fRec44[l26] = 0.0;
		}
		for (int l27 = 0; (l27 < 3); l27 = (l27 + 1)) {
			fRec45[l27] = 0.0;
		}
		for (int l28 = 0; (l28 < 3); l28 = (l28 + 1)) {
			fRec46[l28] = 0.0;
		}
		for (int l29 = 0; (l29 < 3); l29 = (l29 + 1)) {
			fRec47[l29] = 0.0;
		}
		for (int l30 = 0; (l30 < 3); l30 = (l30 + 1)) {
			fRec48[l30] = 0.0;
		}
		for (int l31 = 0; (l31 < 3); l31 = (l31 + 1)) {
			fRec49[l31] = 0.0;
		}
		for (int l32 = 0; (l32 < 3); l32 = (l32 + 1)) {
			fRec50[l32] = 0.0;
		}
		for (int l33 = 0; (l33 < 3); l33 = (l33 + 1)) {
			fRec51[l33] = 0.0;
		}
		for (int l34 = 0; (l34 < 3); l34 = (l34 + 1)) {
			fRec52[l34] = 0.0;
		}
		for (int l35 = 0; (l35 < 3); l35 = (l35 + 1)) {
			fRec53[l35] = 0.0;
		}
		for (int l36 = 0; (l36 < 3); l36 = (l36 + 1)) {
			fRec54[l36] = 0.0;
		}
		for (int l37 = 0; (l37 < 3); l37 = (l37 + 1)) {
			fRec55[l37] = 0.0;
		}
		for (int l38 = 0; (l38 < 3); l38 = (l38 + 1)) {
			fRec56[l38] = 0.0;
		}
		for (int l39 = 0; (l39 < 3); l39 = (l39 + 1)) {
			fRec57[l39] = 0.0;
		}
		for (int l40 = 0; (l40 < 3); l40 = (l40 + 1)) {
			fRec58[l40] = 0.0;
		}
		for (int l41 = 0; (l41 < 3); l41 = (l41 + 1)) {
			fRec59[l41] = 0.0;
		}
		for (int l42 = 0; (l42 < 3); l42 = (l42 + 1)) {
			fRec60[l42] = 0.0;
		}
		for (int l43 = 0; (l43 < 3); l43 = (l43 + 1)) {
			fRec61[l43] = 0.0;
		}
		for (int l44 = 0; (l44 < 3); l44 = (l44 + 1)) {
			fRec62[l44] = 0.0;
		}
		for (int l45 = 0; (l45 < 3); l45 = (l45 + 1)) {
			fRec63[l45] = 0.0;
		}
		for (int l46 = 0; (l46 < 3); l46 = (l46 + 1)) {
			fRec64[l46] = 0.0;
		}
		for (int l47 = 0; (l47 < 3); l47 = (l47 + 1)) {
			fRec65[l47] = 0.0;
		}
		for (int l48 = 0; (l48 < 3); l48 = (l48 + 1)) {
			fRec66[l48] = 0.0;
		}
		for (int l49 = 0; (l49 < 3); l49 = (l49 + 1)) {
			fRec67[l49] = 0.0;
		}
		for (int l50 = 0; (l50 < 3); l50 = (l50 + 1)) {
			fRec68[l50] = 0.0;
		}
		for (int l51 = 0; (l51 < 3); l51 = (l51 + 1)) {
			fRec69[l51] = 0.0;
		}
		for (int l52 = 0; (l52 < 3); l52 = (l52 + 1)) {
			fRec70[l52] = 0.0;
		}
		for (int l53 = 0; (l53 < 3); l53 = (l53 + 1)) {
			fRec71[l53] = 0.0;
		}
		for (int l54 = 0; (l54 < 3); l54 = (l54 + 1)) {
			fRec72[l54] = 0.0;
		}
		for (int l55 = 0; (l55 < 3); l55 = (l55 + 1)) {
			fRec73[l55] = 0.0;
		}
		for (int l56 = 0; (l56 < 3); l56 = (l56 + 1)) {
			fRec74[l56] = 0.0;
		}
		for (int l57 = 0; (l57 < 3); l57 = (l57 + 1)) {
			fRec75[l57] = 0.0;
		}
		for (int l58 = 0; (l58 < 3); l58 = (l58 + 1)) {
			fRec76[l58] = 0.0;
		}
		for (int l59 = 0; (l59 < 3); l59 = (l59 + 1)) {
			fRec77[l59] = 0.0;
		}
		for (int l60 = 0; (l60 < 2); l60 = (l60 + 1)) {
			iRec10[l60] = 0;
		}
		for (int l61 = 0; (l61 < 2); l61 = (l61 + 1)) {
			iRec6[l61] = 0;
		}
		for (int l62 = 0; (l62 < 2048); l62 = (l62 + 1)) {
			fRec2[l62] = 0.0;
		}
		for (int l63 = 0; (l63 < 2); l63 = (l63 + 1)) {
			fRec0[l63] = 0.0;
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
		ui_interface->openVerticalBox("modularInterpInstr");
		ui_interface->declare(0, "0", "");
		ui_interface->openHorizontalBox("midi");
		ui_interface->declare(&fHslider2, "0", "");
		ui_interface->declare(&fHslider2, "style", "knob");
		ui_interface->addHorizontalSlider("freq", &fHslider2, 440.0, 50.0, 1000.0, 0.01);
		ui_interface->declare(&fHslider4, "1", "");
		ui_interface->declare(&fHslider4, "hidden", "1");
		ui_interface->declare(&fHslider4, "midi", "pitchwheel");
		ui_interface->declare(&fHslider4, "style", "knob");
		ui_interface->addHorizontalSlider("bend", &fHslider4, 0.0, -2.0, 2.0, 0.01);
		ui_interface->declare(&fHslider5, "2", "");
		ui_interface->declare(&fHslider5, "style", "knob");
		ui_interface->addHorizontalSlider("gain", &fHslider5, 0.80000000000000004, 0.0, 1.0, 0.01);
		ui_interface->declare(&fHslider3, "3", "");
		ui_interface->declare(&fHslider3, "hidden", "1");
		ui_interface->declare(&fHslider3, "midi", "ctrl 64");
		ui_interface->declare(&fHslider3, "style", "knob");
		ui_interface->addHorizontalSlider("sustain", &fHslider3, 0.0, 0.0, 1.0, 1.0);
		ui_interface->closeBox();
		ui_interface->declare(0, "1", "");
		ui_interface->openHorizontalBox("body");
		ui_interface->declare(&fHslider7, "0", "");
		ui_interface->declare(&fHslider7, "style", "knob");
		ui_interface->addHorizontalSlider("shape", &fHslider7, 0.0, 0.0, 1.0, 0.01);
		ui_interface->declare(&fHslider6, "1", "");
		ui_interface->declare(&fHslider6, "style", "knob");
		ui_interface->addHorizontalSlider("scale", &fHslider6, 0.0, 0.0, 1.0, 0.010999999999999999);
		ui_interface->declare(&fButton1, "2", "");
		ui_interface->addButton("tapBody", &fButton1);
		ui_interface->closeBox();
		ui_interface->declare(&fHslider1, "2", "");
		ui_interface->declare(&fHslider1, "midi", "ctrl 1");
		ui_interface->addHorizontalSlider("pluckPosition", &fHslider1, 0.80000000000000004, 0.0, 1.0, 0.01);
		ui_interface->declare(&fHslider0, "3", "");
		ui_interface->addHorizontalSlider("outGain", &fHslider0, 0.5, 0.0, 1.0, 0.01);
		ui_interface->declare(&fButton0, "4", "");
		ui_interface->addButton("gate", &fButton0);
		ui_interface->closeBox();
	}
	
	virtual void compute(int count, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs) {
		FAUSTFLOAT* output0 = outputs[0];
		FAUSTFLOAT* output1 = outputs[1];
		double fSlow0 = (0.5 * double(fHslider0));
		double fSlow1 = (0.0010000000000000009 * double(fHslider1));
		double fSlow2 = double(fHslider2);
		double fSlow3 = (340.0 / fSlow2);
		double fSlow4 = std::min<double>(1.0, (double(fHslider3) + double(fButton0)));
		int iSlow5 = (fSlow4 == 0.0);
		double fSlow6 = std::pow(2.0, (0.083333333333333329 * double(fHslider4)));
		double fSlow7 = double(fHslider5);
		double fSlow8 = (fConst3 * fSlow2);
		double fSlow9 = (0.00050000000000000001 * fSlow2);
		double fSlow10 = double(fButton1);
		double fSlow11 = (2.0 * double(fHslider6));
		int iSlow12 = (fSlow11 < 1.0);
		double fSlow13 = double(fHslider7);
		double fSlow14 = (2.0 * fSlow13);
		int iSlow15 = (fSlow14 < 1.0);
		double fSlow16 = (fSlow14 + -1.0);
		double fSlow17 = (iSlow15 ? (0.23606099999999999 - (0.050781999999999994 * fSlow13)) : (0.21067 - (0.017016000000000003 * fSlow16)));
		double fSlow18 = (iSlow12 ? (iSlow15 ? ((0.068907999999999997 * fSlow13) + 0.102256) : ((0.063330999999999998 * fSlow16) + 0.13671)) : fSlow17);
		double fSlow19 = (iSlow12 ? fSlow11 : (fSlow11 + -1.0));
		double fSlow20 = std::pow(0.001, (fConst5 / (fSlow18 + (((iSlow12 ? fSlow17 : (iSlow15 ? (0.41711300000000001 - (0.444104 * fSlow13)) : ((0.088483000000000006 * fSlow16) + 0.19506100000000001))) - fSlow18) * fSlow19))));
		double fSlow21 = (0.0 - (2.0 * fSlow20));
		double fSlow22 = (iSlow15 ? ((20.536300000000097 * fSlow13) + 306.24339099999997) : ((64.399121999999977 * fSlow16) + 316.51154100000002));
		double fSlow23 = (iSlow12 ? (iSlow15 ? ((131.14035200000001 * fSlow13) + 375.008194) : ((70.541131000000007 * fSlow16) + 440.57837000000001)) : fSlow22);
		double fSlow24 = std::cos((fConst6 * (fSlow23 + (fSlow19 * ((iSlow12 ? fSlow22 : (iSlow15 ? ((193.16238999999996 * fSlow13) + 222.231629) : (318.81282399999998 - (32.67841999999996 * fSlow16)))) - fSlow23)))));
		double fSlow25 = mydsp_faustpower2_f(fSlow20);
		double fSlow26 = (iSlow15 ? (1.0 - (0.00071799999999999642 * fSlow13)) : ((0.00035899999999999821 * fSlow16) + 0.999641));
		double fSlow27 = (iSlow12 ? (iSlow15 ? ((0.017859999999999987 * fSlow13) + 0.99107000000000001) : 1.0) : fSlow26);
		double fSlow28 = (fSlow27 + (fSlow19 * ((iSlow12 ? fSlow26 : (iSlow15 ? ((0.8375459999999999 * fSlow13) + 0.58122700000000005) : 1.0)) - fSlow27)));
		double fSlow29 = (iSlow15 ? (0.17900099999999999 - (0.233344 * fSlow13)) : ((0.013726999999999996 * fSlow16) + 0.062329000000000002));
		double fSlow30 = (iSlow12 ? (iSlow15 ? (0.088068999999999995 - (0.034825999999999996 * fSlow13)) : (0.070655999999999997 - (0.040456999999999993 * fSlow16))) : fSlow29);
		double fSlow31 = std::pow(0.001, (fConst5 / (fSlow30 + (fSlow19 * ((iSlow12 ? fSlow29 : (iSlow15 ? ((0.043535999999999964 * fSlow13) + 0.24798000000000001) : (0.26974799999999999 - (0.193025 * fSlow16)))) - fSlow30)))));
		double fSlow32 = (0.0 - (2.0 * fSlow31));
		double fSlow33 = (iSlow15 ? ((244.43377799999996 * fSlow13) + 426.38155599999999) : ((67.264386000000059 * fSlow16) + 548.59844499999997));
		double fSlow34 = (iSlow12 ? (iSlow15 ? ((219.34688399999982 * fSlow13) + 527.94558400000005) : ((223.27842700000008 * fSlow16) + 637.61902599999996)) : fSlow33);
		double fSlow35 = std::cos((fConst6 * (fSlow34 + (fSlow19 * ((iSlow12 ? fSlow33 : (iSlow15 ? ((418.60621199999991 * fSlow13) + 336.34361100000001) : (545.64671699999997 - (69.262261999999964 * fSlow16)))) - fSlow34)))));
		double fSlow36 = mydsp_faustpower2_f(fSlow31);
		double fSlow37 = (iSlow15 ? ((1.0111780000000001 * fSlow13) + 0.29259200000000002) : (0.79818100000000003 - (0.36068600000000001 * fSlow16)));
		double fSlow38 = (iSlow12 ? (iSlow15 ? (1.0 - (1.4056060000000001 * fSlow13)) : ((0.38937300000000002 * fSlow16) + 0.29719699999999999)) : fSlow37);
		double fSlow39 = (fSlow38 + (fSlow19 * ((iSlow12 ? fSlow37 : (iSlow15 ? (1.0 - (1.22787 * fSlow13)) : ((0.22364600000000001 * fSlow16) + 0.38606499999999999))) - fSlow38)));
		double fSlow40 = (iSlow15 ? ((0.02606 * fSlow13) + 0.084440000000000001) : ((0.100047 * fSlow16) + 0.097470000000000001));
		double fSlow41 = (iSlow12 ? (iSlow15 ? ((0.047064000000000002 * fSlow13) + 0.026433000000000002) : ((0.00088999999999999496 * fSlow16) + 0.049965000000000002)) : fSlow40);
		double fSlow42 = std::pow(0.001, (fConst5 / (fSlow41 + (fSlow19 * ((iSlow12 ? fSlow40 : (iSlow15 ? ((0.0091960000000000097 * fSlow13) + 0.10379099999999999) : ((0.17903399999999997 * fSlow16) + 0.108389))) - fSlow41)))));
		double fSlow43 = (0.0 - (2.0 * fSlow42));
		double fSlow44 = (iSlow15 ? ((318.60205799999994 * fSlow13) + 523.626577) : ((250.92804999999998 * fSlow16) + 682.92760599999997));
		double fSlow45 = (iSlow12 ? (iSlow15 ? ((106.36015399999997 * fSlow13) + 775.00256000000002) : ((149.49305300000003 * fSlow16) + 828.182637)) : fSlow44);
		double fSlow46 = std::cos((fConst6 * (fSlow45 + (fSlow19 * ((iSlow12 ? fSlow44 : (iSlow15 ? ((344.12529599999993 * fSlow13) + 555.04106899999999) : (727.10371699999996 - (124.23113000000001 * fSlow16)))) - fSlow45)))));
		double fSlow47 = mydsp_faustpower2_f(fSlow42);
		double fSlow48 = (iSlow15 ? (0.55471000000000004 - (0.70926 * fSlow13)) : (0.20008000000000001 - (0.089482000000000006 * fSlow16)));
		double fSlow49 = (iSlow12 ? (iSlow15 ? ((0.39830199999999993 * fSlow13) + 0.51594600000000002) : (0.71509699999999998 - (0.35248099999999999 * fSlow16))) : fSlow48);
		double fSlow50 = (fSlow49 + (fSlow19 * ((iSlow12 ? fSlow48 : (iSlow15 ? ((0.26111400000000001 * fSlow13) + 0.16736599999999999) : (0.29792299999999999 - (0.075548000000000004 * fSlow16)))) - fSlow49)));
		double fSlow51 = (iSlow15 ? (0.16519700000000001 - (0.056790000000000007 * fSlow13)) : (0.13680200000000001 - (0.11732700000000001 * fSlow16)));
		double fSlow52 = (iSlow12 ? (iSlow15 ? (0.131998 - (0.19578800000000002 * fSlow13)) : (0.034104000000000002 - (0.013969000000000002 * fSlow16))) : fSlow51);
		double fSlow53 = std::pow(0.001, (fConst5 / (fSlow52 + (fSlow19 * ((iSlow12 ? fSlow51 : (iSlow15 ? ((0.0026460000000000372 * fSlow13) + 0.13123399999999999) : (0.13255700000000001 - (0.07416600000000001 * fSlow16)))) - fSlow52)))));
		double fSlow54 = (0.0 - (2.0 * fSlow53));
		double fSlow55 = (iSlow15 ? ((252.54912999999988 * fSlow13) + 637.81365500000004) : ((364.6816050000001 * fSlow16) + 764.08821999999998));
		double fSlow56 = (iSlow12 ? (iSlow15 ? (981.58426199999997 - (6.6099899999999252 * fSlow13)) : ((267.17539999999997 * fSlow16) + 978.279267)) : fSlow55);
		double fSlow57 = std::cos((fConst6 * (fSlow56 + (fSlow19 * ((iSlow12 ? fSlow55 : (iSlow15 ? ((82.373140000000149 * fSlow13) + 772.21966199999997) : ((18.417112999999972 * fSlow16) + 813.40623200000005))) - fSlow56)))));
		double fSlow58 = mydsp_faustpower2_f(fSlow53);
		double fSlow59 = (iSlow15 ? (0.359595 - (0.15824400000000005 * fSlow13)) : (0.28047299999999997 - (0.25461499999999998 * fSlow16)));
		double fSlow60 = (iSlow12 ? (iSlow15 ? (0.54991100000000004 - (0.74220000000000008 * fSlow13)) : (0.178811 - (0.00012599999999998723 * fSlow16))) : fSlow59);
		double fSlow61 = (fSlow60 + (fSlow19 * ((iSlow12 ? fSlow59 : (iSlow15 ? (0.663184 - (0.85602599999999995 * fSlow13)) : (0.23517099999999999 - (0.20966599999999999 * fSlow16)))) - fSlow60)));
		double fSlow62 = (iSlow15 ? (0.083835999999999994 - (0.012091999999999992 * fSlow13)) : (0.077789999999999998 - (0.034702999999999998 * fSlow16)));
		double fSlow63 = (iSlow12 ? (iSlow15 ? (0.050148999999999999 - (0.052351999999999996 * fSlow13)) : (0.023973000000000001 - (0.0038720000000000004 * fSlow16))) : fSlow62);
		double fSlow64 = std::pow(0.001, (fConst5 / (fSlow63 + (fSlow19 * ((iSlow12 ? fSlow62 : (iSlow15 ? (0.19476399999999999 - (0.13527800000000001 * fSlow13)) : (0.12712499999999999 - (0.051952999999999985 * fSlow16)))) - fSlow63)))));
		double fSlow65 = (0.0 - (2.0 * fSlow64));
		double fSlow66 = (iSlow15 ? ((580.43766400000004 * fSlow13) + 855.57638999999995) : ((85.558230000000094 * fSlow16) + 1145.795222));
		double fSlow67 = (iSlow12 ? (iSlow15 ? (1125.2038339999999 - (73.603845999999976 * fSlow13)) : ((320.421288 * fSlow16) + 1088.4019109999999)) : fSlow66);
		double fSlow68 = std::cos((fConst6 * (fSlow67 + (fSlow19 * ((iSlow12 ? fSlow66 : (iSlow15 ? ((53.529708000000028 * fSlow13) + 911.26144199999999) : (938.026296 - (75.252685000000042 * fSlow16)))) - fSlow67)))));
		double fSlow69 = mydsp_faustpower2_f(fSlow64);
		double fSlow70 = (iSlow15 ? ((0.0030759999999999954 * fSlow13) + 0.041223000000000003) : (0.042761 - (0.031920000000000004 * fSlow16)));
		double fSlow71 = (iSlow12 ? (iSlow15 ? ((0.22816800000000001 * fSlow13) + 0.11527900000000001) : (0.22936300000000001 - (0.11538200000000001 * fSlow16))) : fSlow70);
		double fSlow72 = (fSlow71 + (fSlow19 * ((iSlow12 ? fSlow70 : (iSlow15 ? ((0.43883999999999995 * fSlow13) + 0.12717300000000001) : (0.34659299999999998 - (0.32048499999999996 * fSlow16)))) - fSlow71)));
		double fSlow73 = (iSlow15 ? (0.055543000000000002 - (0.059332000000000003 * fSlow13)) : ((0.037952 * fSlow16) + 0.025877000000000001));
		double fSlow74 = (iSlow12 ? (iSlow15 ? (0.052897 - (0.039814000000000002 * fSlow13)) : (0.032989999999999998 - (0.0060799999999999986 * fSlow16))) : fSlow73);
		double fSlow75 = std::pow(0.001, (fConst5 / (fSlow74 + (fSlow19 * ((iSlow12 ? fSlow73 : (iSlow15 ? (0.13056899999999999 - (0.033619999999999983 * fSlow13)) : ((0.0070349999999999996 * fSlow16) + 0.113759))) - fSlow74)))));
		double fSlow76 = (0.0 - (2.0 * fSlow75));
		double fSlow77 = (iSlow15 ? ((575.73707599999989 * fSlow13) + 992.20045400000004) : ((124.87910000000011 * fSlow16) + 1280.068992));
		double fSlow78 = (iSlow12 ? (iSlow15 ? ((67.22860400000036 * fSlow13) + 1287.7013979999999) : ((283.16319099999987 * fSlow16) + 1321.3157000000001)) : fSlow77);
		double fSlow79 = std::cos((fConst6 * (fSlow78 + (fSlow19 * ((iSlow12 ? fSlow77 : (iSlow15 ? (1096.774273 - (58.602939999999762 * fSlow13)) : (1067.4728030000001 - (42.487490000000207 * fSlow16)))) - fSlow78)))));
		double fSlow80 = mydsp_faustpower2_f(fSlow75);
		double fSlow81 = (iSlow15 ? (0.28324899999999997 - (0.40658799999999995 * fSlow13)) : (0.079954999999999998 - (0.057554999999999995 * fSlow16)));
		double fSlow82 = (iSlow12 ? (iSlow15 ? ((0.051835999999999993 * fSlow13) + 0.15781300000000001) : (0.18373100000000001 - (0.025003999999999998 * fSlow16))) : fSlow81);
		double fSlow83 = (fSlow82 + (fSlow19 * ((iSlow12 ? fSlow81 : (iSlow15 ? ((0.15533800000000003 * fSlow13) + 0.14142399999999999) : (0.21909300000000001 - (0.19225300000000001 * fSlow16)))) - fSlow82)));
		double fSlow84 = (iSlow15 ? (0.067029000000000005 - (0.02479400000000001 * fSlow13)) : ((0.019675999999999999 * fSlow16) + 0.054632));
		double fSlow85 = (iSlow12 ? (iSlow15 ? (0.066531000000000007 - (0.033852000000000007 * fSlow13)) : (0.049605000000000003 - (0.0063560000000000005 * fSlow16))) : fSlow84);
		double fSlow86 = std::pow(0.001, (fConst5 / (fSlow85 + (fSlow19 * ((iSlow12 ? fSlow84 : (iSlow15 ? ((0.07979799999999998 * fSlow13) + 0.119615) : (0.15951399999999999 - (0.027973999999999999 * fSlow16)))) - fSlow85)))));
		double fSlow87 = (0.0 - (2.0 * fSlow86));
		double fSlow88 = (iSlow15 ? ((851.39929800000027 * fSlow13) + 1098.863347) : ((240.07824299999993 * fSlow16) + 1524.5629960000001));
		double fSlow89 = (iSlow12 ? (iSlow15 ? ((508.28413399999999 * fSlow13) + 1394.790058) : ((176.92565100000002 * fSlow16) + 1648.932125)) : fSlow88);
		double fSlow90 = std::cos((fConst6 * (fSlow89 + (fSlow19 * ((iSlow12 ? fSlow88 : (iSlow15 ? ((72.416506000000027 * fSlow13) + 1203.1028240000001) : (1239.3110770000001 - (29.378648999999996 * fSlow16)))) - fSlow89)))));
		double fSlow91 = mydsp_faustpower2_f(fSlow86);
		double fSlow92 = (iSlow15 ? ((0.20923200000000003 * fSlow13) + 0.182057) : (0.28667300000000001 - (0.041432000000000024 * fSlow16)));
		double fSlow93 = (iSlow12 ? (iSlow15 ? (0.46374300000000002 - (0.71211599999999997 * fSlow13)) : (0.107685 - (0.083617999999999998 * fSlow16))) : fSlow92);
		double fSlow94 = (fSlow93 + (fSlow19 * ((iSlow12 ? fSlow92 : (iSlow15 ? (0.232076 - (0.362842 * fSlow13)) : ((0.55511200000000005 * fSlow16) + 0.050654999999999999))) - fSlow93)));
		double fSlow95 = (iSlow15 ? ((0.011875999999999998 * fSlow13) + 0.054546999999999998) : (0.060484999999999997 - (0.00069400000000000017 * fSlow16)));
		double fSlow96 = (iSlow12 ? (iSlow15 ? (0.060621000000000001 - (0.067646000000000012 * fSlow13)) : ((0.014016000000000004 * fSlow16) + 0.026797999999999999)) : fSlow95);
		double fSlow97 = std::pow(0.001, (fConst5 / (fSlow96 + (fSlow19 * ((iSlow12 ? fSlow95 : (iSlow15 ? (0.103862 - (0.090669999999999987 * fSlow13)) : ((0.022645999999999993 * fSlow16) + 0.058527000000000003))) - fSlow96)))));
		double fSlow98 = (0.0 - (2.0 * fSlow97));
		double fSlow99 = (iSlow15 ? ((900.44768199999999 * fSlow13) + 1226.9227840000001) : ((418.43991099999971 * fSlow16) + 1677.1466250000001));
		double fSlow100 = (iSlow12 ? (iSlow15 ? ((480.15624799999978 * fSlow13) + 1577.1386660000001) : ((143.76831500000003 * fSlow16) + 1817.2167899999999)) : fSlow99);
		double fSlow101 = std::cos((fConst6 * (fSlow100 + (fSlow19 * ((iSlow12 ? fSlow99 : (iSlow15 ? ((148.796468 * fSlow13) + 1404.0110010000001) : ((37.139363000000003 * fSlow16) + 1478.4092350000001))) - fSlow100)))));
		double fSlow102 = mydsp_faustpower2_f(fSlow97);
		double fSlow103 = (iSlow15 ? ((0.16797199999999998 * fSlow13) + 0.078666) : ((0.025337999999999999 * fSlow16) + 0.16265199999999999));
		double fSlow104 = (iSlow12 ? (iSlow15 ? (0.206985 - (0.17244200000000001 * fSlow13)) : (0.120764 - (0.097104999999999997 * fSlow16))) : fSlow103);
		double fSlow105 = (fSlow104 + (fSlow19 * ((iSlow12 ? fSlow103 : (iSlow15 ? ((0.53315400000000002 * fSlow13) + 0.043454) : ((0.058155000000000012 * fSlow16) + 0.310031))) - fSlow104)));
		double fSlow106 = (iSlow15 ? ((0.030859999999999999 * fSlow13) + 0.046195) : ((0.020990999999999996 * fSlow16) + 0.061624999999999999));
		double fSlow107 = (iSlow12 ? (iSlow15 ? (0.045315000000000001 - (0.041076000000000001 * fSlow13)) : ((0.033350999999999999 * fSlow16) + 0.024777)) : fSlow106);
		double fSlow108 = std::pow(0.001, (fConst5 / (fSlow107 + (fSlow19 * ((iSlow12 ? fSlow106 : (iSlow15 ? (0.102785 - (0.070903999999999995 * fSlow13)) : ((0.021215999999999999 * fSlow16) + 0.067333000000000004))) - fSlow107)))));
		double fSlow109 = (0.0 - (2.0 * fSlow108));
		double fSlow110 = (iSlow15 ? ((1075.9004960000002 * fSlow13) + 1545.9505220000001) : ((367.31977799999959 * fSlow16) + 2083.9007700000002));
		double fSlow111 = (iSlow12 ? (iSlow15 ? ((421.72274800000014 * fSlow13) + 1727.825366) : ((401.51763699999992 * fSlow16) + 1938.6867400000001)) : fSlow110);
		double fSlow112 = std::cos((fConst6 * (fSlow111 + (fSlow19 * ((iSlow12 ? fSlow110 : (iSlow15 ? ((53.892363999999816 * fSlow13) + 1540.420421) : ((210.452988 * fSlow16) + 1567.3666029999999))) - fSlow111)))));
		double fSlow113 = mydsp_faustpower2_f(fSlow108);
		double fSlow114 = (iSlow15 ? ((0.20359400000000005 * fSlow13) + 0.89820299999999997) : (1.0 - (0.97035000000000005 * fSlow16)));
		double fSlow115 = (iSlow12 ? (iSlow15 ? (0.131491 - (0.14211000000000001 * fSlow13)) : ((0.024676999999999998 * fSlow16) + 0.060435999999999997)) : fSlow114);
		double fSlow116 = (fSlow115 + (fSlow19 * ((iSlow12 ? fSlow114 : (iSlow15 ? ((0.013411999999999979 * fSlow13) + 0.168681) : (0.17538699999999999 - (0.081860999999999989 * fSlow16)))) - fSlow115)));
		double fSlow117 = (iSlow15 ? ((0.01192399999999999 * fSlow13) + 0.055390000000000002) : ((0.020596999999999997 * fSlow16) + 0.061351999999999997));
		double fSlow118 = (iSlow12 ? (iSlow15 ? (0.047412999999999997 - (0.013483999999999996 * fSlow13)) : ((0.0068189999999999987 * fSlow16) + 0.040670999999999999)) : fSlow117);
		double fSlow119 = std::pow(0.001, (fConst5 / (fSlow118 + (fSlow19 * ((iSlow12 ? fSlow117 : (iSlow15 ? (0.079975000000000004 - (0.015958 * fSlow13)) : (0.071996000000000004 - (0.037005000000000003 * fSlow16)))) - fSlow118)))));
		double fSlow120 = (0.0 - (2.0 * fSlow119));
		double fSlow121 = (iSlow15 ? ((1162.0150019999996 * fSlow13) + 1704.776255) : ((280.65183200000001 * fSlow16) + 2285.7837559999998));
		double fSlow122 = (iSlow12 ? (iSlow15 ? ((626.394452 * fSlow13) + 1860.70027) : ((420.43996400000015 * fSlow16) + 2173.897496)) : fSlow121);
		double fSlow123 = std::cos((fConst6 * (fSlow122 + (fSlow19 * ((iSlow12 ? fSlow121 : (iSlow15 ? ((422.54204999999956 * fSlow13) + 1628.9448400000001) : ((161.59037600000011 * fSlow16) + 1840.2158649999999))) - fSlow122)))));
		double fSlow124 = mydsp_faustpower2_f(fSlow119);
		double fSlow125 = (iSlow15 ? ((0.40984800000000005 * fSlow13) + 0.105021) : (0.30994500000000003 - (0.25177100000000002 * fSlow16)));
		double fSlow126 = (iSlow12 ? (iSlow15 ? ((0.51861399999999991 * fSlow13) + 0.24405399999999999) : (0.50336099999999995 - (0.31113699999999994 * fSlow16))) : fSlow125);
		double fSlow127 = (fSlow126 + (fSlow19 * ((iSlow12 ? fSlow125 : (iSlow15 ? (0.24490799999999999 - (0.12888799999999995 * fSlow13)) : (0.18046400000000001 - (0.16380800000000001 * fSlow16)))) - fSlow126)));
		double fSlow128 = (iSlow15 ? ((0.015708 * fSlow13) + 0.052760000000000001) : (0.060614000000000001 - (0.017759000000000004 * fSlow16)));
		double fSlow129 = (iSlow12 ? (iSlow15 ? (0.058375000000000003 - (0.03822600000000001 * fSlow13)) : ((0.011927 * fSlow16) + 0.039261999999999998)) : fSlow128);
		double fSlow130 = std::pow(0.001, (fConst5 / (fSlow129 + (fSlow19 * ((iSlow12 ? fSlow128 : (iSlow15 ? (0.068871000000000002 - (0.0037399999999999933 * fSlow13)) : (0.067001000000000005 - (0.0067640000000000061 * fSlow16)))) - fSlow129)))));
		double fSlow131 = (0.0 - (2.0 * fSlow130));
		double fSlow132 = (iSlow15 ? ((1226.1273660000002 * fSlow13) + 1839.163057) : ((329.24037999999973 * fSlow16) + 2452.2267400000001));
		double fSlow133 = (iSlow12 ? (iSlow15 ? ((1000.3159339999997 * fSlow13) + 1950.8090589999999) : ((249.07467200000019 * fSlow16) + 2450.9670259999998)) : fSlow132);
		double fSlow134 = std::cos((fConst6 * (fSlow133 + (fSlow19 * ((iSlow12 ? fSlow132 : (iSlow15 ? ((334.74774600000001 * fSlow13) + 1994.925663) : ((511.83589699999993 * fSlow16) + 2162.299536))) - fSlow133)))));
		double fSlow135 = mydsp_faustpower2_f(fSlow130);
		double fSlow136 = (iSlow15 ? ((0.120742 * fSlow13) + 0.037156000000000002) : (0.097527000000000003 - (0.090492000000000003 * fSlow16)));
		double fSlow137 = (iSlow12 ? (iSlow15 ? ((0.38430000000000003 * fSlow13) + 0.16011400000000001) : (0.35226400000000002 - (0.17785200000000001 * fSlow16))) : fSlow136);
		double fSlow138 = (fSlow137 + (fSlow19 * ((iSlow12 ? fSlow136 : (iSlow15 ? (0.19334299999999999 - (0.25725399999999998 * fSlow13)) : ((0.094883999999999996 * fSlow16) + 0.064715999999999996))) - fSlow137)));
		double fSlow139 = (iSlow15 ? (0.079020999999999994 - (0.096307999999999991 * fSlow13)) : ((0.0052729999999999999 * fSlow16) + 0.030866999999999999));
		double fSlow140 = (iSlow12 ? (iSlow15 ? (0.059097999999999998 - (0.047715999999999995 * fSlow13)) : ((0.033822999999999999 * fSlow16) + 0.03524)) : fSlow139);
		double fSlow141 = std::pow(0.001, (fConst5 / (fSlow140 + (fSlow19 * ((iSlow12 ? fSlow139 : (iSlow15 ? (0.12083000000000001 - (0.133272 * fSlow13)) : (0.054193999999999999 - (0.040041 * fSlow16)))) - fSlow140)))));
		double fSlow142 = (0.0 - (2.0 * fSlow141));
		double fSlow143 = (iSlow15 ? ((1590.8833159999999 * fSlow13) + 1983.832482) : ((167.48325800000021 * fSlow16) + 2779.27414));
		double fSlow144 = (iSlow12 ? (iSlow15 ? ((1124.2871600000003 * fSlow13) + 2020.1690630000001) : ((695.45399999999972 * fSlow16) + 2582.3126430000002)) : fSlow143);
		double fSlow145 = std::cos((fConst6 * (fSlow144 + (fSlow19 * ((iSlow12 ? fSlow143 : (iSlow15 ? ((211.10791400000016 * fSlow13) + 2197.106949) : ((851.62123799999972 * fSlow16) + 2302.6609060000001))) - fSlow144)))));
		double fSlow146 = mydsp_faustpower2_f(fSlow141);
		double fSlow147 = (iSlow15 ? (0.128493 - (0.20905799999999999 * fSlow13)) : (0.023963999999999999 - (0.010775999999999999 * fSlow16)));
		double fSlow148 = (iSlow12 ? (iSlow15 ? (0.116274 - (0.119684 * fSlow13)) : ((0.10384699999999999 * fSlow16) + 0.056432000000000003)) : fSlow147);
		double fSlow149 = (fSlow148 + (fSlow19 * ((iSlow12 ? fSlow147 : (iSlow15 ? (0.14754300000000001 - (0.153644 * fSlow13)) : (0.070721000000000006 - (0.056885000000000005 * fSlow16)))) - fSlow148)));
		double fSlow150 = (iSlow15 ? (0.073764999999999997 - (0.068649999999999989 * fSlow13)) : ((0.029123999999999997 * fSlow16) + 0.039440000000000003));
		double fSlow151 = (iSlow12 ? (iSlow15 ? (0.029950000000000001 - (0.021472000000000005 * fSlow13)) : ((0.028129999999999999 * fSlow16) + 0.019213999999999998)) : fSlow150);
		double fSlow152 = std::pow(0.001, (fConst5 / (fSlow151 + (fSlow19 * ((iSlow12 ? fSlow150 : (iSlow15 ? (0.066401000000000002 - (0.02716600000000001 * fSlow13)) : ((0.021891000000000001 * fSlow16) + 0.052817999999999997))) - fSlow151)))));
		double fSlow153 = (0.0 - (2.0 * fSlow152));
		double fSlow154 = (iSlow15 ? ((2011.1827899999998 * fSlow13) + 2190.8145709999999) : ((325.64192500000036 * fSlow16) + 3196.4059659999998));
		double fSlow155 = (iSlow12 ? (iSlow15 ? ((1124.4084979999998 * fSlow13) + 2185.7988789999999) : ((640.50750400000015 * fSlow16) + 2748.0031279999998)) : fSlow154);
		double fSlow156 = std::cos((fConst6 * (fSlow155 + (fSlow19 * ((iSlow12 ? fSlow154 : (iSlow15 ? (2533.3542440000001 - (278.69505800000024 * fSlow13)) : ((935.5489419999999 * fSlow16) + 2394.006715))) - fSlow155)))));
		double fSlow157 = mydsp_faustpower2_f(fSlow152);
		double fSlow158 = (iSlow15 ? (0.30256899999999998 - (0.35607999999999995 * fSlow13)) : (0.124529 - (0.114762 * fSlow16)));
		double fSlow159 = (iSlow12 ? (iSlow15 ? (0.16167799999999999 - (0.23750399999999999 * fSlow13)) : (0.042925999999999999 - (0.0006509999999999988 * fSlow16))) : fSlow158);
		double fSlow160 = (fSlow159 + (fSlow19 * ((iSlow12 ? fSlow158 : (iSlow15 ? (0.062213999999999998 - (0.069629999999999997 * fSlow13)) : ((0.016600999999999998 * fSlow16) + 0.027399))) - fSlow159)));
		double fSlow161 = (iSlow15 ? (0.059743999999999998 - (0.059379999999999995 * fSlow13)) : ((0.011656999999999997 * fSlow16) + 0.030054000000000001));
		double fSlow162 = (iSlow12 ? (iSlow15 ? ((0.068927999999999989 * fSlow13) + 0.018537000000000001) : (0.053001 - (0.0035530000000000006 * fSlow16))) : fSlow161);
		double fSlow163 = std::pow(0.001, (fConst5 / (fSlow162 + (fSlow19 * ((iSlow12 ? fSlow161 : (iSlow15 ? (0.064963999999999994 - (0.028057999999999986 * fSlow13)) : ((0.017957999999999995 * fSlow16) + 0.050935000000000001))) - fSlow162)))));
		double fSlow164 = (0.0 - (2.0 * fSlow163));
		double fSlow165 = (iSlow15 ? ((2247.7061359999998 * fSlow13) + 2445.4098600000002) : ((179.11232599999994 * fSlow16) + 3569.2629280000001));
		double fSlow166 = (iSlow12 ? (iSlow15 ? ((951.89782799999921 * fSlow13) + 2382.3439830000002) : ((646.80760800000007 * fSlow16) + 2858.2928969999998)) : fSlow165);
		double fSlow167 = std::cos((fConst6 * (fSlow166 + (fSlow19 * ((iSlow12 ? fSlow165 : (iSlow15 ? (2672.4701650000002 - (346.14607200000046 * fSlow13)) : ((931.98846000000003 * fSlow16) + 2499.3971289999999))) - fSlow166)))));
		double fSlow168 = mydsp_faustpower2_f(fSlow163);
		double fSlow169 = (iSlow15 ? ((0.042466000000000004 * fSlow13) + 0.13408100000000001) : (0.15531400000000001 - (0.096030000000000004 * fSlow16)));
		double fSlow170 = (iSlow12 ? (iSlow15 ? ((0.084134 * fSlow13) + 0.048839) : (0.090906000000000001 - (0.048702000000000002 * fSlow16))) : fSlow169);
		double fSlow171 = (fSlow170 + (fSlow19 * ((iSlow12 ? fSlow169 : (iSlow15 ? (0.112585 - (0.07122400000000001 * fSlow13)) : (0.076973 - (0.053870000000000001 * fSlow16)))) - fSlow170)));
		double fSlow172 = (iSlow15 ? (0.071779999999999997 - (0.067648 * fSlow13)) : ((0.005843000000000001 * fSlow16) + 0.037955999999999997));
		double fSlow173 = (iSlow12 ? (iSlow15 ? ((0.043661999999999999 * fSlow13) + 0.024086) : (0.045916999999999999 - (0.021197000000000001 * fSlow16))) : fSlow172);
		double fSlow174 = std::pow(0.001, (fConst5 / (fSlow173 + (fSlow19 * ((iSlow12 ? fSlow172 : (iSlow15 ? ((0.016201999999999994 * fSlow13) + 0.037272) : (0.045372999999999997 - (0.012018999999999995 * fSlow16)))) - fSlow173)))));
		double fSlow175 = (0.0 - (2.0 * fSlow174));
		double fSlow176 = (iSlow15 ? ((2250.6884 * fSlow13) + 2672.9075790000002) : ((94.100803999999698 * fSlow16) + 3798.2517790000002));
		double fSlow177 = (iSlow12 ? (iSlow15 ? ((771.72554800000034 * fSlow13) + 2587.1407589999999) : ((1178.454232 * fSlow16) + 2973.0035330000001)) : fSlow176);
		double fSlow178 = std::cos((fConst6 * (fSlow177 + (fSlow19 * ((iSlow12 ? fSlow176 : (iSlow15 ? (2821.185962 - (193.04679599999963 * fSlow13)) : ((948.99443099999962 * fSlow16) + 2724.6625640000002))) - fSlow177)))));
		double fSlow179 = mydsp_faustpower2_f(fSlow174);
		double fSlow180 = (iSlow15 ? (0.090180999999999997 - (0.029161999999999993 * fSlow13)) : ((0.08524799999999999 * fSlow16) + 0.075600000000000001));
		double fSlow181 = (iSlow12 ? (iSlow15 ? ((0.30712400000000001 * fSlow13) + 0.071332000000000007) : (0.22489400000000001 - (0.19767500000000002 * fSlow16))) : fSlow180);
		double fSlow182 = (fSlow181 + (fSlow19 * ((iSlow12 ? fSlow180 : (iSlow15 ? (0.38389899999999999 - (0.59506400000000004 * fSlow13)) : ((0.021603999999999998 * fSlow16) + 0.086366999999999999))) - fSlow181)));
		double fSlow183 = (iSlow15 ? (0.050509999999999999 - (0.039123999999999999 * fSlow13)) : (0.030948 - (0.0049599999999999991 * fSlow16)));
		double fSlow184 = (iSlow12 ? (iSlow15 ? ((0.025760000000000005 * fSlow13) + 0.032983999999999999) : (0.045864000000000002 - (0.018589000000000001 * fSlow16))) : fSlow183);
		double fSlow185 = std::pow(0.001, (fConst5 / (fSlow184 + (fSlow19 * ((iSlow12 ? fSlow183 : (iSlow15 ? (0.057999000000000002 - (0.01277600000000001 * fSlow13)) : (0.051610999999999997 - (0.019597999999999997 * fSlow16)))) - fSlow184)))));
		double fSlow186 = (0.0 - (2.0 * fSlow185));
		double fSlow187 = (iSlow15 ? ((2577.2428460000001 * fSlow13) + 2782.6427549999999) : ((269.63206800000034 * fSlow16) + 4071.2641779999999));
		double fSlow188 = (iSlow12 ? (iSlow15 ? ((885.65977199999998 * fSlow13) + 2792.6527350000001) : ((1021.825828 * fSlow16) + 3235.4826210000001)) : fSlow187);
		double fSlow189 = std::cos((fConst6 * (fSlow188 + (fSlow19 * ((iSlow12 ? fSlow187 : (iSlow15 ? (2967.4362660000002 - (181.44646000000012 * fSlow13)) : ((1352.0888169999998 * fSlow16) + 2876.7130360000001))) - fSlow188)))));
		double fSlow190 = mydsp_faustpower2_f(fSlow185);
		double fSlow191 = (iSlow15 ? ((0.124552 * fSlow13) + 0.116704) : (0.17898 - (0.171517 * fSlow16)));
		double fSlow192 = (iSlow12 ? (iSlow15 ? ((0.11133799999999996 * fSlow13) + 0.072482000000000005) : (0.12815099999999999 - (0.087645999999999988 * fSlow16))) : fSlow191);
		double fSlow193 = (fSlow192 + (fSlow19 * ((iSlow12 ? fSlow191 : (iSlow15 ? (0.053138999999999999 - (0.057901999999999995 * fSlow13)) : (0.024188000000000001 - (0.010492000000000001 * fSlow16)))) - fSlow192)));
		double fSlow194 = (iSlow15 ? ((0.019622000000000001 * fSlow13) + 0.036325999999999997) : ((0.011357000000000006 * fSlow16) + 0.046136999999999997));
		double fSlow195 = (iSlow12 ? (iSlow15 ? ((0.023804000000000006 * fSlow13) + 0.036867999999999998) : (0.048770000000000001 - (0.020362999999999999 * fSlow16))) : fSlow194);
		double fSlow196 = std::pow(0.001, (fConst5 / (fSlow195 + (fSlow19 * ((iSlow12 ? fSlow194 : (iSlow15 ? ((0.025950000000000001 * fSlow13) + 0.038434999999999997) : (0.051409999999999997 - (0.032321999999999997 * fSlow16)))) - fSlow195)))));
		double fSlow197 = (0.0 - (2.0 * fSlow196));
		double fSlow198 = (iSlow15 ? ((2859.8979079999999 * fSlow13) + 2983.8503759999999) : ((41.052440000000388 * fSlow16) + 4413.7993299999998));
		double fSlow199 = (iSlow12 ? (iSlow15 ? ((756.23604999999952 * fSlow13) + 2970.4438810000001) : ((1076.7572170000003 * fSlow16) + 3348.5619059999999)) : fSlow198);
		double fSlow200 = std::cos((fConst6 * (fSlow199 + (fSlow19 * ((iSlow12 ? fSlow198 : (iSlow15 ? (3342.1896080000001 - (581.48505000000023 * fSlow13)) : ((1338.3949769999999 * fSlow16) + 3051.447083))) - fSlow199)))));
		double fSlow201 = mydsp_faustpower2_f(fSlow196);
		double fSlow202 = (iSlow15 ? ((0.094913999999999998 * fSlow13) + 0.041710999999999998) : (0.089167999999999997 - (0.072352 * fSlow16)));
		double fSlow203 = (iSlow12 ? (iSlow15 ? ((0.31888400000000006 * fSlow13) + 0.097678000000000001) : (0.25712000000000002 - (0.19225400000000004 * fSlow16))) : fSlow202);
		double fSlow204 = (fSlow203 + (fSlow19 * ((iSlow12 ? fSlow202 : (iSlow15 ? (0.10465099999999999 - (0.0081959999999999811 * fSlow13)) : (0.100553 - (0.056938000000000002 * fSlow16)))) - fSlow203)));
		double fSlow205 = (iSlow15 ? ((0.018366000000000007 * fSlow13) + 0.036162) : (0.045345000000000003 - (0.016992000000000004 * fSlow16)));
		double fSlow206 = (iSlow12 ? (iSlow15 ? ((0.017551999999999998 * fSlow13) + 0.032204999999999998) : (0.040980999999999997 - (0.010809999999999997 * fSlow16))) : fSlow205);
		double fSlow207 = std::pow(0.001, (fConst5 / (fSlow206 + (fSlow19 * ((iSlow12 ? fSlow205 : (iSlow15 ? ((0.048689999999999997 * fSlow13) + 0.036927000000000001) : (0.061272 - (0.040279999999999996 * fSlow16)))) - fSlow206)))));
		double fSlow208 = (0.0 - (2.0 * fSlow207));
		double fSlow209 = (iSlow15 ? ((2564.1008519999996 * fSlow13) + 3268.3139780000001) : ((78.938527999999678 * fSlow16) + 4550.3644039999999));
		double fSlow210 = (iSlow12 ? (iSlow15 ? ((897.06103399999938 * fSlow13) + 3047.5156820000002) : ((1030.8326700000002 * fSlow16) + 3496.0461989999999)) : fSlow209);
		double fSlow211 = std::cos((fConst6 * (fSlow210 + (fSlow19 * ((iSlow12 ? fSlow209 : (iSlow15 ? (3479.7233679999999 - (521.21430799999962 * fSlow13)) : ((1333.6291919999999 * fSlow16) + 3219.1162140000001))) - fSlow210)))));
		double fSlow212 = mydsp_faustpower2_f(fSlow207);
		double fSlow213 = (iSlow15 ? (0.26820699999999997 - (0.26648999999999995 * fSlow13)) : (0.134962 - (0.126273 * fSlow16)));
		double fSlow214 = (iSlow12 ? (iSlow15 ? (0.083415000000000003 - (0.01399800000000001 * fSlow13)) : (0.076415999999999998 - (0.0073420000000000013 * fSlow16))) : fSlow213);
		double fSlow215 = (fSlow214 + (fSlow19 * ((iSlow12 ? fSlow213 : (iSlow15 ? (0.093521000000000007 - (0.12949000000000002 * fSlow13)) : ((0.011072000000000002 * fSlow16) + 0.028775999999999999))) - fSlow214)));
		double fSlow216 = (iSlow15 ? (0.039267999999999997 - (0.027883999999999992 * fSlow13)) : (0.025326000000000001 - (0.0067400000000000029 * fSlow16)));
		double fSlow217 = (iSlow12 ? (iSlow15 ? (0.023709999999999998 - (0.0059259999999999938 * fSlow13)) : ((0.0058589999999999996 * fSlow16) + 0.020747000000000002)) : fSlow216);
		double fSlow218 = std::pow(0.001, (fConst5 / (fSlow217 + (fSlow19 * ((iSlow12 ? fSlow216 : (iSlow15 ? ((0.027747999999999995 * fSlow13) + 0.036970000000000003) : (0.050844 - (0.024140999999999999 * fSlow16)))) - fSlow217)))));
		double fSlow219 = (0.0 - (2.0 * fSlow218));
		double fSlow220 = (iSlow15 ? ((3016.7194139999992 * fSlow13) + 3458.03838) : (4966.3980869999996 - (159.98069399999986 * fSlow16)));
		double fSlow221 = (iSlow12 ? (iSlow15 ? ((747.62508799999978 * fSlow13) + 3332.6952270000002) : ((923.18030399999998 * fSlow16) + 3706.507771)) : fSlow220);
		double fSlow222 = std::cos((fConst6 * (fSlow221 + (fSlow19 * ((iSlow12 ? fSlow220 : (iSlow15 ? (3631.7053500000002 - (506.0761080000002 * fSlow13)) : ((1387.4842979999999 * fSlow16) + 3378.6672960000001))) - fSlow221)))));
		double fSlow223 = mydsp_faustpower2_f(fSlow218);
		double fSlow224 = (iSlow15 ? ((0.082158000000000009 * fSlow13) + 0.079651) : (0.12073 - (0.089085999999999999 * fSlow16)));
		double fSlow225 = (iSlow12 ? (iSlow15 ? ((0.078974000000000003 * fSlow13) + 0.057484) : (0.096971000000000002 - (0.067498000000000002 * fSlow16))) : fSlow224);
		double fSlow226 = (fSlow225 + (fSlow19 * ((iSlow12 ? fSlow224 : (iSlow15 ? (0.029219999999999999 - (0.0024920000000000012 * fSlow13)) : ((0.013929000000000004 * fSlow16) + 0.027973999999999999))) - fSlow225)));
		double fSlow227 = (iSlow15 ? (0.031777 - (0.012524 * fSlow13)) : (0.025514999999999999 - (0.0068639999999999986 * fSlow16)));
		double fSlow228 = (iSlow12 ? (iSlow15 ? (0.023429999999999999 - (0.0017299999999999954 * fSlow13)) : ((0.0077239999999999982 * fSlow16) + 0.022565000000000002)) : fSlow227);
		double fSlow229 = std::pow(0.001, (fConst5 / (fSlow228 + (fSlow19 * ((iSlow12 ? fSlow227 : (iSlow15 ? (0.069147 - (0.079464000000000007 * fSlow13)) : (0.029415 - (0.011375 * fSlow16)))) - fSlow228)))));
		double fSlow230 = (0.0 - (2.0 * fSlow229));
		double fSlow231 = (iSlow15 ? ((3131.8987999999999 * fSlow13) + 3590.8500479999998) : (5156.7994479999998 - (132.25397299999986 * fSlow16)));
		double fSlow232 = (iSlow12 ? (iSlow15 ? ((1035.3434040000002 * fSlow13) + 3416.6361189999998) : ((884.70438299999978 * fSlow16) + 3934.3078209999999)) : fSlow231);
		double fSlow233 = std::cos((fConst6 * (fSlow232 + (fSlow19 * ((iSlow12 ? fSlow231 : (iSlow15 ? (3806.882908 - (598.29237199999989 * fSlow13)) : ((1346.6182709999998 * fSlow16) + 3507.7367220000001))) - fSlow232)))));
		double fSlow234 = mydsp_faustpower2_f(fSlow229);
		double fSlow235 = (iSlow15 ? ((0.005514000000000005 * fSlow13) + 0.034127999999999999) : ((0.0087230000000000016 * fSlow16) + 0.036885000000000001));
		double fSlow236 = (iSlow12 ? (iSlow15 ? ((0.038820000000000007 * fSlow13) + 0.059258999999999999) : (0.078669000000000003 - (0.045615000000000003 * fSlow16))) : fSlow235);
		double fSlow237 = (fSlow236 + (fSlow19 * ((iSlow12 ? fSlow235 : (iSlow15 ? ((0.025107999999999998 * fSlow13) + 0.027646) : ((0.0034710000000000019 * fSlow16) + 0.0402))) - fSlow236)));
		double fSlow238 = (iSlow15 ? (0.037990000000000003 - (0.030952000000000007 * fSlow13)) : ((0.0028660000000000005 * fSlow16) + 0.022513999999999999));
		double fSlow239 = (iSlow12 ? (iSlow15 ? ((0.031145999999999997 * fSlow13) + 0.015516) : ((0.0096830000000000041 * fSlow16) + 0.031088999999999999)) : fSlow238);
		double fSlow240 = std::pow(0.001, (fConst5 / (fSlow239 + (fSlow19 * ((iSlow12 ? fSlow238 : (iSlow15 ? (0.036096000000000003 - (0.028260000000000007 * fSlow13)) : (0.021965999999999999 - (0.0061510000000000002 * fSlow16)))) - fSlow239)))));
		double fSlow241 = (0.0 - (2.0 * fSlow240));
		double fSlow242 = (iSlow15 ? ((3159.9610379999995 * fSlow13) + 3783.1436020000001) : ((241.56063100000028 * fSlow16) + 5363.1241209999998));
		double fSlow243 = (iSlow12 ? (iSlow15 ? ((1809.0639200000005 * fSlow13) + 3513.8764160000001) : ((1075.2197900000001 * fSlow16) + 4418.4083760000003)) : fSlow242);
		double fSlow244 = std::cos((fConst6 * (fSlow243 + (fSlow19 * ((iSlow12 ? fSlow242 : (iSlow15 ? (3910.0784039999999 - (444.34212200000002 * fSlow13)) : ((1282.6087939999998 * fSlow16) + 3687.9073429999999))) - fSlow243)))));
		double fSlow245 = mydsp_faustpower2_f(fSlow240);
		double fSlow246 = (iSlow15 ? (0.29683799999999999 - (0.54563600000000001 * fSlow13)) : (0.02402 - (0.0042470000000000008 * fSlow16)));
		double fSlow247 = (iSlow12 ? (iSlow15 ? ((0.061822000000000016 * fSlow13) + 0.034661999999999998) : (0.065573000000000006 - (0.010262000000000007 * fSlow16))) : fSlow246);
		double fSlow248 = (fSlow247 + (fSlow19 * ((iSlow12 ? fSlow246 : (iSlow15 ? ((0.021401999999999997 * fSlow13) + 0.020979000000000001) : ((0.023337999999999998 * fSlow16) + 0.03168))) - fSlow247)));
		double fSlow249 = (iSlow15 ? (0.072567000000000006 - (0.058850000000000013 * fSlow13)) : (0.043142 - (0.021891999999999998 * fSlow16)));
		double fSlow250 = (iSlow12 ? (iSlow15 ? ((0.0080299999999999955 * fSlow13) + 0.022773000000000002) : (0.026787999999999999 - (0.0029320000000000006 * fSlow16))) : fSlow249);
		double fSlow251 = std::pow(0.001, (fConst5 / (fSlow250 + (fSlow19 * ((iSlow12 ? fSlow249 : (iSlow15 ? (0.049135999999999999 - (0.051305999999999997 * fSlow13)) : (0.023483 - (0.0068120000000000021 * fSlow16)))) - fSlow250)))));
		double fSlow252 = (0.0 - (2.0 * fSlow251));
		double fSlow253 = (iSlow15 ? ((2976.072064 * fSlow13) + 4123.0023279999996) : ((60.074155000000246 * fSlow16) + 5611.0383599999996));
		double fSlow254 = (iSlow12 ? (iSlow15 ? ((2489.1550479999996 * fSlow13) + 3994.492272) : ((1084.3139730000003 * fSlow16) + 5239.0697959999998)) : fSlow253);
		double fSlow255 = std::cos((fConst6 * (fSlow254 + (fSlow19 * ((iSlow12 ? fSlow253 : (iSlow15 ? (4173.7791280000001 - (691.74827000000005 * fSlow13)) : ((1236.2983320000003 * fSlow16) + 3827.9049930000001))) - fSlow254)))));
		double fSlow256 = mydsp_faustpower2_f(fSlow251);
		double fSlow257 = (iSlow15 ? ((0.101338 * fSlow13) + 0.051958999999999998) : (0.102628 - (0.08408199999999999 * fSlow16)));
		double fSlow258 = (iSlow12 ? (iSlow15 ? (0.064478999999999995 - (0.01411599999999999 * fSlow13)) : ((0.016272999999999996 * fSlow16) + 0.057421)) : fSlow257);
		double fSlow259 = (fSlow258 + (fSlow19 * ((iSlow12 ? fSlow257 : (iSlow15 ? (0.031886999999999999 - (0.020385999999999994 * fSlow13)) : ((0.020036000000000002 * fSlow16) + 0.021694000000000001))) - fSlow258)));
		double fSlow260 = (iSlow15 ? (0.045823999999999997 - (0.054321999999999995 * fSlow13)) : ((0.00055400000000000241 * fSlow16) + 0.018662999999999999));
		double fSlow261 = (iSlow12 ? (iSlow15 ? (0.025264999999999999 - (0.022873999999999999 * fSlow13)) : ((0.0070610000000000013 * fSlow16) + 0.013828)) : fSlow260);
		double fSlow262 = std::pow(0.001, (fConst5 / (fSlow261 + (fSlow19 * ((iSlow12 ? fSlow260 : (iSlow15 ? (0.046267999999999997 - (0.046803999999999991 * fSlow13)) : ((0.0058659999999999997 * fSlow16) + 0.022866000000000001))) - fSlow261)))));
		double fSlow263 = (0.0 - (2.0 * fSlow262));
		double fSlow264 = (iSlow15 ? ((3490.6790920000003 * fSlow13) + 4250.310528) : ((537.49701300000015 * fSlow16) + 5995.6500740000001));
		double fSlow265 = (iSlow12 ? (iSlow15 ? ((2599.5130399999998 * fSlow13) + 4128.261485) : ((1086.4952430000003 * fSlow16) + 5428.0180049999999)) : fSlow264);
		double fSlow266 = std::cos((fConst6 * (fSlow265 + (fSlow19 * ((iSlow12 ? fSlow264 : (iSlow15 ? (4292.9015429999999 - (506.08702799999992 * fSlow13)) : ((1160.5418870000003 * fSlow16) + 4039.858029))) - fSlow265)))));
		double fSlow267 = mydsp_faustpower2_f(fSlow262);
		double fSlow268 = (iSlow15 ? (0.124454 - (0.078397999999999995 * fSlow13)) : (0.085254999999999997 - (0.07175999999999999 * fSlow16)));
		double fSlow269 = (iSlow12 ? (iSlow15 ? ((0.047851999999999992 * fSlow13) + 0.061452) : (0.085377999999999996 - (0.014036999999999994 * fSlow16))) : fSlow268);
		double fSlow270 = (fSlow269 + (fSlow19 * ((iSlow12 ? fSlow268 : (iSlow15 ? (0.052609999999999997 - (0.034495999999999999 * fSlow13)) : ((0.027470000000000001 * fSlow16) + 0.035361999999999998))) - fSlow269)));
		double fSlow271 = (iSlow15 ? (0.026516000000000001 - (0.022918000000000004 * fSlow13)) : ((0.001269000000000001 * fSlow16) + 0.015056999999999999));
		double fSlow272 = (iSlow12 ? (iSlow15 ? (0.028871999999999998 - (0.0099099999999999952 * fSlow13)) : ((0.0089419999999999986 * fSlow16) + 0.023917000000000001)) : fSlow271);
		double fSlow273 = std::pow(0.001, (fConst5 / (fSlow272 + (fSlow19 * ((iSlow12 ? fSlow271 : (iSlow15 ? (0.029451999999999999 - (0.012067999999999995 * fSlow13)) : (0.023418000000000001 - (0.0054890000000000008 * fSlow16)))) - fSlow272)))));
		double fSlow274 = (0.0 - (2.0 * fSlow273));
		double fSlow275 = (iSlow15 ? ((3508.3634139999995 * fSlow13) + 4408.4425179999998) : ((589.32559100000071 * fSlow16) + 6162.6242249999996));
		double fSlow276 = (iSlow12 ? (iSlow15 ? ((3047.8105799999994 * fSlow13) + 4247.828681) : ((938.24566500000037 * fSlow16) + 5771.7339709999997)) : fSlow275);
		double fSlow277 = std::cos((fConst6 * (fSlow276 + (fSlow19 * ((iSlow12 ? fSlow275 : (iSlow15 ? (4519.2884400000003 - (621.09203600000001 * fSlow13)) : ((1138.9084089999997 * fSlow16) + 4208.7424220000003))) - fSlow276)))));
		double fSlow278 = mydsp_faustpower2_f(fSlow273);
		double fSlow279 = (iSlow15 ? (0.077586000000000002 - (0.036294000000000007 * fSlow13)) : (0.059438999999999999 - (0.040957 * fSlow16)));
		double fSlow280 = (iSlow12 ? (iSlow15 ? (0.110522 - (0.050249999999999989 * fSlow13)) : (0.085397000000000001 - (0.017031000000000004 * fSlow16))) : fSlow279);
		double fSlow281 = (fSlow280 + (fSlow19 * ((iSlow12 ? fSlow279 : (iSlow15 ? (0.044999999999999998 - (0.026357999999999993 * fSlow13)) : (0.031821000000000002 - (0.0013570000000000006 * fSlow16)))) - fSlow280)));
		double fSlow282 = (iSlow15 ? ((0.050320000000000004 * fSlow13) + 0.027684) : (0.052844000000000002 - (0.033375000000000002 * fSlow16)));
		double fSlow283 = (iSlow12 ? (iSlow15 ? ((0.0054840000000000028 * fSlow13) + 0.019743) : ((0.005058 * fSlow16) + 0.022485000000000002)) : fSlow282);
		double fSlow284 = std::pow(0.001, (fConst5 / (fSlow283 + (fSlow19 * ((iSlow12 ? fSlow282 : (iSlow15 ? ((0.017011999999999992 * fSlow13) + 0.025066000000000001) : (0.033571999999999998 - (0.012051999999999997 * fSlow16)))) - fSlow283)))));
		double fSlow285 = (0.0 - (2.0 * fSlow284));
		double fSlow286 = (iSlow15 ? ((4104.1139180000009 * fSlow13) + 4531.9160549999997) : ((278.80897600000026 * fSlow16) + 6583.9730140000001));
		double fSlow287 = (iSlow12 ? (iSlow15 ? ((3362.4950599999993 * fSlow13) + 4425.1310190000004) : ((716.7260409999999 * fSlow16) + 6106.378549)) : fSlow286);
		double fSlow288 = std::cos((fConst6 * (fSlow287 + (fSlow19 * ((iSlow12 ? fSlow286 : (iSlow15 ? (4631.053559 - (490.80351200000041 * fSlow13)) : ((1074.1556920000003 * fSlow16) + 4385.6518029999997))) - fSlow287)))));
		double fSlow289 = mydsp_faustpower2_f(fSlow284);
		double fSlow290 = (iSlow15 ? (0.084915000000000004 - (0.12315200000000001 * fSlow13)) : (0.023338999999999999 - (0.003209 * fSlow16)));
		double fSlow291 = (iSlow12 ? (iSlow15 ? ((0.0096159999999999995 * fSlow13) + 0.045769999999999998) : (0.050577999999999998 - (0.023843 * fSlow16))) : fSlow290);
		double fSlow292 = (fSlow291 + (fSlow19 * ((iSlow12 ? fSlow290 : (iSlow15 ? ((0.0083680000000000004 * fSlow13) + 0.023542) : ((0.00032300000000000037 * fSlow16) + 0.027726000000000001))) - fSlow291)));
		double fSlow293 = (iSlow15 ? (0.034229000000000002 - (0.035502000000000006 * fSlow13)) : ((0.004276000000000002 * fSlow16) + 0.016478));
		double fSlow294 = (iSlow12 ? (iSlow15 ? (0.036565 - (0.0024919999999999942 * fSlow13)) : (0.035319000000000003 - (0.0088920000000000041 * fSlow16))) : fSlow293);
		double fSlow295 = std::pow(0.001, (fConst5 / (fSlow294 + (fSlow19 * ((iSlow12 ? fSlow293 : (iSlow15 ? ((0.011644000000000002 * fSlow13) + 0.020941999999999999) : (0.026764 - (0.0021419999999999981 * fSlow16)))) - fSlow294)))));
		double fSlow296 = (0.0 - (2.0 * fSlow295));
		double fSlow297 = (iSlow15 ? ((4328.6280779999997 * fSlow13) + 4865.2116800000003) : (7029.5257190000002 - (78.299439999999777 * fSlow16)));
		double fSlow298 = (iSlow12 ? (iSlow15 ? ((3235.2378960000005 * fSlow13) + 4686.893838) : ((705.23561199999949 * fSlow16) + 6304.5127860000002)) : fSlow297);
		double fSlow299 = std::cos((fConst6 * (fSlow298 + (fSlow19 * ((iSlow12 ? fSlow297 : (iSlow15 ? (4855.4051419999996 - (561.17338799999925 * fSlow13)) : ((1055.3258050000004 * fSlow16) + 4574.818448))) - fSlow298)))));
		double fSlow300 = mydsp_faustpower2_f(fSlow295);
		double fSlow301 = (iSlow15 ? (0.12620200000000001 - (0.19751800000000003 * fSlow13)) : (0.027442999999999999 - (0.012025999999999998 * fSlow16)));
		double fSlow302 = (iSlow12 ? (iSlow15 ? (0.053317999999999997 - (0.013567999999999997 * fSlow13)) : (0.046533999999999999 - (0.0091749999999999957 * fSlow16))) : fSlow301);
		double fSlow303 = (fSlow302 + (fSlow19 * ((iSlow12 ? fSlow301 : (iSlow15 ? (0.049425999999999998 - (0.024413999999999991 * fSlow13)) : (0.037219000000000002 - (0.024339 * fSlow16)))) - fSlow302)));
		double fSlow304 = (iSlow15 ? (0.040867000000000001 - (0.029748000000000004 * fSlow13)) : (0.025992999999999999 - (0.0065059999999999979 * fSlow16)));
		double fSlow305 = (iSlow12 ? (iSlow15 ? (0.032703999999999997 - (0.029497999999999996 * fSlow13)) : ((0.014199000000000003 * fSlow16) + 0.017954999999999999)) : fSlow304);
		double fSlow306 = std::pow(0.001, (fConst5 / (fSlow305 + (fSlow19 * ((iSlow12 ? fSlow304 : (iSlow15 ? (0.040527000000000001 - (0.046386000000000004 * fSlow13)) : ((0.0053720000000000018 * fSlow16) + 0.017333999999999999))) - fSlow305)))));
		double fSlow307 = (0.0 - (2.0 * fSlow306));
		double fSlow308 = (iSlow15 ? ((3918.4665299999997 * fSlow13) + 5248.6607260000001) : (7207.8939909999999 - (27.030878999999914 * fSlow16)));
		double fSlow309 = (iSlow12 ? (iSlow15 ? ((4114.186318 * fSlow13) + 4731.1417540000002) : ((410.68194499999936 * fSlow16) + 6788.2349130000002)) : fSlow308);
		double fSlow310 = std::cos((fConst6 * (fSlow309 + (fSlow19 * ((iSlow12 ? fSlow308 : (iSlow15 ? (5079.8281390000002 - (671.38340600000083 * fSlow13)) : ((1126.1215980000006 * fSlow16) + 4744.1364359999998))) - fSlow309)))));
		double fSlow311 = mydsp_faustpower2_f(fSlow306);
		double fSlow312 = (iSlow15 ? ((0.243842 * fSlow13) + 0.019696999999999999) : (0.14161799999999999 - (0.110509 * fSlow16)));
		double fSlow313 = (iSlow12 ? (iSlow15 ? ((0.32451000000000002 * fSlow13) + 0.051225) : (0.21348 - (0.187472 * fSlow16))) : fSlow312);
		double fSlow314 = (fSlow313 + (fSlow19 * ((iSlow12 ? fSlow312 : (iSlow15 ? (0.029062999999999999 - (0.0055099999999999941 * fSlow13)) : ((0.020846 * fSlow16) + 0.026308000000000002))) - fSlow313)));
		double fSlow315 = (iSlow15 ? (0.0378 - (0.03984 * fSlow13)) : ((0.0017099999999999997 * fSlow16) + 0.01788));
		double fSlow316 = (iSlow12 ? (iSlow15 ? (0.025233999999999999 - (0.020805999999999998 * fSlow13)) : ((0.0057450000000000001 * fSlow16) + 0.014831)) : fSlow315);
		double fSlow317 = std::pow(0.001, (fConst5 / (fSlow316 + (fSlow19 * ((iSlow12 ? fSlow315 : (iSlow15 ? (0.027373999999999999 - (0.011959999999999998 * fSlow13)) : ((0.001743999999999999 * fSlow16) + 0.021394))) - fSlow316)))));
		double fSlow318 = (0.0 - (2.0 * fSlow317));
		double fSlow319 = (iSlow15 ? ((4639.5268539999997 * fSlow13) + 5387.0115919999998) : ((194.85629200000039 * fSlow16) + 7706.7750189999997));
		double fSlow320 = (iSlow12 ? (iSlow15 ? ((2702.1926439999988 * fSlow13) + 5553.9049720000003) : ((2114.2470869999997 * fSlow16) + 6905.0012939999997)) : fSlow319);
		double fSlow321 = std::cos((fConst6 * (fSlow320 + (fSlow19 * ((iSlow12 ? fSlow319 : (iSlow15 ? (5182.392108 - (630.99645199999941 * fSlow13)) : ((1255.067524 * fSlow16) + 4866.8938820000003))) - fSlow320)))));
		double fSlow322 = mydsp_faustpower2_f(fSlow317);
		double fSlow323 = (iSlow15 ? ((0.052820000000000006 * fSlow13) + 0.017642999999999999) : (0.044053000000000002 - (0.036429000000000003 * fSlow16)));
		double fSlow324 = (iSlow12 ? (iSlow15 ? ((0.110288 * fSlow13) + 0.034998000000000001) : (0.090142 - (0.064184000000000005 * fSlow16))) : fSlow323);
		double fSlow325 = (fSlow324 + (fSlow19 * ((iSlow12 ? fSlow323 : (iSlow15 ? (0.049062000000000001 - (0.065795999999999993 * fSlow13)) : ((0.068543000000000007 * fSlow16) + 0.016164000000000001))) - fSlow324)));
		double fSlow326 = (iSlow15 ? (0.026894000000000001 - (0.025776000000000004 * fSlow13)) : (0.014005999999999999 - (0.0026579999999999989 * fSlow16)));
		double fSlow327 = (iSlow12 ? (iSlow15 ? (0.028382999999999999 - (0.018962 * fSlow13)) : ((0.014088 * fSlow16) + 0.018901999999999999)) : fSlow326);
		double fSlow328 = std::pow(0.001, (fConst5 / (fSlow327 + (fSlow19 * ((iSlow12 ? fSlow326 : (iSlow15 ? (0.030986 - (0.010003999999999999 * fSlow13)) : ((0.0013389999999999999 * fSlow16) + 0.025984))) - fSlow327)))));
		double fSlow329 = (0.0 - (2.0 * fSlow328));
		double fSlow330 = (iSlow15 ? ((4126.8764679999986 * fSlow13) + 5780.5937860000004) : ((230.6301540000004 * fSlow16) + 7844.0320199999996));
		double fSlow331 = (iSlow12 ? (iSlow15 ? ((2704.9761199999994 * fSlow13) + 5702.2063889999999) : ((2437.5582039999999 * fSlow16) + 7054.6944489999996)) : fSlow330);
		double fSlow332 = std::cos((fConst6 * (fSlow331 + (fSlow19 * ((iSlow12 ? fSlow330 : (iSlow15 ? (5328.2918179999997 - (475.51847799999996 * fSlow13)) : ((1153.9897620000002 * fSlow16) + 5090.5325789999997))) - fSlow331)))));
		double fSlow333 = mydsp_faustpower2_f(fSlow328);
		double fSlow334 = (iSlow15 ? (0.077300999999999995 - (0.10606399999999999 * fSlow13)) : (0.024268999999999999 - (0.015743 * fSlow16)));
		double fSlow335 = (iSlow12 ? (iSlow15 ? ((0.054187999999999986 * fSlow13) + 0.051538) : (0.078631999999999994 - (0.053304999999999991 * fSlow16))) : fSlow334);
		double fSlow336 = (fSlow335 + (fSlow19 * ((iSlow12 ? fSlow334 : (iSlow15 ? ((0.015682000000000001 * fSlow13) + 0.015096999999999999) : ((0.027594 * fSlow16) + 0.022938))) - fSlow335)));
		double fSlow337 = (iSlow15 ? (0.023151000000000001 - (0.00036000000000000615 * fSlow13)) : (0.022970999999999998 - (0.0070019999999999978 * fSlow16)));
		double fSlow338 = (iSlow12 ? (iSlow15 ? (0.018356000000000001 - (0.0073900000000000007 * fSlow13)) : ((0.0046829999999999997 * fSlow16) + 0.014661)) : fSlow337);
		double fSlow339 = std::pow(0.001, (fConst5 / (fSlow338 + (fSlow19 * ((iSlow12 ? fSlow337 : (iSlow15 ? ((0.012541999999999998 * fSlow13) + 0.019875) : (0.026145999999999999 - (0.0055899999999999977 * fSlow16)))) - fSlow338)))));
		double fSlow340 = (0.0 - (2.0 * fSlow339));
		double fSlow341 = (iSlow15 ? ((8835.7005659999995 * fSlow13) + 5880.9546730000002) : (10298.804956 - (1319.4090419999993 * fSlow16)));
		double fSlow342 = (iSlow12 ? (iSlow15 ? ((1625.8326900000011 * fSlow13) + 6415.0717519999998) : ((2478.4631519999994 * fSlow16) + 7227.9880970000004)) : fSlow341);
		double fSlow343 = std::cos((fConst6 * (fSlow342 + (fSlow19 * ((iSlow12 ? fSlow341 : (iSlow15 ? (5636.8179790000004 - (873.06973600000128 * fSlow13)) : ((1476.7694580000007 * fSlow16) + 5200.2831109999997))) - fSlow342)))));
		double fSlow344 = mydsp_faustpower2_f(fSlow339);
		double fSlow345 = (iSlow15 ? (0.068366999999999997 - (0.043369999999999992 * fSlow13)) : (0.046682000000000001 - (0.014143000000000003 * fSlow16)));
		double fSlow346 = (iSlow12 ? (iSlow15 ? ((0.017157999999999979 * fSlow13) + 0.081127000000000005) : (0.089705999999999994 - (0.036846999999999991 * fSlow16))) : fSlow345);
		double fSlow347 = (fSlow346 + (fSlow19 * ((iSlow12 ? fSlow345 : (iSlow15 ? ((0.031488000000000002 * fSlow13) + 0.030925000000000001) : (0.046669000000000002 - (0.013934000000000002 * fSlow16)))) - fSlow346)));
		double fSlow348 = (iSlow15 ? (0.023380999999999999 - (0.011321999999999999 * fSlow13)) : (0.01772 - (0.0020459999999999992 * fSlow16)));
		double fSlow349 = (iSlow12 ? (iSlow15 ? ((0.028749999999999998 * fSlow13) + 0.018609000000000001) : (0.032983999999999999 - (0.013156999999999999 * fSlow16))) : fSlow348);
		double fSlow350 = std::pow(0.001, (fConst5 / (fSlow349 + (fSlow19 * ((iSlow12 ? fSlow348 : (iSlow15 ? (0.028632000000000001 - (0.019662000000000006 * fSlow13)) : ((0.0057760000000000034 * fSlow16) + 0.018800999999999998))) - fSlow349)))));
		double fSlow351 = (0.0 - (2.0 * fSlow350));
		double fSlow352 = (iSlow15 ? ((8938.4807019999989 * fSlow13) + 6095.2914989999999) : (10564.531849999999 - (489.2824759999985 * fSlow16)));
		double fSlow353 = (iSlow12 ? (iSlow15 ? ((1591.8431760000003 * fSlow13) + 6552.4228999999996) : ((2510.5449450000006 * fSlow16) + 7348.3444879999997)) : fSlow352);
		double fSlow354 = std::cos((fConst6 * (fSlow353 + (fSlow19 * ((iSlow12 ? fSlow352 : (iSlow15 ? (5983.6291289999999 - (1070.5795240000007 * fSlow13)) : ((1784.7613440000005 * fSlow16) + 5448.3393669999996))) - fSlow353)))));
		double fSlow355 = mydsp_faustpower2_f(fSlow350);
		double fSlow356 = (iSlow15 ? ((0.021402000000000004 * fSlow13) + 0.023764) : (0.034465000000000003 - (0.024619000000000002 * fSlow16)));
		double fSlow357 = (iSlow12 ? (iSlow15 ? ((0.47375400000000001 * fSlow13) + 0.039421999999999999) : (0.27629900000000002 - (0.193388 * fSlow16))) : fSlow356);
		double fSlow358 = (fSlow357 + (fSlow19 * ((iSlow12 ? fSlow356 : (iSlow15 ? (0.032215000000000001 - (0.0021740000000000023 * fSlow13)) : ((0.00081000000000000169 * fSlow16) + 0.031127999999999999))) - fSlow357)));
		double fSlow359 = (iSlow15 ? ((0.021791999999999999 * fSlow13) + 0.021277000000000001) : (0.032173 - (0.015868 * fSlow16)));
		double fSlow360 = (iSlow12 ? (iSlow15 ? ((0.020389999999999998 * fSlow13) + 0.017534000000000001) : (0.027729 - (0.0091579999999999995 * fSlow16))) : fSlow359);
		double fSlow361 = std::pow(0.001, (fConst5 / (fSlow360 + (fSlow19 * ((iSlow12 ? fSlow359 : (iSlow15 ? (0.032343999999999998 - (0.028203999999999993 * fSlow13)) : ((0.0054269999999999978 * fSlow16) + 0.018242000000000001))) - fSlow360)))));
		double fSlow362 = (0.0 - (2.0 * fSlow361));
		double fSlow363 = (iSlow15 ? ((10366.166120000002 * fSlow13) + 6432.6053869999996) : (11615.688447 - (1094.3374839999997 * fSlow16)));
		double fSlow364 = (iSlow12 ? (iSlow15 ? ((3329.975252000002 * fSlow13) + 6785.4614229999997) : ((1629.9732289999993 * fSlow16) + 8450.4490490000007)) : fSlow363);
		double fSlow365 = std::cos((fConst6 * (fSlow364 + (fSlow19 * ((iSlow12 ? fSlow363 : (iSlow15 ? (6104.0137999999997 - (704.58782799999972 * fSlow13)) : ((1768.3113650000005 * fSlow16) + 5751.7198859999999))) - fSlow364)))));
		double fSlow366 = mydsp_faustpower2_f(fSlow361);
		double fSlow367 = (iSlow15 ? ((0.10601200000000001 * fSlow13) + 0.015924000000000001) : (0.068930000000000005 - (0.061325000000000005 * fSlow16)));
		double fSlow368 = (iSlow12 ? (iSlow15 ? (0.22601099999999999 - (0.32965800000000001 * fSlow13)) : (0.061182 - (0.023025999999999998 * fSlow16))) : fSlow367);
		double fSlow369 = (fSlow368 + (fSlow19 * ((iSlow12 ? fSlow367 : (iSlow15 ? (0.032146000000000001 - (0.0094320000000000029 * fSlow13)) : (0.02743 - (0.003876000000000001 * fSlow16)))) - fSlow368)));
		double fSlow370 = (iSlow15 ? (0.023057000000000001 - (0.015332000000000002 * fSlow13)) : ((0.011381 * fSlow16) + 0.015391));
		double fSlow371 = (iSlow12 ? (iSlow15 ? ((0.012735999999999997 * fSlow13) + 0.018699) : (0.025066999999999999 - (0.0078539999999999999 * fSlow16))) : fSlow370);
		double fSlow372 = std::pow(0.001, (fConst5 / (fSlow371 + (fSlow19 * ((iSlow12 ? fSlow370 : (iSlow15 ? (0.033709000000000003 - (0.022538000000000002 * fSlow13)) : ((0.0022739999999999982 * fSlow16) + 0.022440000000000002))) - fSlow371)))));
		double fSlow373 = (0.0 - (2.0 * fSlow372));
		double fSlow374 = (iSlow15 ? ((10919.529722000001 * fSlow13) + 6596.4035009999998) : (12056.168362 - (91.308441000001039 * fSlow16)));
		double fSlow375 = (iSlow12 ? (iSlow15 ? ((4020.4240120000013 * fSlow13) + 6990.1044709999996) : ((1251.6843339999996 * fSlow16) + 9000.3164770000003)) : fSlow374);
		double fSlow376 = std::cos((fConst6 * (fSlow375 + (fSlow19 * ((iSlow12 ? fSlow374 : (iSlow15 ? (6184.5479420000001 - (288.97653200000059 * fSlow13)) : ((1637.8430600000002 * fSlow16) + 6040.0596759999999))) - fSlow375)))));
		double fSlow377 = mydsp_faustpower2_f(fSlow372);
		double fSlow378 = (iSlow15 ? ((0.096677999999999986 * fSlow13) + 0.022162999999999999) : (0.070501999999999995 - (0.050635999999999994 * fSlow16)));
		double fSlow379 = (iSlow12 ? (iSlow15 ? (0.053012999999999998 - (0.028143999999999988 * fSlow13)) : ((0.0071379999999999985 * fSlow16) + 0.038941000000000003)) : fSlow378);
		double fSlow380 = (fSlow379 + (fSlow19 * ((iSlow12 ? fSlow378 : (iSlow15 ? (0.050861999999999997 - (0.03691599999999999 * fSlow13)) : (0.032404000000000002 - (0.014870000000000001 * fSlow16)))) - fSlow379)));
		double fSlow381 = (iSlow15 ? (0.020206999999999999 - (0.02198 * fSlow13)) : ((0.001380000000000001 * fSlow16) + 0.0092169999999999995));
		double fSlow382 = (iSlow12 ? (iSlow15 ? ((0.010897999999999998 * fSlow13) + 0.015762000000000002) : (0.021211000000000001 - (0.0022089999999999992 * fSlow16))) : fSlow381);
		double fSlow383 = std::pow(0.001, (fConst5 / (fSlow382 + (fSlow19 * ((iSlow12 ? fSlow381 : (iSlow15 ? (0.024937999999999998 - (0.0079979999999999982 * fSlow13)) : ((0.0034810000000000015 * fSlow16) + 0.020938999999999999))) - fSlow382)))));
		double fSlow384 = (0.0 - (2.0 * fSlow383));
		double fSlow385 = (iSlow15 ? ((11680.57494 * fSlow13) + 6820.9176870000001) : ((1566.0081649999993 * fSlow16) + 12661.205157));
		double fSlow386 = (iSlow12 ? (iSlow15 ? ((7348.4221319999997 * fSlow13) + 7256.1165149999997) : (10930.327581 - (540.46263499999986 * fSlow16))) : fSlow385);
		double fSlow387 = std::cos((fConst6 * (fSlow386 + (fSlow19 * ((iSlow12 ? fSlow385 : (iSlow15 ? (6602.6148400000002 - (203.89929599999959 * fSlow13)) : ((1304.4735249999994 * fSlow16) + 6500.6651920000004))) - fSlow386)))));
		double fSlow388 = mydsp_faustpower2_f(fSlow383);
		double fSlow389 = (iSlow15 ? ((0.015916 * fSlow13) + 0.017954000000000001) : (0.025912000000000001 - (0.018329000000000002 * fSlow16)));
		double fSlow390 = (iSlow12 ? (iSlow15 ? (0.05262 - (0.014345999999999998 * fSlow13)) : (0.045447000000000001 - (0.010607999999999999 * fSlow16))) : fSlow389);
		double fSlow391 = (fSlow390 + (fSlow19 * ((iSlow12 ? fSlow389 : (iSlow15 ? ((0.0029899999999999996 * fSlow13) + 0.016861000000000001) : (0.018356000000000001 - (0.0010880000000000022 * fSlow16)))) - fSlow390)));
		double fSlow392 = (iSlow15 ? ((0.018528000000000003 * fSlow13) + 0.019671999999999999) : (0.028936 - (0.019009999999999999 * fSlow16)));
		double fSlow393 = (iSlow12 ? (iSlow15 ? (0.022800000000000001 - (0.011779999999999999 * fSlow13)) : ((0.0035299999999999984 * fSlow16) + 0.016910000000000001)) : fSlow392);
		double fSlow394 = std::pow(0.001, (fConst5 / (fSlow393 + (fSlow19 * ((iSlow12 ? fSlow392 : (iSlow15 ? (0.063833000000000001 - (0.103792 * fSlow13)) : ((0.0057959999999999991 * fSlow16) + 0.011937))) - fSlow393)))));
		double fSlow395 = (0.0 - (2.0 * fSlow394));
		double fSlow396 = (iSlow15 ? ((11938.701158000002 * fSlow13) + 6907.0551079999996) : ((2625.0495630000005 * fSlow16) + 12876.405687));
		double fSlow397 = (iSlow12 ? (iSlow15 ? ((6773.8854499999998 * fSlow13) + 7884.5085300000001) : ((1056.4410869999992 * fSlow16) + 11271.451255)) : fSlow396);
		double fSlow398 = std::cos((fConst6 * (fSlow397 + (fSlow19 * ((iSlow12 ? fSlow396 : (iSlow15 ? ((125.27146800000082 * fSlow13) + 6718.2476429999997) : ((3138.0225680000003 * fSlow16) + 6780.8833770000001))) - fSlow397)))));
		double fSlow399 = mydsp_faustpower2_f(fSlow394);
		double fSlow400 = (iSlow15 ? ((0.092342000000000007 * fSlow13) + 0.029916999999999999) : (0.076088000000000003 - (0.067097000000000004 * fSlow16)));
		double fSlow401 = (iSlow12 ? (iSlow15 ? ((0.0029819999999999985 * fSlow13) + 0.041956) : (0.043447 - (0.020708999999999998 * fSlow16))) : fSlow400);
		double fSlow402 = (fSlow401 + (fSlow19 * ((iSlow12 ? fSlow400 : (iSlow15 ? ((0.015318000000000005 * fSlow13) + 0.019227999999999999) : (0.026887000000000001 - (0.00068500000000000158 * fSlow16)))) - fSlow401)));
		double fSlow403 = (iSlow15 ? (0.016358000000000001 - (0.0089260000000000034 * fSlow13)) : (0.011894999999999999 - (0.0042999999999999991 * fSlow16)));
		double fSlow404 = (iSlow12 ? (iSlow15 ? ((0.028673999999999998 * fSlow13) + 0.013150999999999999) : (0.027487999999999999 - (0.0061589999999999978 * fSlow16))) : fSlow403);
		double fSlow405 = std::pow(0.001, (fConst5 / (fSlow404 + (fSlow19 * ((iSlow12 ? fSlow403 : (iSlow15 ? (0.032296999999999999 - (0.041116 * fSlow13)) : ((0.0061420000000000016 * fSlow16) + 0.011738999999999999))) - fSlow404)))));
		double fSlow406 = (0.0 - (2.0 * fSlow405));
		double fSlow407 = (iSlow15 ? ((12104.121034 * fSlow13) + 7030.8297110000003) : ((2504.9627280000004 * fSlow16) + 13082.890228));
		double fSlow408 = (iSlow12 ? (iSlow15 ? ((5261.6448280000004 * fSlow13) + 8787.4794669999992) : ((1047.1732119999997 * fSlow16) + 11418.301880999999)) : fSlow407);
		double fSlow409 = std::cos((fConst6 * (fSlow408 + (fSlow19 * ((iSlow12 ? fSlow407 : (iSlow15 ? ((85.776859999999942 * fSlow13) + 6835.8440440000004) : ((3540.255826999999 * fSlow16) + 6878.7324740000004))) - fSlow408)))));
		double fSlow410 = mydsp_faustpower2_f(fSlow405);
		double fSlow411 = (iSlow15 ? ((0.0070460000000000036 * fSlow13) + 0.021448999999999999) : (0.024972000000000001 - (0.015778 * fSlow16)));
		double fSlow412 = (iSlow12 ? (iSlow15 ? (0.058146999999999997 - (0.013600000000000001 * fSlow13)) : (0.051346999999999997 - (0.026866999999999999 * fSlow16))) : fSlow411);
		double fSlow413 = (fSlow412 + (fSlow19 * ((iSlow12 ? fSlow411 : (iSlow15 ? ((0.039149999999999997 * fSlow13) + 0.021427999999999999) : (0.041002999999999998 - (0.027277999999999997 * fSlow16)))) - fSlow412)));
		double fSlow414 = (iSlow15 ? (0.024757000000000001 - (0.024300000000000002 * fSlow13)) : (0.012607 - (0.0039310000000000005 * fSlow16)));
		double fSlow415 = (iSlow12 ? (iSlow15 ? ((0.0017280000000000004 * fSlow13) + 0.0123) : ((0.0098629999999999985 * fSlow16) + 0.013164)) : fSlow414);
		double fSlow416 = std::pow(0.001, (fConst5 / (fSlow415 + (fSlow19 * ((iSlow12 ? fSlow414 : (iSlow15 ? (0.028472999999999998 - (0.016317999999999999 * fSlow13)) : (0.020313999999999999 - (0.0015989999999999997 * fSlow16)))) - fSlow415)))));
		double fSlow417 = (0.0 - (2.0 * fSlow416));
		double fSlow418 = (iSlow15 ? ((13027.108352000001 * fSlow13) + 7337.8830349999998) : ((2438.7015449999999 * fSlow16) + 13851.437211));
		double fSlow419 = (iSlow12 ? (iSlow15 ? ((5837.0728780000027 * fSlow13) + 10529.849260999999) : ((42.80209399999876 * fSlow16) + 13448.385700000001)) : fSlow418);
		double fSlow420 = std::cos((fConst6 * (fSlow419 + (fSlow19 * ((iSlow12 ? fSlow418 : (iSlow15 ? ((490.41887999999926 * fSlow13) + 6882.2545140000002) : ((4168.6379999999999 * fSlow16) + 7127.4639539999998))) - fSlow419)))));
		double fSlow421 = mydsp_faustpower2_f(fSlow416);
		double fSlow422 = (iSlow15 ? ((0.038836000000000002 * fSlow13) + 0.019082999999999999) : (0.038501000000000001 - (0.028373000000000002 * fSlow16)));
		double fSlow423 = (iSlow12 ? (iSlow15 ? (0.061261999999999997 - (0.01122999999999999 * fSlow13)) : ((0.102409 * fSlow16) + 0.055647000000000002)) : fSlow422);
		double fSlow424 = (fSlow423 + (fSlow19 * ((iSlow12 ? fSlow422 : (iSlow15 ? (0.021038999999999999 - (0.016519999999999996 * fSlow13)) : ((0.0028319999999999994 * fSlow16) + 0.012779))) - fSlow423)));
		double fSlow425 = (iSlow15 ? ((0.0022100000000000036 * fSlow13) + 0.017035999999999999) : (0.018141000000000001 - (0.0098670000000000008 * fSlow16)));
		double fSlow426 = (iSlow12 ? (iSlow15 ? (0.018114000000000002 - (0.011578000000000005 * fSlow13)) : ((0.0065079999999999999 * fSlow16) + 0.012324999999999999)) : fSlow425);
		double fSlow427 = std::pow(0.001, (fConst5 / (fSlow426 + (fSlow19 * ((iSlow12 ? fSlow425 : (iSlow15 ? (0.025767999999999999 - (0.02384 * fSlow13)) : ((0.0038890000000000001 * fSlow16) + 0.013847999999999999))) - fSlow426)))));
		double fSlow428 = (0.0 - (2.0 * fSlow427));
		double fSlow429 = (iSlow15 ? ((13699.287907999998 * fSlow13) + 7602.7391520000001) : ((1965.7896400000009 * fSlow16) + 14452.383105999999));
		double fSlow430 = (iSlow12 ? (iSlow15 ? ((6593.7441699999981 * fSlow13) + 10641.861836) : ((1133.5573120000008 * fSlow16) + 13938.733920999999)) : fSlow429);
		double fSlow431 = std::cos((fConst6 * (fSlow430 + (fSlow19 * ((iSlow12 ? fSlow429 : (iSlow15 ? ((960.50127999999859 * fSlow13) + 7220.7748140000003) : ((3731.6944130000002 * fSlow16) + 7701.0254539999996))) - fSlow430)))));
		double fSlow432 = mydsp_faustpower2_f(fSlow427);
		double fSlow433 = (iSlow15 ? ((0.050118000000000003 * fSlow13) + 0.026075999999999998) : (0.051135 - (0.042577999999999998 * fSlow16)));
		double fSlow434 = (iSlow12 ? (iSlow15 ? ((0.051435999999999996 * fSlow13) + 0.046117999999999999) : (0.071835999999999997 - (0.044988 * fSlow16))) : fSlow433);
		double fSlow435 = (fSlow434 + (fSlow19 * ((iSlow12 ? fSlow433 : (iSlow15 ? ((0.023594000000000004 * fSlow13) + 0.016988) : (0.028785000000000002 - (0.0074910000000000011 * fSlow16)))) - fSlow434)));
		double fSlow436 = (iSlow15 ? ((0.003481999999999999 * fSlow13) + 0.0166) : (0.018341 - (0.0068699999999999994 * fSlow16)));
		double fSlow437 = (iSlow12 ? (iSlow15 ? ((0.013585999999999997 * fSlow13) + 0.013192000000000001) : (0.019984999999999999 - (0.0016410000000000001 * fSlow16))) : fSlow436);
		double fSlow438 = std::pow(0.001, (fConst5 / (fSlow437 + (fSlow19 * ((iSlow12 ? fSlow436 : (iSlow15 ? (0.024877 - (0.016745999999999997 * fSlow13)) : (0.016504000000000001 - (0.000844000000000001 * fSlow16)))) - fSlow437)))));
		double fSlow439 = (0.0 - (2.0 * fSlow438));
		double fSlow440 = (iSlow15 ? ((15514.147632000002 * fSlow13) + 7926.8364119999997) : ((979.35646600000109 * fSlow16) + 15683.910228000001));
		double fSlow441 = (iSlow12 ? (iSlow15 ? ((4518.7533159999984 * fSlow13) + 12075.018006) : ((842.35207900000023 * fSlow16) + 14334.394663999999)) : fSlow440);
		double fSlow442 = std::cos((fConst6 * (fSlow441 + (fSlow19 * ((iSlow12 ? fSlow440 : (iSlow15 ? ((6108.7194680000011 * fSlow13) + 7283.2639499999996) : ((3084.6146809999991 * fSlow16) + 10337.623684))) - fSlow441)))));
		double fSlow443 = mydsp_faustpower2_f(fSlow438);
		double fSlow444 = (iSlow15 ? ((0.019672000000000002 * fSlow13) + 0.017964999999999998) : (0.027800999999999999 - (0.018467999999999998 * fSlow16)));
		double fSlow445 = (iSlow12 ? (iSlow15 ? ((0.0051140000000000074 * fSlow13) + 0.038255999999999998) : (0.040813000000000002 - (0.012804000000000003 * fSlow16))) : fSlow444);
		double fSlow446 = (fSlow445 + (fSlow19 * ((iSlow12 ? fSlow444 : (iSlow15 ? (0.017971000000000001 - (0.0092500000000000013 * fSlow13)) : (0.013346 - (0.0018600000000000005 * fSlow16)))) - fSlow445)));
		double fSlow447 = (iSlow15 ? ((0.015970000000000005 * fSlow13) + 0.010194999999999999) : (0.018180000000000002 - (0.0057560000000000024 * fSlow16)));
		double fSlow448 = (iSlow12 ? (iSlow15 ? (0.085669999999999996 - (0.012483999999999995 * fSlow13)) : (0.079427999999999999 - (0.060616999999999997 * fSlow16))) : fSlow447);
		double fSlow449 = std::pow(0.001, (fConst5 / (fSlow448 + (fSlow19 * ((iSlow12 ? fSlow447 : (iSlow15 ? ((0.11889000000000001 * fSlow13) + 0.026384999999999999) : (0.085830000000000004 - (0.069366000000000011 * fSlow16)))) - fSlow448)))));
		double fSlow450 = (0.0 - (2.0 * fSlow449));
		double fSlow451 = (iSlow15 ? ((8839.2095480000025 * fSlow13) + 12248.346109) : ((1323.6641109999982 * fSlow16) + 16667.950883000001));
		double fSlow452 = (iSlow12 ? (iSlow15 ? ((16.557827999997244 * fSlow13) + 17405.170359) : ((2729.8527790000007 * fSlow16) + 17413.449272999998)) : fSlow451);
		double fSlow453 = std::cos((fConst6 * (fSlow452 + (fSlow19 * ((iSlow12 ? fSlow451 : (iSlow15 ? ((20006.695070000002 * fSlow13) + 7407.6720660000001) : (17411.019601 - (3731.7868230000004 * fSlow16)))) - fSlow452)))));
		double fSlow454 = mydsp_faustpower2_f(fSlow449);
		double fSlow455 = (iSlow15 ? ((0.010902000000000002 * fSlow13) + 0.017017999999999998) : (0.022468999999999999 - (0.014974999999999999 * fSlow16)));
		double fSlow456 = (iSlow12 ? (iSlow15 ? (0.124151 - (0.114596 * fSlow13)) : (0.066852999999999996 - (0.035465999999999998 * fSlow16))) : fSlow455);
		double fSlow457 = (fSlow456 + (fSlow19 * ((iSlow12 ? fSlow455 : (iSlow15 ? (0.033064999999999997 - (0.031567999999999992 * fSlow13)) : (0.017281000000000001 - (0.0054060000000000011 * fSlow16)))) - fSlow456)));
		for (int i = 0; (i < count); i = (i + 1)) {
			iRec14[0] = 0;
			int iRec15 = iRec14[1];
			double fRec19 = (double(iRec6[1]) - (0.99784365473539205 * ((0.69999999999999996 * fRec20[2]) + (0.14999999999999999 * (fRec20[1] + fRec20[3])))));
			fRec30[0] = (fSlow1 + (0.999 * fRec30[1]));
			fVec0[0] = fSlow4;
			double fTemp0 = double(((fSlow4 == fVec0[1]) | iSlow5));
			fRec31[0] = ((0.999 * (fRec31[1] * fTemp0)) + (fSlow6 * (1.0 - (0.999 * fTemp0))));
			double fTemp1 = ((fSlow3 / fRec31[0]) + -0.10000000000000001);
			double fTemp2 = (fConst2 * ((1.0 - fRec30[0]) * fTemp1));
			double fTemp3 = (fTemp2 + -1.499995);
			int iTemp4 = int(fTemp3);
			int iTemp5 = int(std::min<double>(fConst1, double(std::max<int>(0, int(iTemp4)))));
			double fTemp6 = std::floor(fTemp3);
			double fTemp7 = (fTemp2 + (-1.0 - fTemp6));
			double fTemp8 = (0.0 - fTemp7);
			double fTemp9 = (fTemp2 + (-2.0 - fTemp6));
			double fTemp10 = (0.0 - (0.5 * fTemp9));
			double fTemp11 = (fTemp2 + (-3.0 - fTemp6));
			double fTemp12 = (0.0 - (0.33333333333333331 * fTemp11));
			double fTemp13 = (fTemp2 + (-4.0 - fTemp6));
			double fTemp14 = (0.0 - (0.25 * fTemp13));
			double fTemp15 = (fTemp2 - fTemp6);
			int iTemp16 = int(std::min<double>(fConst1, double(std::max<int>(0, int((iTemp4 + 1))))));
			double fTemp17 = (0.0 - fTemp9);
			double fTemp18 = (0.0 - (0.5 * fTemp11));
			double fTemp19 = (0.0 - (0.33333333333333331 * fTemp13));
			int iTemp20 = int(std::min<double>(fConst1, double(std::max<int>(0, int((iTemp4 + 2))))));
			double fTemp21 = (0.0 - fTemp11);
			double fTemp22 = (0.0 - (0.5 * fTemp13));
			double fTemp23 = (fTemp7 * fTemp9);
			int iTemp24 = int(std::min<double>(fConst1, double(std::max<int>(0, int((iTemp4 + 3))))));
			double fTemp25 = (0.0 - fTemp13);
			double fTemp26 = (fTemp23 * fTemp11);
			int iTemp27 = int(std::min<double>(fConst1, double(std::max<int>(0, int((iTemp4 + 4))))));
			fRec27[0] = (((((fRec2[((IOTA - (iTemp5 + 1)) & 2047)] * fTemp8) * fTemp10) * fTemp12) * fTemp14) + (fTemp15 * ((((((fRec2[((IOTA - (iTemp16 + 1)) & 2047)] * fTemp17) * fTemp18) * fTemp19) + (0.5 * (((fTemp7 * fRec2[((IOTA - (iTemp20 + 1)) & 2047)]) * fTemp21) * fTemp22))) + (0.16666666666666666 * ((fTemp23 * fRec2[((IOTA - (iTemp24 + 1)) & 2047)]) * fTemp25))) + (0.041666666666666664 * (fTemp26 * fRec2[((IOTA - (iTemp27 + 1)) & 2047)])))));
			fRec32[0] = ((0.050000000000000003 * fRec32[1]) + (0.94999999999999996 * fRec27[1]));
			double fRec28 = fRec32[0];
			fRec34[0] = fRec0[1];
			fRec35[(IOTA & 2047)] = (-1.0 * (0.99784365473539205 * ((0.69999999999999996 * fRec34[2]) + (0.14999999999999999 * (fRec34[1] + fRec34[3])))));
			double fTemp28 = (fConst2 * (fRec30[0] * fTemp1));
			double fTemp29 = (fTemp28 + -1.499995);
			int iTemp30 = int(fTemp29);
			int iTemp31 = int(std::min<double>(fConst1, double(std::max<int>(0, int(iTemp30)))));
			double fTemp32 = std::floor(fTemp29);
			double fTemp33 = (fTemp28 + (-1.0 - fTemp32));
			double fTemp34 = (0.0 - fTemp33);
			double fTemp35 = (fTemp28 + (-2.0 - fTemp32));
			double fTemp36 = (0.0 - (0.5 * fTemp35));
			double fTemp37 = (fTemp28 + (-3.0 - fTemp32));
			double fTemp38 = (0.0 - (0.33333333333333331 * fTemp37));
			double fTemp39 = (fTemp28 + (-4.0 - fTemp32));
			double fTemp40 = (0.0 - (0.25 * fTemp39));
			double fTemp41 = (fTemp28 - fTemp32);
			int iTemp42 = int(std::min<double>(fConst1, double(std::max<int>(0, int((iTemp30 + 1))))));
			double fTemp43 = (0.0 - fTemp35);
			double fTemp44 = (0.0 - (0.5 * fTemp37));
			double fTemp45 = (0.0 - (0.33333333333333331 * fTemp39));
			int iTemp46 = int(std::min<double>(fConst1, double(std::max<int>(0, int((iTemp30 + 2))))));
			double fTemp47 = (0.0 - fTemp37);
			double fTemp48 = (0.0 - (0.5 * fTemp39));
			double fTemp49 = (fTemp33 * fTemp35);
			int iTemp50 = int(std::min<double>(fConst1, double(std::max<int>(0, int((iTemp30 + 3))))));
			double fTemp51 = (0.0 - fTemp39);
			double fTemp52 = (fTemp49 * fTemp37);
			int iTemp53 = int(std::min<double>(fConst1, double(std::max<int>(0, int((iTemp30 + 4))))));
			fVec1[0] = (((((fRec35[((IOTA - (iTemp31 + 2)) & 2047)] * fTemp34) * fTemp36) * fTemp38) * fTemp40) + (fTemp41 * ((((((fRec35[((IOTA - (iTemp42 + 2)) & 2047)] * fTemp43) * fTemp44) * fTemp45) + (0.5 * (((fTemp33 * fRec35[((IOTA - (iTemp46 + 2)) & 2047)]) * fTemp47) * fTemp48))) + (0.16666666666666666 * ((fTemp49 * fRec35[((IOTA - (iTemp50 + 2)) & 2047)]) * fTemp51))) + (0.041666666666666664 * (fTemp52 * fRec35[((IOTA - (iTemp53 + 2)) & 2047)])))));
			iRec37[0] = ((1103515245 * iRec37[1]) + 12345);
			double fTemp54 = std::tan((fSlow8 * fRec31[0]));
			double fTemp55 = (1.0 / fTemp54);
			double fTemp56 = (((fTemp55 + 1.4142135623730949) / fTemp54) + 1.0);
			fRec36[0] = ((4.6566128752457969e-10 * double(iRec37[0])) - (((fRec36[2] * (((fTemp55 + -1.4142135623730949) / fTemp54) + 1.0)) + (2.0 * (fRec36[1] * (1.0 - (1.0 / mydsp_faustpower2_f(fTemp54)))))) / fTemp56));
			iRec38[0] = (((iRec38[1] + (iRec38[1] > 0)) * (fSlow4 <= fVec0[1])) + (fSlow4 > fVec0[1]));
			double fTemp57 = (double(iRec38[0]) / std::max<double>(1.0, (fConst4 * mydsp_faustpower2_f((1.0 - (fSlow9 * fRec31[0]))))));
			double fTemp58 = (fSlow7 * (((fRec36[2] + (fRec36[0] + (2.0 * fRec36[1]))) * std::max<double>(0.0, std::min<double>(fTemp57, (2.0 - fTemp57)))) / fTemp56));
			fVec2[0] = (fVec1[1] + fTemp58);
			fRec33[(IOTA & 2047)] = ((0.050000000000000003 * fRec33[((IOTA - 1) & 2047)]) + (0.94999999999999996 * fVec2[1]));
			double fRec29 = (((((fTemp8 * fTemp10) * fTemp12) * fTemp14) * fRec33[((IOTA - iTemp5) & 2047)]) + (fTemp15 * ((((((fTemp17 * fTemp18) * fTemp19) * fRec33[((IOTA - iTemp16) & 2047)]) + (0.5 * (((fTemp7 * fTemp21) * fTemp22) * fRec33[((IOTA - iTemp20) & 2047)]))) + (0.16666666666666666 * ((fTemp23 * fTemp25) * fRec33[((IOTA - iTemp24) & 2047)]))) + (0.041666666666666664 * (fTemp26 * fRec33[((IOTA - iTemp27) & 2047)])))));
			fRec24[0] = fRec28;
			double fRec25 = (fTemp58 + fRec24[1]);
			double fRec26 = fRec29;
			fRec21[(IOTA & 2047)] = fRec25;
			double fRec22 = (((((fTemp34 * fTemp36) * fTemp38) * fTemp40) * fRec21[((IOTA - (iTemp31 + 1)) & 2047)]) + (fTemp41 * ((((((fTemp43 * fTemp44) * fTemp45) * fRec21[((IOTA - (iTemp42 + 1)) & 2047)]) + (0.5 * (((fTemp33 * fTemp47) * fTemp48) * fRec21[((IOTA - (iTemp46 + 1)) & 2047)]))) + (0.16666666666666666 * ((fTemp49 * fTemp51) * fRec21[((IOTA - (iTemp50 + 1)) & 2047)]))) + (0.041666666666666664 * (fTemp52 * fRec21[((IOTA - (iTemp53 + 1)) & 2047)])))));
			fRec23[0] = fRec26;
			fRec20[0] = fRec23[1];
			fVec3[0] = fSlow10;
			fVec4[0] = (fRec20[1] + double((fSlow10 > fVec3[1])));
			fRec18[0] = (fVec4[1] - (((fRec18[1] * fSlow21) * fSlow24) + (fSlow25 * fRec18[2])));
			fRec39[0] = (fVec4[1] - (((fRec39[1] * fSlow32) * fSlow35) + (fSlow36 * fRec39[2])));
			fRec40[0] = (fVec4[1] - (((fRec40[1] * fSlow43) * fSlow46) + (fSlow47 * fRec40[2])));
			fRec41[0] = (fVec4[1] - (((fRec41[1] * fSlow54) * fSlow57) + (fSlow58 * fRec41[2])));
			fRec42[0] = (fVec4[1] - (((fRec42[1] * fSlow65) * fSlow68) + (fSlow69 * fRec42[2])));
			fRec43[0] = (fVec4[1] - (((fRec43[1] * fSlow76) * fSlow79) + (fSlow80 * fRec43[2])));
			fRec44[0] = (fVec4[1] - (((fRec44[1] * fSlow87) * fSlow90) + (fSlow91 * fRec44[2])));
			fRec45[0] = (fVec4[1] - (((fRec45[1] * fSlow98) * fSlow101) + (fSlow102 * fRec45[2])));
			fRec46[0] = (fVec4[1] - (((fRec46[1] * fSlow109) * fSlow112) + (fSlow113 * fRec46[2])));
			fRec47[0] = (fVec4[1] - (((fRec47[1] * fSlow120) * fSlow123) + (fSlow124 * fRec47[2])));
			fRec48[0] = (fVec4[1] - (((fRec48[1] * fSlow131) * fSlow134) + (fSlow135 * fRec48[2])));
			fRec49[0] = (fVec4[1] - (((fRec49[1] * fSlow142) * fSlow145) + (fSlow146 * fRec49[2])));
			fRec50[0] = (fVec4[1] - (((fRec50[1] * fSlow153) * fSlow156) + (fSlow157 * fRec50[2])));
			fRec51[0] = (fVec4[1] - (((fRec51[1] * fSlow164) * fSlow167) + (fSlow168 * fRec51[2])));
			fRec52[0] = (fVec4[1] - (((fRec52[1] * fSlow175) * fSlow178) + (fSlow179 * fRec52[2])));
			fRec53[0] = (fVec4[1] - (((fRec53[1] * fSlow186) * fSlow189) + (fSlow190 * fRec53[2])));
			fRec54[0] = (fVec4[1] - (((fRec54[1] * fSlow197) * fSlow200) + (fSlow201 * fRec54[2])));
			fRec55[0] = (fVec4[1] - (((fRec55[1] * fSlow208) * fSlow211) + (fSlow212 * fRec55[2])));
			fRec56[0] = (fVec4[1] - (((fRec56[1] * fSlow219) * fSlow222) + (fSlow223 * fRec56[2])));
			fRec57[0] = (fVec4[1] - (((fRec57[1] * fSlow230) * fSlow233) + (fSlow234 * fRec57[2])));
			fRec58[0] = (fVec4[1] - (((fRec58[1] * fSlow241) * fSlow244) + (fSlow245 * fRec58[2])));
			fRec59[0] = (fVec4[1] - (((fRec59[1] * fSlow252) * fSlow255) + (fSlow256 * fRec59[2])));
			fRec60[0] = (fVec4[1] - (((fRec60[1] * fSlow263) * fSlow266) + (fSlow267 * fRec60[2])));
			fRec61[0] = (fVec4[1] - (((fRec61[1] * fSlow274) * fSlow277) + (fSlow278 * fRec61[2])));
			fRec62[0] = (fVec4[1] - (((fRec62[1] * fSlow285) * fSlow288) + (fSlow289 * fRec62[2])));
			fRec63[0] = (fVec4[1] - (((fRec63[1] * fSlow296) * fSlow299) + (fSlow300 * fRec63[2])));
			fRec64[0] = (fVec4[1] - (((fRec64[1] * fSlow307) * fSlow310) + (fSlow311 * fRec64[2])));
			fRec65[0] = (fVec4[1] - (((fRec65[1] * fSlow318) * fSlow321) + (fSlow322 * fRec65[2])));
			fRec66[0] = (fVec4[1] - (((fRec66[1] * fSlow329) * fSlow332) + (fSlow333 * fRec66[2])));
			fRec67[0] = (fVec4[1] - (((fRec67[1] * fSlow340) * fSlow343) + (fSlow344 * fRec67[2])));
			fRec68[0] = (fVec4[1] - (((fRec68[1] * fSlow351) * fSlow354) + (fSlow355 * fRec68[2])));
			fRec69[0] = (fVec4[1] - (((fRec69[1] * fSlow362) * fSlow365) + (fSlow366 * fRec69[2])));
			fRec70[0] = (fVec4[1] - (((fRec70[1] * fSlow373) * fSlow376) + (fSlow377 * fRec70[2])));
			fRec71[0] = (fVec4[1] - (((fRec71[1] * fSlow384) * fSlow387) + (fSlow388 * fRec71[2])));
			fRec72[0] = (fVec4[1] - (((fRec72[1] * fSlow395) * fSlow398) + (fSlow399 * fRec72[2])));
			fRec73[0] = (fVec4[1] - (((fRec73[1] * fSlow406) * fSlow409) + (fSlow410 * fRec73[2])));
			fRec74[0] = (fVec4[1] - (((fRec74[1] * fSlow417) * fSlow420) + (fSlow421 * fRec74[2])));
			fRec75[0] = (fVec4[1] - (((fRec75[1] * fSlow428) * fSlow431) + (fSlow432 * fRec75[2])));
			fRec76[0] = (fVec4[1] - (((fRec76[1] * fSlow439) * fSlow442) + (fSlow443 * fRec76[2])));
			fRec77[0] = (fVec4[1] - (((fRec77[1] * fSlow450) * fSlow453) + (fSlow454 * fRec77[2])));
			double fTemp59 = (0.025000000000000001 * (((((((((((((((((((((((((((((((((((((((((fRec18[0] - fRec18[2]) * fSlow28) + ((fRec39[0] - fRec39[2]) * fSlow39)) + ((fRec40[0] - fRec40[2]) * fSlow50)) + ((fRec41[0] - fRec41[2]) * fSlow61)) + ((fRec42[0] - fRec42[2]) * fSlow72)) + ((fRec43[0] - fRec43[2]) * fSlow83)) + ((fRec44[0] - fRec44[2]) * fSlow94)) + ((fRec45[0] - fRec45[2]) * fSlow105)) + ((fRec46[0] - fRec46[2]) * fSlow116)) + ((fRec47[0] - fRec47[2]) * fSlow127)) + ((fRec48[0] - fRec48[2]) * fSlow138)) + ((fRec49[0] - fRec49[2]) * fSlow149)) + ((fRec50[0] - fRec50[2]) * fSlow160)) + ((fRec51[0] - fRec51[2]) * fSlow171)) + ((fRec52[0] - fRec52[2]) * fSlow182)) + ((fRec53[0] - fRec53[2]) * fSlow193)) + ((fRec54[0] - fRec54[2]) * fSlow204)) + ((fRec55[0] - fRec55[2]) * fSlow215)) + ((fRec56[0] - fRec56[2]) * fSlow226)) + ((fRec57[0] - fRec57[2]) * fSlow237)) + ((fRec58[0] - fRec58[2]) * fSlow248)) + ((fRec59[0] - fRec59[2]) * fSlow259)) + ((fRec60[0] - fRec60[2]) * fSlow270)) + ((fRec61[0] - fRec61[2]) * fSlow281)) + ((fRec62[0] - fRec62[2]) * fSlow292)) + ((fRec63[0] - fRec63[2]) * fSlow303)) + ((fRec64[0] - fRec64[2]) * fSlow314)) + ((fRec65[0] - fRec65[2]) * fSlow325)) + ((fRec66[0] - fRec66[2]) * fSlow336)) + ((fRec67[0] - fRec67[2]) * fSlow347)) + ((fRec68[0] - fRec68[2]) * fSlow358)) + ((fRec69[0] - fRec69[2]) * fSlow369)) + ((fRec70[0] - fRec70[2]) * fSlow380)) + ((fRec71[0] - fRec71[2]) * fSlow391)) + ((fRec72[0] - fRec72[2]) * fSlow402)) + ((fRec73[0] - fRec73[2]) * fSlow413)) + ((fRec74[0] - fRec74[2]) * fSlow424)) + ((fRec75[0] - fRec75[2]) * fSlow435)) + ((fRec76[0] - fRec76[2]) * fSlow446)) + ((fRec77[0] - fRec77[2]) * fSlow457)));
			double fRec16 = fTemp59;
			double fRec17 = fTemp59;
			iRec10[0] = iRec15;
			int iRec11 = iRec10[1];
			double fRec12 = fRec16;
			double fRec13 = fRec17;
			iRec6[0] = iRec11;
			double fRec7 = fRec19;
			double fRec8 = fRec12;
			double fRec9 = fRec13;
			fRec2[(IOTA & 2047)] = fRec7;
			double fRec3 = fRec22;
			double fRec4 = fRec8;
			double fRec5 = fRec9;
			fRec0[0] = fRec3;
			double fRec1 = fRec5;
			double fTemp60 = (fSlow0 * fRec1);
			output0[i] = FAUSTFLOAT(fTemp60);
			output1[i] = FAUSTFLOAT(fTemp60);
			iRec14[1] = iRec14[0];
			fRec30[1] = fRec30[0];
			fVec0[1] = fVec0[0];
			fRec31[1] = fRec31[0];
			fRec27[1] = fRec27[0];
			fRec32[1] = fRec32[0];
			for (int j0 = 3; (j0 > 0); j0 = (j0 - 1)) {
				fRec34[j0] = fRec34[(j0 - 1)];
			}
			IOTA = (IOTA + 1);
			fVec1[1] = fVec1[0];
			iRec37[1] = iRec37[0];
			fRec36[2] = fRec36[1];
			fRec36[1] = fRec36[0];
			iRec38[1] = iRec38[0];
			fVec2[1] = fVec2[0];
			fRec24[1] = fRec24[0];
			fRec23[1] = fRec23[0];
			for (int j1 = 3; (j1 > 0); j1 = (j1 - 1)) {
				fRec20[j1] = fRec20[(j1 - 1)];
			}
			fVec3[1] = fVec3[0];
			fVec4[1] = fVec4[0];
			fRec18[2] = fRec18[1];
			fRec18[1] = fRec18[0];
			fRec39[2] = fRec39[1];
			fRec39[1] = fRec39[0];
			fRec40[2] = fRec40[1];
			fRec40[1] = fRec40[0];
			fRec41[2] = fRec41[1];
			fRec41[1] = fRec41[0];
			fRec42[2] = fRec42[1];
			fRec42[1] = fRec42[0];
			fRec43[2] = fRec43[1];
			fRec43[1] = fRec43[0];
			fRec44[2] = fRec44[1];
			fRec44[1] = fRec44[0];
			fRec45[2] = fRec45[1];
			fRec45[1] = fRec45[0];
			fRec46[2] = fRec46[1];
			fRec46[1] = fRec46[0];
			fRec47[2] = fRec47[1];
			fRec47[1] = fRec47[0];
			fRec48[2] = fRec48[1];
			fRec48[1] = fRec48[0];
			fRec49[2] = fRec49[1];
			fRec49[1] = fRec49[0];
			fRec50[2] = fRec50[1];
			fRec50[1] = fRec50[0];
			fRec51[2] = fRec51[1];
			fRec51[1] = fRec51[0];
			fRec52[2] = fRec52[1];
			fRec52[1] = fRec52[0];
			fRec53[2] = fRec53[1];
			fRec53[1] = fRec53[0];
			fRec54[2] = fRec54[1];
			fRec54[1] = fRec54[0];
			fRec55[2] = fRec55[1];
			fRec55[1] = fRec55[0];
			fRec56[2] = fRec56[1];
			fRec56[1] = fRec56[0];
			fRec57[2] = fRec57[1];
			fRec57[1] = fRec57[0];
			fRec58[2] = fRec58[1];
			fRec58[1] = fRec58[0];
			fRec59[2] = fRec59[1];
			fRec59[1] = fRec59[0];
			fRec60[2] = fRec60[1];
			fRec60[1] = fRec60[0];
			fRec61[2] = fRec61[1];
			fRec61[1] = fRec61[0];
			fRec62[2] = fRec62[1];
			fRec62[1] = fRec62[0];
			fRec63[2] = fRec63[1];
			fRec63[1] = fRec63[0];
			fRec64[2] = fRec64[1];
			fRec64[1] = fRec64[0];
			fRec65[2] = fRec65[1];
			fRec65[1] = fRec65[0];
			fRec66[2] = fRec66[1];
			fRec66[1] = fRec66[0];
			fRec67[2] = fRec67[1];
			fRec67[1] = fRec67[0];
			fRec68[2] = fRec68[1];
			fRec68[1] = fRec68[0];
			fRec69[2] = fRec69[1];
			fRec69[1] = fRec69[0];
			fRec70[2] = fRec70[1];
			fRec70[1] = fRec70[0];
			fRec71[2] = fRec71[1];
			fRec71[1] = fRec71[0];
			fRec72[2] = fRec72[1];
			fRec72[1] = fRec72[0];
			fRec73[2] = fRec73[1];
			fRec73[1] = fRec73[0];
			fRec74[2] = fRec74[1];
			fRec74[1] = fRec74[0];
			fRec75[2] = fRec75[1];
			fRec75[1] = fRec75[0];
			fRec76[2] = fRec76[1];
			fRec76[1] = fRec76[0];
			fRec77[2] = fRec77[1];
			fRec77[1] = fRec77[0];
			iRec10[1] = iRec10[0];
			iRec6[1] = iRec6[0];
			fRec0[1] = fRec0[0];
		}
	}

};

#ifdef FAUST_UIMACROS
	#define FAUST_INPUTS 0
	#define FAUST_OUTPUTS 2
	#define FAUST_ACTIVES 10
	#define FAUST_PASSIVES 0
	FAUST_ADDHORIZONTALSLIDER("modularInterpInstr/[0]midi/freq", fHslider2, 440.0, 50.0, 1000.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("modularInterpInstr/[0]midi/bend", fHslider4, 0.0, -2.0, 2.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("modularInterpInstr/[0]midi/gain", fHslider5, 0.80000000000000004, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("modularInterpInstr/[0]midi/sustain", fHslider3, 0.0, 0.0, 1.0, 1.0);
	FAUST_ADDHORIZONTALSLIDER("modularInterpInstr/[1]body/shape", fHslider7, 0.0, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("modularInterpInstr/[1]body/scale", fHslider6, 0.0, 0.0, 1.0, 0.010999999999999999);
	FAUST_ADDBUTTON("modularInterpInstr/[1]body/tapBody", fButton1);
	FAUST_ADDHORIZONTALSLIDER("modularInterpInstr/pluckPosition", fHslider1, 0.80000000000000004, 0.0, 1.0, 0.01);
	FAUST_ADDHORIZONTALSLIDER("modularInterpInstr/outGain", fHslider0, 0.5, 0.0, 1.0, 0.01);
	FAUST_ADDBUTTON("modularInterpInstr/gate", fButton0);
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

extern "C" int csound_main(CSOUND *csound) {
        csound->Message(csound, "Hello, World! This is csound_main with csound: %p.\\n", csound);
        //std::cerr << "And this is std::cerr!" << std::endl;
        return 0;
};

}}

gi_result clang_orc gS_guitar_source_code, "-v -O0 -std=c++14 -I/usr/local/include/csound -stdlib=libstdc++", "/usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.so /usr/lib/gcc/x86_64-linux-gnu/9/libgcc_s.so /usr/lib/x86_64-linux-gnu/libm.so /usr/lib/x86_64-linux-gnu/libpthread.so"
</CsInstruments>
<CsScore>
f 0 30
</CsScore>
</CsoundSynthesizer>
