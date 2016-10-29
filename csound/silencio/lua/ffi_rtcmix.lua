print[[

=============================================================
H E L L O ,   T H I S   I S   R T C M I X   F R O M   L U A .
Example code by Michael Gogins
25 January 2012
=============================================================

]]

-- Load the just-in-time compiler
-- and its foreign function interface library.

local jit = require('jit')
local ffi = require('ffi')

-- Declare to the FFI library the functions that we need,
-- using basic C synatax. Note that "RTcmix *" has become "void *"
-- because we do not wish to declare the type of RTcmix to FFI.
-- Note that [[ begins a multi-line string constant and ]] ends it.

ffi.cdef[[

// RTCmix types.

void *ffi_create(double tsr, int tnchans, int bsize, const char *opt1, const char *opt2, const char *opt3);
double ffi_cmdval(const char *name);
double ffi_cmdval_d(const char *name, int n_args, double p0, ...);
double ffi_cmdval_s(const char *name, int n_args, const char* p0, ...);
void *ffi_cmd_d(const char *name, int n_args, double p0, ...);
void *ffi_cmd_s(const char *name, int n_args, const char* p0, ...);
void *ffi_cmd_l(const char *name, const char *luaname, int n_args, ...);
void *ffi_cmd_l_15(const char *name, const char *luaname, double p1, double p2, double p3, double p4, double p5, double p6, double p7, double p8, double p9, double p10, double p11, double p12, double p13, double p14, double p15);
void ffi_printOn();
void ffi_printOff();
void ffi_close();
void ffi_destroy();
int advise(const char*, const char *,...);
int warn(const char*, const char *,...);
int rterror(const char*, const char *,...);
int die(const char*, const char *,...);

// Operating system and runtime library functions.

unsigned int sleep(unsigned int seconds);

]]

-- Load the RTcmix library. Note that symbols must be loaded as globals, 
-- so they can be referenced by instruments that RTcmix, later on,
-- will itself dynamically load.

local cmix = ffi.load('/home/mkg/RTcmix/lib/librtcmix.so', true)

-- Now we are ready to actually use RTcmix in the usual way.
-- It seems to be necessary to pass the device selection to the creator.

cmix.ffi_create(44100, 2, 4096, 'device=plughw', nil, nil)
ffi.C.sleep(1)
cmix.ffi_printOn()

-- Because all FFI functions are typed as C, there are no
-- parameter type overloads; hence the FFI function names end with _s or _d 
-- to indicate whether they have string or double varargs.

print[[
============================================
First, we create and perform a score in Lua.
============================================
]]

cmix.ffi_cmd_s("load", 1, "METAFLUTE")
cmix.ffi_cmd_d("makegen", 7, 1.0, 24.0, 1000.0, 0.0, 1.0, 1.0, 1.0)
cmix.ffi_cmd_d("makegen", 11, 2.0, 24.0, 1000.0, 0.0, 0.0, 0.05, 1.0, 0.95, 1.0, 1.0, 0.0)
cmix.ffi_cmd_d("SFLUTE", 7, 0.0, 1.0, 0.1, 106.0, 25.0, 5000.0, 0.5)
cmix.ffi_cmd_d("SFLUTE", 7, 1.0, 1.0, 0.1, 95.0, 21.0, 5000.0, 0.5)
cmix.ffi_cmd_d("SFLUTE", 7, 2.0, 1.0, 0.1, 89.0, 19.0, 5000.0, 0.5)
cmix.ffi_cmd_d("SFLUTE", 7, 3.0, 1.0, 0.1, 75.0, 19.0, 5000.0, 0.5)
cmix.ffi_cmd_d("SFLUTE", 7, 4.0, 1.0, 0.1, 70.0, 15.0, 5000.0, 0.5)
cmix.ffi_cmd_d("SFLUTE", 7, 5.0, 1.0, 0.1, 67.0, 16.0, 5000.0, 0.5)
cmix.ffi_cmd_d("SFLUTE", 7, 6.0, 1.0, 0.1, 56.0, 17.0, 5000.0, 0.5)
cmix.ffi_cmd_d("SFLUTE", 7, 7.0, 1.0, 0.1, 53.0, 25.0, 5000.0, 0.5)

print[[

=======================================================
Next, a totally rudimentary synthesizer written in Lua!
=======================================================

]]

cmix.ffi_cmd_s("load", 1, "LUAINST")
--[[
The next line is key. We are registering the Lua instrument
LUA_OSC's Lua code, using the lua_intro function that the 
LUAINST library has loaded into RTcmix.

The double brackets enclose Lua multi-line string constants;
zero or more equals signs within the brackets denote zero or 
more levels of nesting.
]]
cmix.ffi_cmd_s("lua_intro", 2, "LUA_OSC", [[
local ffi = require('ffi')
local math = require('math')
local m = ffi.load('m')
ffi.cdef[=[
  double sin(double);
  void *ffi_create(double tsr, int tnchans, int bsize, const char *opt1, const char *opt2, const char *opt3);
  double ffi_cmdval(const char *name);
  double ffi_cmdval_d(const char *name, int n_args, double p0, ...);
  double ffi_cmdval_s(const char *name, int n_args, const char* p0, ...);
  void *ffi_cmd_d(const char *name, int n_args, double p0, ...);
  void *ffi_cmd_s(const char *name, int n_args, const char* p0, ...);
  void *ffi_cmd_l(const char *name, const char *luaname, int n_args, ...);
  void ffi_printOn();
  void ffi_printOff();
  void ffi_close();
  int ffi_bufsamps();
  float ffi_sr(); 
  int ffi_chans(); 
  long ffi_getElapsedFrames(); 
  void ffi_destroy();
  int LUA_INTRO(const char *NAME, const char *luacode);
  int advise(const char*, const char *,...);
  double cpsoct(double oct);
  double octcps(double cps);
  double octpch(double pch);
  double cpspch(double pch);
  double pchoct(double oct);
  double pchcps(double cps);
  double midipch(double pch);
  double pchmidi(unsigned char midinote);
  double octmidi(unsigned char midinote);
  void *calloc(size_t num, size_t size);	
  double fabs(double x);
  struct LuaInstrumentState
  {
	char *name;
	int parameterCount;
	double *parameters;
  	int inputChannelCount;
  	int inputSampleCount;
  	float *input;
  	int outputChannelCount;
	int outputSampleCount;
  	float *output;
  	int startFrame;
  	int currentFrame;
  	int endFrame;
  	bool initialized;
  	// This points to a C structure, declared as a LuaJIT FFI cdef in Lua code,
  	// which contains state that specifically belongs to an instance of a Lua 
  	// instrument. If such state exists, the NAME_init function must declare 
  	// and define an instance of a C structure containing all elements of that 
  	// state, and set this pointer to the address of that structure. And,
	// the C allocator must be used to allocate this structure so that it will 
	// not be garbage-collected by the Lua runtime.
  	void *instanceState;
  };
]=]

-- Obtain a ctype for a pointer to the LuaInstrumentState struct,
-- for greater efficiency.

local LuaInstrumentState_ct = ffi.typeof("struct LuaInstrumentState *");

-- We may, if we wish, load RTcmix into the symbol table for the Lua instrument.

local cmix = ffi.load('/home/mkg/RTcmix/lib/librtcmix.so', true)

function LUA_OSC_init(state)
	-- Type casting the ctype for LuaInstrumentState enables us to 
	-- access members of that type as though they are Lua variables.
	local luastate = ffi.cast(LuaInstrumentState_ct, state)
	cmix.advise('LUA_OSC', string.format('outskip: %9.4f  inskip: %9.4f  dur: %9.4f  amp: %9.4f  freq: %9.4f', luastate.parameters[1], luastate.parameters[2], luastate.parameters[3], luastate.parameters[4], luastate.parameters[5]))
	return 0
end

function LUA_OSC_run(state)
	 -- Type casting the ctype for LuaInstrumentState enables us to 
	 -- access members of that type as though they are Lua variables.
	 local luastate = ffi.cast(LuaInstrumentState_ct, state)
	 local sampleI = 0
	 for currentFrame = luastate.startFrame, luastate.endFrame - 1 do
	     -- print(luastate.startFrame, currentFrame, luastate.endFrame)
	     local t = currentFrame / cmix.ffi_sr()      
	     local w = 2.0 * math.pi * luastate.parameters[5]
             local x = m.sin(w * t)
	     local signal = x * luastate.parameters[4]
	     luastate.output[sampleI] = signal
	     sampleI = sampleI + 1
	     luastate.output[sampleI] = signal
	     sampleI = sampleI + 1
	 end
	 return 0
end

]])

-- The ffi_cmd_l function invokes a Lua instrument; there is a second
-- string name for the actual Lua instrument class defined above.
-- This function could be invoked from Minc or any other language as it is 
-- now a loaded and bound CMIX function.

cmix.ffi_cmd_l("LUAINST", "LUA_OSC", 5,  9.0, 0.0, 5.0, 4000.0, 440.00)
cmix.ffi_cmd_l("LUAINST", "LUA_OSC", 5, 10.0, 0.0, 5.0, 4000.0, 554.37)
cmix.ffi_cmd_l("LUAINST", "LUA_OSC", 5, 11.0, 0.0, 5.0, 4000.0, 659.26)

print[[

====================================
Circle Map
====================================

]]

cmix.ffi_cmd_s("lua_intro", 2, "CIRCLEMAP", [[
local ffi = require('ffi')
local math = require('math')
local m = ffi.load('m')
ffi.cdef[=[

  struct CIRCLEMAP
  {
	double twopi;
	double aphs0;
	double aphs;
	double kphaseincrement;
	double kcouple;
  };

]=]

local LuaInstrumentState_ct = ffi.typeof("struct LuaInstrumentState *")
local CIRCLEMAP_ct = ffi.typeof("struct CIRCLEMAP")
local CIRCLEMAPp_ct = ffi.typeof("struct CIRCLEMAP *")
local cmix = ffi.load('/home/mkg/RTcmix/lib/librtcmix.so', true)

function CIRCLEMAP_init(state)
	local luastate = ffi.cast(LuaInstrumentState_ct, state)
	luastate.instanceState = ffi.C.calloc(1, ffi.sizeof(CIRCLEMAP_ct))
	local circlemap = ffi.cast(CIRCLEMAPp_ct, luastate.instanceState)
	circlemap.twopi = 2.0 * math.pi
	print(circlemap.twopi)
	circlemap.aphs0 = luastate.parameters[7]
	cmix.advise('CIRCLEMAP', string.format('outskip: %9.4f  inskip: %9.4f  dur: %9.4f  amp: %9.4f  freq: %9.4f  coupling: %9.4f  phase: %9.4f', luastate.parameters[1], luastate.parameters[2], luastate.parameters[3], luastate.parameters[4], luastate.parameters[5], luastate.parameters[6], luastate.parameters[7]))
	return 0
end

function CIRCLEMAP_run(state)
	local luastate = ffi.cast(LuaInstrumentState_ct, state)
	local circlemap = ffi.cast(CIRCLEMAPp_ct, luastate.instanceState)
	local sampleI = 0
	local amp = luastate.parameters[4]
	circlemap.kphaseincrement = circlemap.twopi / luastate.parameters[5]
	circlemap.kcouple = luastate.parameters[6]
	for currentFrame = luastate.startFrame, luastate.endFrame - 1 do
	     circlemap.aphs = (circlemap.aphs0 + circlemap.kphaseincrement - (circlemap.kcouple / circlemap.twopi) * math.sin(circlemap.twopi * circlemap.aphs0)) % 1.0	     
	     circlemap.aphs0 = circlemap.aphs 
	     local signal = circlemap.aphs0 * amp
	     --print(luastate.startFrame, currentFrame, luastate.endFrame, signal)
	     luastate.output[sampleI] = signal
	     sampleI = sampleI + 1
	     luastate.output[sampleI] = signal
	     sampleI = sampleI + 1
        end
	return 0
end

]])

local outskip = 18
local inskip = 0.0

-- outskip1, inskip2, dur3, amp4, hz5, kcouple6, kphase7

cmix.ffi_cmd_l("LUAINST", "CIRCLEMAP", 7, outskip, inskip, 20.0, 15000.0, 220.0, 0.02485, 0.3039)
outskip = outskip + 21
cmix.ffi_cmd_l("LUAINST", "CIRCLEMAP", 7, outskip, inskip, 20.0, 15000.0, 22.0, 0.0285,  0.339)

print [[

Press [Ctrl-C] to exit...

]]

while true do
      ffi.C.sleep(1)
end



