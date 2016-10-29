/**
 * LUAINST -- WRITE RTCMIX INSTRUMENTS IN LUA
 * Michael Gogins
 * 18 January 2012
 *
 * LuaInstrument is an Instrument class 
 * that enables users to define Instrument code in LuaJIT,
 * which is compiled just in time and runs nearly as fast as C.
 * LuaJIT has an FFI facility that enables such Lua 
 * instrument code to call any public C function in the process
 * space. This includes most of the RTCmix functions.
 *
 * Build LUAINST with this command in the RTcmix directory
 * (you may need to change the path of the ffi_rtinstrument.cpp file).
 * And if you don't have OpenMP support, just omit -fopenmp 
 * and add -lpthread.
 *
 * g++ -O2 -shared -fopenmp -Wl,-soname,libLUAINST.so -o shlib/libLUAINST.so ~/silencio/ffi_rtinstrument.cpp -Iinclude -Isrc -lm -ldl -lluajit-5.1
 */
#include <RTcmix.h>
#include <Instrument.h>
#include <ugens.h>
#include <rt.h>

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <string>
#include <vector>

#include <omp.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/syscall.h>

extern "C"
{
#include <lua/lua.h>
#include <lua/lauxlib.h>
#include <lua/lualib.h>
}

/** 
 * Holds basic performance state for an instance (not class) 
 * of a LUAINST Instrument and permits that state
 * to be passed to and from the actual 
 * Lua instrument code, e.g. it holds 
 * the frame count, input and output buffers, etc.
 * It also holds a pointer to additional
 * instance instrument state that may be defined
 * by the actual Lua instrument code.
 */
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

static std::map<std::string, std::string> luaCodeForInstrumentNames;

/**
 * Holds state specific to a Lua instrument class (not instance).
 */
struct LuaInstrumentClass_t
{
  LuaInstrumentClass_t() : initialized(false), init_key(0), run_key(0) {}
  bool initialized;
  std::string luacode;
  int init_key;
  int run_key;
};

LuaInstrumentClass_t &manageLuaInstrumentClass(const lua_State *L, const std::string &name)
{
  static std::map<const lua_State *, std::map<std::string, LuaInstrumentClass_t> > luaInstrumentClasses;
  LuaInstrumentClass_t *luaInstrumentClass = 0;
#pragma omp critical(lc_getrefkey)
  {
    luaInstrumentClass = &luaInstrumentClasses[L][name];
  }
  return *luaInstrumentClass;
}

/**
 * Associates Lua states with threads.
 */
lua_State *manageLuaState()
{
  static std::map<int, lua_State *> luaStatesForThreads;
  lua_State *L = 0;
#pragma omp critical(lc_manageLuaState)
  {
    int threadId = pthread_self();
    if (luaStatesForThreads.find(threadId) == luaStatesForThreads.end())
      {
	L = lua_open();
	luaL_openlibs(L);
	luaStatesForThreads[threadId] = L;
	advise("manageLuaState", "Created Lua state %p for thread %p.\n", L, threadId);
      }  
    else
      {
	L = luaStatesForThreads[threadId];
      }	    
  }
  return L;
}

/**
 * LUAINST is actually a wrapper around a Lua "class"
 * that defines a Lua cdef NAME structure containing 
 * instrument state, a Lua NAME_init function that performs
 * the work of any other RTcmix Instrument::init function,
 * and a Lua NAME_run function that performs the work of any 
 * other RTcix Instrument::run function. The actual 
 * Lua instrument state and functions are looked up by 
 * NAME. 
 *
 * The Lua cdef NAME, the NAME_init function, and the 
 * NAME_run function must be defined by calling the 
 * lua_intro command with a chunk of Lua source code.
 *
 * Note that there is one LUAINST class for both 
 * output and input/output processing. To use an input,
 * set inskip to non-zero and use bus_config.
 */
class LUAINST : public Instrument 
{
public:
  LUAINST() 
  {
    std::memset(&state, 0, sizeof(LuaInstrumentState));        
  }
  virtual ~LUAINST()
  {
    if (state.name) {
      free((void *)state.name);
      state.name = 0;
    }
    if (state.parameters) {
      delete[] state.parameters;
      state.parameters = 0;
    }
    if (state.input) {
      delete[] state.input;
      state.input = 0;
    }
    if (state.output) {
      delete[] state.output;
      state.output = 0;
    }
    state.initialized = false;
  }
  /**
   * These pfields are standard for LUAINST instruments:
   * p0 = Lua instrument name (string, all others are doubles).
   * p1 = Output start time (outskip).
   * p2 = Input start time (inskip, must be zero if there is no input).
   * p3 = Duration.
   * p4 = Amplitude.
   * pN = User-defined optional parameters.
   */
  virtual int init(double *parameters, int parameterCount)
  {
    state.name = strdup(DOUBLE_TO_STRING(parameters[0]));
    state.parameters = new double[parameterCount];
    state.parameterCount = parameterCount;
    for (int parameterI = 0; parameterI < parameterCount; ++parameterI) 
      {
	state.parameters[parameterI] = parameters[parameterI];
      }
    if (rtsetoutput((float) parameters[1], (float) parameters[3], this) == -1) 
      {
	return DONT_SCHEDULE;
      }
    if (parameters[2] != 0.0) 
      {
	if (rtsetinput(parameters[1], this) == -1) {
	  return DONT_SCHEDULE;
	}
	state.inputChannelCount = inputChannels();
      }
    state.outputChannelCount = outputChannels();
    return nSamps();
  }
  /**
   * Allocates input and output buffers just before run() is called.
   */
  virtual int configure()
  {
    if (state.input) 
      {
	delete [] state.input;
	state.input = 0;
      }
    state.inputSampleCount = RTBUFSAMPS * inputChannels();
    state.input = new float[state.inputSampleCount];
    if (state.output) 
      {
	delete [] state.output;
	state.output = 0;
      }
    state.outputSampleCount = RTBUFSAMPS * outputChannels();
    state.output = new float[state.outputSampleCount];
    return 0;
  }
  /**
   * For Lua instruments, the run method also performs the Lua 
   * portion of the init method. That is because the init 
   * method is called from the main RTcmix thread whereas the 
   * run method is called from the traverse thread, but Lua 
   * is not thread-safe and our Lua state management code
   * creates a separate Lua state for each thread.
   */
  virtual int run()
  {
    //advise("LUAINST::run", "Began (thread %p)...", pthread_self());
    int result = 0;
    if (state.parameters[2] != 0.0) 
      {
	state.inputSampleCount = RTBUFSAMPS * inputChannels();
	rtgetin(state.input, this, state.inputSampleCount);
      }
    lua_State *L = manageLuaState();
    LuaInstrumentClass_t &luaInstrumentClass = manageLuaInstrumentClass(L, state.name);
    if (!luaInstrumentClass.initialized) 
      {
	const char *luacode = luaCodeForInstrumentNames[state.name].c_str();
	advise("LUAINST", "Defining Lua instrument code:\n\n%0.120s\n...\n", luacode);
	result = luaL_dostring(L, luacode);
	if (result == 0)
	  {
	    char init_function[0x100];
	    std::snprintf(init_function, 0x100, "%s_init", state.name);
	    lua_getglobal(L, init_function);
	    if (!lua_isnil(L, 1))
	      {
		luaInstrumentClass.init_key = luaL_ref(L, LUA_REGISTRYINDEX);
		lua_pop(L, 1);
	      }
	    else
	      {
		exit(die("LUAINST", "Failed to register: %s.", init_function));
	      }
	    char run_function[0x100];
	    std::snprintf(run_function, 0x100, "%s_run", state.name);
	    lua_getglobal(L, run_function);
	    if (!lua_isnil(L, 1))
	      {
		luaInstrumentClass.run_key = luaL_ref(L, LUA_REGISTRYINDEX);
		lua_pop(L, 1);
	      }
	    else
	      {
		exit(die("LUAINST", "Failed to register: %s.", run_function));
	      }
	  }
	else
	  {
	    warn("LUAINST", "Failed with: %d\n", result);
	  }
	luaInstrumentClass.initialized = true;
      }
    if (!state.initialized) 
      {
	lua_rawgeti(L, LUA_REGISTRYINDEX, luaInstrumentClass.init_key);
	lua_pushlightuserdata(L, &state);
	if (lua_pcall(L, 1, 1, 0) != 0)
	  {
	    rterror("LUAINST", "Lua error in \"%s_init\": %s.\n", state.name, lua_tostring(L, -1));
	  }
	result = lua_tonumber(L, -1);
	lua_pop(L, 1);
	state.initialized = true;
      }
    state.startFrame = state.endFrame;
    long frameCount = framesToRun();
    state.endFrame += frameCount;
    doupdate();
    lua_rawgeti(L, LUA_REGISTRYINDEX, luaInstrumentClass.run_key);
    lua_pushlightuserdata(L, &state);
    if (lua_pcall(L, 1, 1, 0) != 0)
      {
	die("LUAINST", "Lua error in \"%s_run\": %s with key %p frame %i.\n", state.name, lua_tostring(L, -1), luaInstrumentClass.run_key, state.currentFrame);
	exit(-1);
      }
    result = lua_tonumber(L, -1);
    lua_pop(L, 1);
    rtbaddout(state.output, frameCount);
    increment(frameCount);
    return framesToRun();
  }
private:
  void doupdate()
  {
    update(state.parameters, state.parameterCount);
  }
  LuaInstrumentState state;
};

extern "C" 
{
  /**
   * Register a Lua instrument with RTcmix as NAME.
   * NAME must be defined in the Lua code and consists
   * of the following:
   * (1) A LuaJIT FFI cdef that declares the type of 
   *     a NAME C structure containing all state for the 
   *     the instrument. This can be as elaborate as one 
   *     likes, contain arrays and pointers, etc.
   * (2) A Lua NAME_init(LuaInstrumentState) function
   *     that creates and initializes an instance of 
   *     the NAME structure, then assigns its address
   *     to the LuaInstrumentState.instanceState pointer.
   *     This is what associates the Lua instrument 
   *     instance that does all the actual work, with 
   *     the RTCmix C++ LUAINST instance that is 
   *     created and managed by RTcmix.
   * (3) A Lua NAME_run(LuaInstrumentState) function
   *     that performs the same work as a regular 
   *     RTcmix Instrument::run function.
   * Note that any other Lua code in the text also will 
   * be executed. This can be used to install or require 
   * arbitrary Lua modules.
   */
  int LUA_INTRO(const char *NAME, const char *luacode)
  {
    luaCodeForInstrumentNames[NAME] = luacode;
    return 0;
  }
  
  double lua_intro(float *p, int n, double *pp)
  {
    const char *NAME = DOUBLE_TO_STRING(pp[0]);
    const char *luacode = DOUBLE_TO_STRING(pp[1]);
    return (double) LUA_INTRO(NAME, luacode);
  }
};

/** 
 * Factory function called by RTcmix during 
 * performance to make new instances of LUAINST which,
 * in turn, will call lua_intro to define and create
 * new instances of the actual Lua instruments..
 */
Instrument *makeLUAINST()
{
  LUAINST *luainst = new LUAINST();
  luainst->set_bus_config("LUAINST");
  return luainst;
}

void rtprofile()
{
  RT_INTRO("LUAINST", makeLUAINST);
  UG_INTRO("lua_intro", lua_intro);
}

