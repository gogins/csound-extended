print()
print('H E L L O ,   T H I S   I S   R T C M I X   F R O M   L U A .')
print('Example code by Michael Gogins')
print('16 January 2012')
print()

-- Load the just-in-time compiler
-- and the foreign function interface library.

local jit = require('jit')
local ffi = require('ffi')

-- Declare to LuaJIT's FFI library the functions that we need,
-- using basic C synatax. Note that "RTcmix *" has become "void *"
-- because we do not wish to declare the type of RTcmix to FFI.
-- Note that [[ begins a multi-line string constant and ]] ends it.

ffi.cdef[[

// RTCmix functions.

void *	  ffi_create(double tsr, int tnchans, int bsize, const char *opt1, const char *opt2, const char *opt3);
double 	  ffi_cmdval(const char *name);
double 	  ffi_cmdval_d(const char *name, int n_args, double p0, ...);
double 	  ffi_cmdval_s(const char *name, int n_args, const char* p0, ...);
void *	  ffi_cmd_d(const char *name, int n_args, double p0, ...);
void *	  ffi_cmd_s(const char *name, int n_args, const char* p0, ...);
void 	  ffi_printOn();
void 	  ffi_printOff();
void 	  ffi_close();
void 	  ffi_destroy();
void option_dump(void);

// Operating system and runtime library functions.

unsigned int sleep(unsigned int seconds);

]]

-- Load the RTcmix library. Note that symbols must be loaded as globals, 
-- so they can be referenced by instruments that RTcmix will dynamically load.

local cmix = ffi.load('/home/mkg/RTcmix/lib/librtcmix.so', true)

-- Now we are ready to actually use RTcmix in the ordinary way.
-- It seems to be necessary to pass the device selection to the creator.

cmix.ffi_create(44100, 2, 4096, 'device=plughw:1', nil, nil)
ffi.C.sleep(1)
cmix.ffi_printOn()
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
ffi.C.sleep(8)
print()
print('Done.')
print()



