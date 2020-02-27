# - Find fluidsynth
# Find the native fluidsynth includes and library
#
#  FLUIDSYNTH_INCLUDE_DIR - where to find fluidsynth.h
#  FLUIDSYNTH_LIBRARIES   - List of libraries when using fluidsynth.
#  FLUIDSYNTH_FOUND       - True if fluidsynth found.

if (FLUIDSYNTH_INCLUDE_DIR AND FLUIDSYNTH_LIBRARIES)
  # Already in cache, be silent
  set (FluidSynth_FIND_QUIETLY TRUE)
endif ()

find_path(FLUIDSYNTH_INCLUDE_DIR
    NAMES
      fluidsynth.h
    HINTS
      /usr/include
      /usr/local/include
    )

find_library(FLUIDSYNTH_LIBRARIES
    NAMES
      fluidsynth
      libfluidsynth
    HINTS
      /usr/lib
      /usr/local/lib
      /usr/local/lib64
    )

mark_as_advanced (FLUIDSYNTH_LIBRARIES FLUIDSYNTH_INCLUDE_DIR)

# handle the QUIETLY and REQUIRED arguments and set FLUIDSYNTH_FOUND to TRUE if 
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FluidSynth DEFAULT_MSG FLUIDSYNTH_LIBRARIES FLUIDSYNTH_INCLUDE_DIR)
