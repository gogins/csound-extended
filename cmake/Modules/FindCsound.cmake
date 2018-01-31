# Try to find the CSOUND library.
# Once done this will define:
#  CSOUND_FOUND - System has the CSOUND library.
#  CSOUND_INCLUDE_DIRS - The CSOUND include directories.
#  CSOUND_LIBRARIES - The libraries needed to use the CSOUND.

find_path(CSOUND_INCLUDE_DIR csound.h PATH_SUFFIXES csound)
find_library(CSOUND_LIBRARY NAMES csound )

set(CSOUND_INCLUDE_DIRS ${CSOUND_INCLUDE_DIR})
set(CSOUND_LIBRARIES ${CSOUND_LIBRARY})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set CSOUND_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(CSOUND DEFAULT_MSG
                                  CSOUND_LIBRARY CSOUND_INCLUDE_DIR)

mark_as_advanced(CSOUND_INCLUDE_DIR CSOUND_LIBRARY)
