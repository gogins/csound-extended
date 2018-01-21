include(ExternalProject)

message(STATUS "Building CsoundHtml5...")
ExternalProject_Add(CsoundHtml5
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/CsoundHtml5"
    CONFIGURE_COMMAND "qmake"
    BUILD_COMMAND "${MAKE}")
