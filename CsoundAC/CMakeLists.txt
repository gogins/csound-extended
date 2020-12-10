# Build CsoundAC
cmake_minimum_required(VERSION 3.10)
include(CheckIncludeFiles)

set(CMAKE_CXX_STANDARD 17)

# TODO: These will need something more sophisticated.
# For now I am hard-coding them to the Csound Debian control values.

set(CMAKE_INCLUDE_CURRENT_DIR "On")

option(BUILD_CSOUND_AC "Build CsoundAC extension module for Csound with algorithmic composition (needs C++ interface)" ON)
option(BUILD_CSOUND_AC_PYTHON_INTERFACE "Build the Csound AC Python interface" ON)
option(USE_SYSTEM_PORTSMF ON)

find_library(MUSICXML_LIBRARY musicxml2)
#find_package(Eigen3 REQUIRED NO_MODULE)
find_package(FLTK)
find_package(SWIG)
find_package(PYTHON COMPONENTS Interpreter Development)
message(STATUS "Python libraries:  ${PYTHONLIBS_VERSION_STRING}")
find_library(PORTSMF_LIBRARY portSMF)
find_library(JPEG_LIBRARY jpeg)
find_library(PNG_LIBRARY NAMES png png16 libpng16)
find_library(Z_LIBRARY NAMES z zlib)

list(APPEND CMAKE_SWIG_FLAGS
"-I${CMAKE_SOURCE_DIR}/dependencies/csound/interfaces")

if(BUILD_CSOUND_AC)
    set(libcsoundac_SRCS
        Cell.cpp
        ChordLindenmayer.cpp
        Composition.cpp
        Conversions.cpp
        Counterpoint.cpp
        CounterpointMain.cpp
        CounterpointNode.cpp
        Event.cpp
        ImageToScore.cpp
        Lindenmayer.cpp
        MCRM.cpp
        Midifile.cpp
        MusicModel.cpp
        Node.cpp
        Random.cpp
        Rescale.cpp
        Score.cpp
        ScoreModel.cpp
        ScoreNode.cpp
        Sequence.cpp
        Shell.cpp
        Soundfile.cpp
        StrangeAttractor.cpp
        Voicelead.cpp
        VoiceleadingNode.cpp)

    if(NOT USE_SYSTEM_PORTSMF)
        list(APPEND libcsoundac_SRCS
            allegro.cpp
            allegrord.cpp
            allegroserial.cpp
            allegrosmfrd.cpp
            allegrosmfwr.cpp
            allegrowr.cpp
            mfmidi.cpp
            strparse.cpp
            trace.cpp)
    endif()

    if (MSVC)
        add_library(libCsoundAC STATIC ${libcsoundac_SRCS})
    else()
        add_library(libCsoundAC SHARED ${libcsoundac_SRCS})
    endif()

    target_include_directories(libCsoundAC SYSTEM PRIVATE ${Python3_INCLUDE_DIRS})
    target_include_directories(libCsoundAC SYSTEM PRIVATE ${CSOUND_INCLUDE_PATH})
    target_include_directories(libCsoundAC PRIVATE ${EIGEN3_INCLUDE_PATH})
    target_include_directories(libCsoundAC PRIVATE ${FLTK_INCLUDE_DIR})

    set_target_properties(libCsoundAC PROPERTIES
        SOVERSION "${APIVERSION}"
        OUTPUT_NAME CsoundAC
        RUNTIME_OUTPUT_DIRECTORY "${BUILD_BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY "${BUILD_LIB_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY "${BUILD_LIB_DIR}")

    target_link_libraries(libCsoundAC PRIVATE ${CSOUND_LIBRARIES} ${FLTK_LIBRARIES} ${OpenCV_LIBRARIES} ${LIBSNDFILE_LIBRARY})
    if(USE_SYSTEM_PORTSMF)
        target_link_libraries(libCsoundAC PRIVATE ${PORTSMF_LIBRARY})
    endif()

    if(WIN32) # for MSYS2/mingw64
        #  mingw64 needs this to create a valid .pyd.
        if(NOT MSVC AND CMAKE_SIZEOF_VOID_P EQUAL 8)
            target_compile_definitions(libCsoundAC PRIVATE -DMS_WIN64)
        endif()
        target_link_libraries(libCsoundAC PRIVATE ${JPEG_LIBRARY} ${PNG_LIBRARY} ${Z_LIBRARY})
    endif()

    if(HAVE_UNISTD_H)
      set_target_properties(libCsoundAC PROPERTIES
        COMPILE_FLAGS "-DHAVE_UNISTD_H")
    endif()

    if(MUSICXML_LIBRARY)
        target_link_libraries(libCsoundAC PRIVATE csnd6 ${CSOUND_LIBRARIES} ${FLTK_LIBRARIES} ${OpenCV_LIBS} ${LIBSNDFILE_LIBRARY} ${MUSICXML_LIBRARY})
    endif()

    install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIRECTORY}"
        DESTINATION "include/csound"
        FILES_MATCHING PATTERN "*.hpp")

    install(TARGETS libCsoundAC
        LIBRARY DESTINATION "${LIBRARY_INSTALL_DIR}"
        ARCHIVE DESTINATION "${LIBRARY_INSTALL_DIR}")

    if(SWIG_FOUND)
        message(STATUS "SWIG_USE_FILE: " ${SWIG_USE_FILE})
        include(${SWIG_USE_FILE})
        set(CMAKE_SWIG_OUTDIR ${BUILD_LIB_DIR})
        include_directories(${CMAKE_CURRENT_SOURCE_DIR})
        list(APPEND CMAKE_SWIG_FLAGS "-includeall" "-verbose")
    endif()

    if(BUILD_CSOUND_AC_PYTHON_INTERFACE)
        message(STATUS "Building CsoundAC Python interface.")
        include_directories(${Python3_INCLUDE_DIRS})
        # Had: 	    SWIG_FLAGS "-threads"
        set_source_files_properties(CsoundAC.i
            PROPERTIES CPLUSPLUS ON
            SWIG_MODULE_NAME CsoundAC)

        swig_add_library(CsoundAC 
            LANGUAGE python 
            SOURCES CsoundAC.i)
        swig_link_libraries(CsoundAC libCsoundAC csnd6 ${CSOUND_LIBRARIES} ${FLTK_LIBRARIES} ${OpenCV_LIBRARIES} ${LIBSNDFILE_LIBRARY} ${Python3_LIBRARIES})

        if(MUSICXML_LIBRARY)
            swig_link_libraries(CsoundAC libCsoundAC csnd6 ${CSOUND_LIBRARIES} ${FLTK_LIBRARIES} ${OpenCV_LIBRARIES} ${LIBSNDFILE_LIBRARY} ${MUSICXML_LIBRARY} ${Python3_LIBRARIES})
        endif()

        if(APPLE)
            set_target_properties(${SWIG_MODULE_CsoundAC_REAL_NAME} PROPERTIES
                LINK_FLAGS "-undefined dynamic_lookup")
        endif()

        target_include_directories(libCsoundAC SYSTEM PRIVATE . ${CSOUND_INCLUDE_PATH})
        set_target_properties(${SWIG_MODULE_CsoundAC_REAL_NAME} PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY "${BUILD_LIB_DIR}"
            LIBRARY_OUTPUT_DIRECTORY "${BUILD_LIB_DIR}"
            ARCHIVE_OUTPUT_DIRECTORY "${BUILD_LIB_DIR}")

        install(TARGETS ${SWIG_MODULE_CsoundAC_REAL_NAME}
            LIBRARY DESTINATION "${Python3_SITEARCH}"
            ARCHIVE DESTINATION "${Python3_SITEARCH}")
            install(FILES "${BUILD_LIB_DIR}/CsoundAC.py"
            DESTINATION ${Python3_SITEARCH})
    endif()

endif()
