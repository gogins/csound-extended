include(ExternalProject)

#add_subdirectory(csound)
#add_subdirectory(liblo/cmake)
add_subdirectory(libmusicxml/cmake)
#add_subdirectory(portaudio)

ExternalProject_Add(libsndfile
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libsndfile"
    BUILD_IN_SOURCE "1"
    CONFIGURE_COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/libsndfile/autogen.sh" COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/libsndfile/configure"
    BUILD_COMMAND "make"
    INSTALL_COMMAND "")

ExternalProject_Add(luajit
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/luajit-2.0"
    CONFIGURE_COMMAND ""
    BUILD_COMMAND make "BUILDMODE=static amalg"
    INSTALL_COMMAND ""
    BUILD_IN_SOURCE "1"
    LOG_DOWNLOAD 1 LOG_UPDATE 1 LOG_CONFIGURE 1 LOG_BUILD 1 LOG_INSTALL 1)

ExternalProject_Add(stk
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/stk"
    BUILD_IN_SOURCE "1"
    CONFIGURE_COMMAND "autoreconf" COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/stk/configure"
    BUILD_COMMAND "make"
    INSTALL_COMMAND "")
    
add_custom_target(csound-dependencies ALL
    DEPENDS liblo libmusicxml libsndfile stk luajit)

#add_custom_target(csound ALL
#    DEPENDS csound-dependencies)

