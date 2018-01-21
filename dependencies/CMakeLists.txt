include(ExternalProject)

ExternalProject_Add(libsndfile
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libsndfile"
    CONFIGURE_COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/libsndfile/autogen.sh" COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/libsndfile/configure"
    BUILD_COMMAND "make"
    INSTALL_COMMAND "")

ExternalProject_Add(luajit
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/luajit-2.0"
    BUILD_COMMAND "make"
    INSTALL_COMMAND)


