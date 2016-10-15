#-------------------------------------------------
#
# Project created by QtCreator 2015-03-13T04:53:09
#
#-------------------------------------------------
message("B E G I N N I N G   Q M A K E   C O N F I G U R A T I O N . . .")
TARGET   = CHSound
TEMPLATE = app
# for R -multiline strings before Qt 5.6
CONFIG   += c++11
QT       += core gui widgets
QT       += network webenginewidgets webchannel
linux:CSOUND_HOME = /usr/local
win32-msvc2013:CSOUND_HOME = C:/Program_Files/Csound6_x64
SOURCES += main.cpp \
    csoundwebview.cpp \
    qcsound.cpp \
    mainwindow.cpp
HEADERS  += \
    csoundwebview.h \
    qcsound.h \
    mainwindow.h
FORMS    += \
    mainwindow.ui
message("All configuration is via CMake variable CSOUND_HOME.")
message("These point to installation directories, not source directories.")
linux:INCLUDEPATH += $$CSOUND_HOME/include/csound
linux:INCLUDEPATH += $$CSOUND_HOME/H
win32-msvc2013:INCLUDEPATH += $$CSOUND_HOME/include/csound
INCLUDEPATH += .
win32-msvc2013:CSOUND_LIB = $$CSOUND_HOME\\lib\\csound64.lib
linux:CSOUND_LIB = $$CSOUND_HOME/lib/libcsound64.so
LIBS += $$CSOUND_LIB
win32-msvc2013:LIBS += user32.lib
linux:DEFINES += NDEBUG
unix:QMAKE_CFLAGS += -Wno_unused_parameter
unix:QMAKE_CXXFLAGS += -std=gnu++11
win32-msvc2013:QMAKE_LFLAGS += /DEBUG /OPT:REF /OPT:ICF /INCREMENTAL:NO
message("CONFIG:         " $$CONFIG)
message("DEFINES:        " $$DEFINES)
message("INCLUDEPATH:    " $$INCLUDEPATH)
message("LIBS:           " $$LIBS)
message("QMAKE_CFLAGS:   " $$QMAKE_CFLAGS)
message("QMAKE_CXXFLAGS: " $$QMAKE_CXXFLAGS)

RESOURCES +=

DISTFILES += \
    04_Styles.csd \
    xanadu.csd \
    README.md
