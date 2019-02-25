#-------------------------------------------------
#
# Project created by QtCreator 2015-03-13T04:53:09
#
#-------------------------------------------------
message("B E G I N N I N G   Q M A K E   C O N F I G U R A T I O N . . .")
TARGET   = CsoundHtml5
TEMPLATE = app
# for R -multiline strings before Qt 5.6
CONFIG   += c++11
QT       += core gui widgets
QT       += network webenginewidgets webchannel
linux:CSOUND_HOME = /usr
win32-msvc2015:CSOUND_HOME = C:/Program_Files/Csound_x64
SOURCES += main.cpp \
    csoundwebview.cpp \
    qcsound.cpp \
    mainwindow.cpp \
    finddialog.cpp \
    findform.cpp \
    findreplacedialog.cpp \
    findreplaceform.cpp
HEADERS  += \
    csoundwebview.h \
    qcsound.h \
    mainwindow.h \
    finddialog.h \
    findform.h \
    findreplacedialog.h \
    findreplaceform.h \
    findreplace_global.h
FORMS    += \
    mainwindow.ui \
    findreplacedialog.ui \
    findreplaceform.ui
RC_ICONS = cs.ico
# For dynamic language support.
linux {
    CONFIG += link_pkgconfig
    PKGCONFIG += python3
    PKGCONFIG += luajit
}
message("All Csound configuration is via CMake variable CSOUND_HOME.")
message("These point to installation directories, not source directories.")
linux:INCLUDEPATH += $$CSOUND_HOME/include/csound
linux:INCLUDEPATH += $$CSOUND_HOME/H
linux:INCLUDEPATH += $$CSOUND_HOME/H
win32-msvc2015:INCLUDEPATH += $$CSOUND_HOME/include/csound
INCLUDEPATH += .
INCLUDEPATH += ../CsoundAC
win32-msvc2015:CSOUND_LIB = $$CSOUND_HOME\\lib\\csound64.lib
linux:CSOUND_LIB = $$CSOUND_HOME/lib/libcsound64.so
LIBS += $$CSOUND_LIB
linux:LIBS += /usr/lib/libecl.so
win32-msvc2015:LIBS += user32.lib
linux:DEFINES += NDEBUG
unix:QMAKE_CFLAGS += -Wno_unused_parameter
unix:QMAKE_CXXFLAGS += -std=gnu++11
win32-msvc2015:QMAKE_LFLAGS += /DEBUG /OPT:REF /OPT:ICF /INCREMENTAL:NO
message("CONFIG:         " $$CONFIG)
message("DEFINES:        " $$DEFINES)
message("INCLUDEPATH:    " $$INCLUDEPATH)
message("LIBS:           " $$LIBS)
message("QMAKE_CFLAGS:   " $$QMAKE_CFLAGS)
message("QMAKE_CXXFLAGS: " $$QMAKE_CXXFLAGS)
#for(var, $$list($$enumerate_vars())) {
#    message($$var "=" $$eval($$var))
#}

DISTFILES += \
    04_Styles.csd \
    xanadu.csd \
    README.md
