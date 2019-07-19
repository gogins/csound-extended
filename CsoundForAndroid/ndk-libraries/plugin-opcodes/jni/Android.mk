LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE   := cs_date 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/date.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := ampmidid 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/ampmidid.cpp
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := arrayops 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/arrayops.cpp
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := beosc 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/emugens/beosc.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := buchla 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/buchla.c 
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := cellular 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/cellular.c
include $(BUILD_SHARED_LIBRARY)

# No Eigen in Android at this time. May add locally.

#include $(CLEAR_VARS)
#LOCAL_MODULE   := ChuaOscillator 
#LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
#LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
#LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
#LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
#LOCAL_LDFLAGS += -Wl,--export-dynamic
#LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/chua/ChuaOscillator.cpp
#include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := control 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/control.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := emugens 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/emugens/emugens.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := exciter 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/exciter.c 
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := fareygen 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/fareygen.c 
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := fractalnoise 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/tl/fractalnoise.cpp
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := framebuffer 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/framebuffer/Framebuffer.c $(CSOUND_SRC_ROOT)/Opcodes/framebuffer/OLABuffer.c $(CSOUND_SRC_ROOT)/Opcodes/framebuffer/OpcodeEntries.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := ftsamplebank 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/ftsamplebank.cpp
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := getftargs 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/getftargs.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := gft 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/gammatone.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := liveconv 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/liveconv.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := mixer 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/mixer.cpp
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := padsynth_gen 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/padsynth_gen.cpp
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := platerev 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/platerev.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := pvsgendy
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/pvsgendy.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := pvsops 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/pvsops.cpp
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := quadbezier 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/quadbezier.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := scugens 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/emugens/scugens.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := select 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/select.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := stackops 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/stackops.c
include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE   := urandom 
LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni
LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/urandom.c
include $(BUILD_SHARED_LIBRARY)

# Can't find libwebsockets.h.
#include $(CLEAR_VARS)
#LOCAL_MODULE   := WebSocketIO 
#LOCAL_C_INCLUDES := $(CSOUND_SRC_ROOT)/Engine $(CSOUND_SRC_ROOT)/H $(CSOUND_SRC_ROOT)/include $(CSOUND_SRC_ROOT)/ $(NDK_MODULE_PATH)/libsndfile-android/jni $(LOCAL_PATH)/../../../CsoundAndroid/jni $(CSOUND_SRC_ROOT)/Opcodes/websockets 
#LOCAL_CFLAGS := -D__BUILDING_LIBCSOUND -DENABLE_NEW_PARSER -DLINUX -DHAVE_DIRENT_H -DHAVE_FCNTL_H -DHAVE_UNISTD_H -DHAVE_STDINT_H -DHAVE_SYS_TIME_H -DHAVE_SYS_TYPES_H -DHAVE_TERMIOS_H 
#LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
#LOCAL_CPPFLAGS += -std=c++11 -pthread -frtti -fexceptions
#LOCAL_LDFLAGS += -Wl,--export-dynamic
#LOCAL_SRC_FILES := $(CSOUND_SRC_ROOT)/Opcodes/websockets/WebSocketOpcode.c
#include $(BUILD_SHARED_LIBRARY)


