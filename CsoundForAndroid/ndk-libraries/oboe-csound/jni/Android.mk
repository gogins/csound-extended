LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := oboe

LOCAL_C_INCLUDES := $(NDK_MODULE_PATH)/oboe/src $(NDK_MODULE_PATH)/oboe/include 
LOCAL_CFLAGS := -Ofast 
LOCAL_CPPFLAGS :=$(LOCAL_CFLAGS)
LOCAL_CPPFLAGS += -std=c++11 -Wall -frtti -fexceptions
LOCAL_LDFLAGS += -Wl,--export-dynamic
LOCAL_LDLIBS := -L$(SYSROOT)/usr/lib -llog -lOpenSLES

LOCAL_SRC_FILES := \
    $(NDK_MODULE_PATH)/oboe/src/aaudio/AAudioLoader.cpp \
    $(NDK_MODULE_PATH)/oboe/src/aaudio/AudioStreamAAudio.cpp \
    $(NDK_MODULE_PATH)/oboe/src/fifo/FifoBuffer.cpp \
    $(NDK_MODULE_PATH)/oboe/src/fifo/FifoControllerIndirect.cpp \
    $(NDK_MODULE_PATH)/oboe/src/fifo/FifoControllerBase.cpp \
    $(NDK_MODULE_PATH)/oboe/src/fifo/FifoController.cpp \
    $(NDK_MODULE_PATH)/oboe/src/common/AudioStream.cpp \
    $(NDK_MODULE_PATH)/oboe/src/common/AudioStreamBuilder.cpp \
    $(NDK_MODULE_PATH)/oboe/src/common/Utilities.cpp \
    $(NDK_MODULE_PATH)/oboe/src/common/LatencyTuner.cpp \
    $(NDK_MODULE_PATH)/oboe/src/common/Version.cpp \
    $(NDK_MODULE_PATH)/oboe/src/common/Trace.cpp \
    $(NDK_MODULE_PATH)/oboe/src/common/StabilizedCallback.cpp \
    $(NDK_MODULE_PATH)/oboe/src/opensles/AudioInputStreamOpenSLES.cpp \
    $(NDK_MODULE_PATH)/oboe/src/opensles/AudioOutputStreamOpenSLES.cpp \
    $(NDK_MODULE_PATH)/oboe/src/opensles/AudioStreamBuffered.cpp \
    $(NDK_MODULE_PATH)/oboe/src/opensles/AudioStreamOpenSLES.cpp \
    $(NDK_MODULE_PATH)/oboe/src/opensles/EngineOpenSLES.cpp \
    $(NDK_MODULE_PATH)/oboe/src/opensles/OpenSLESUtilities.cpp \
    $(NDK_MODULE_PATH)/oboe/src/opensles/OutputMixerOpenSLES.cpp

include $(BUILD_SHARED_LIBRARY)
