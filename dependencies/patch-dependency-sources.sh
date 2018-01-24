#!/bin/bash
echo "Applying patches to dependency sources..."
patch -N VST_SDK/VST2_SDK/pluginterfaces/vst2.x/aeffect.h < patches/aeffect.h.patch
echo "Finished applying patches to dependency sources."