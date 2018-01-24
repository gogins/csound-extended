#!/bin/bash
echo "Applying patches to dependency sources..."
patch VST_SDK/VST2_SDK/pluginterfaces/vst2.x/aeffect.h < patches/aeffect.h.patch
edho "Finished applying patches to dependency sources."