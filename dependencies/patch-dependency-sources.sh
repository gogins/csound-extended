#!/bin/bash
echo "Applying patches to dependency sources..."
patch -N -d cm2 -p1 < patches/cm2.patch
echo "Finished applying patches to dependency sources."
