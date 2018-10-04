#!/bin/bash
echo "Patching dependencies..."
patch -d ../cm2 -p1 --dry-run < ../patches/cm2.patch