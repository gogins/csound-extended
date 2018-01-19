# DEPENDENCIES

This directly and its scripts acquire, configure, and build all dependencies 
for csound-extended and, thus, for Csound itself.

## Acquiring Dependencies

Some dependencies are installed using the Debian package manager, some 
dependencies are acquired by cloning or pulling Git submodules, and some 
dependencies are directly downloaded and unpacked.

The dependencies that are not Debian packages should work from here in a 
cross-platform way.

# Building Dependencies

Some few of these dependencies are header file only, but all the others are 
configured and built here and so referenced from the Csound build, and other 
builds, in this project.
