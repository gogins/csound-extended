# DEPENDENCIES

This directory and its scripts acquire, configure, and build all dependencies 
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

# Csound

Csound itself is, of course, a dependency of csound-extended. However, Csound 
is not maintained from this repository. It is included as a submodule and 
built here. The reason it is a submodule is that CsoundForAndroid and, 
potentially, other projects must build Csound from sources.
