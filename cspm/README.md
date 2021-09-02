# cspm

cspm - Package manager for Csound.
 
## Description

The Csound package manager (cspm) enables both contributing new extensions as 
prebuilt packages to a central repository, and downloading and installing 
packages from such repositories.

Extension packages may include opcode plugins, GEN plugins, other loadable 
modules, and Csound language files such as orchestras, instrument definitions, 
or examples (along with supporting resources).

## Syntax
```
csound -U cspm command [options...] 
```
## Initialization

The commands for cspm are:

*contribute* package_directory [repository_url] - upload a contributed package 
to the contributor's account in a package repository. The contribution must be 
staged as a directory in a standard format containing a package manifest and 
pre-built package contents (see below).

The contributor must have a GitHub account. If the package already exists, 
its contents are updated.

*list* [repositories...] - list packages in the package repository(s).

*search* "text..." [repositories...] - search for the quoted text in the 
package repository(s) manifests, and print matching packages/repositories.

*information* package [repositories...] - print the package's manifest.

*install* package [repository] - install a package.

*dependencies* package - list dependencies of a package.

*remove* package - remove a package.

## Configuration

The package manager reads a list of package repository URLs from ~/.cspm.ini. 
These are in addition to the default package repository, which is hard-coded.

This configuration file may also contain a list of package architectures to be 
installed, specified as VCPKG triple names, e.g. `x64-linux`. These are in 
addition to the default package architecture, which is inferred from the 
system.

## Package Specification

### Package Names

Package names must be UTF-encoded without spaces. Underlines should be used 
to demarcate words. Package names must not identify the target architecture; 
different architectures are all stored in the same package file.

### Package Directory Structure

Packages must be staged in a directory with the same name as the package.
In the root directory, there must be a `manifest.json` file that contains 
metadata for the package (format specified below). There must also be a 
README.md file that provides Csound-style help for the package, and a 
LICENSE.md file that contains a software license which is compatible with 
Csound's license (LGPLv2.1). NOTE WELL: Binaries must be pre-built.

The package directory then contains subdirectories structured by installation 
target and, where necessary, qualified by target runtime architecture 
specified by triplet name. Some installation targets are standard Csound 
directories, and some identify "inner packages" such as Debian packages 
or Windows installers. 
```
manifest.json - Metadata in JSON format (required).
README.md - Csound-style manual page in markdown format (required).
LICENSE.md - Csound-compatible open source software license in markdown 
format (required).
SFDIR/ - Soundfile resources.
SSDIR/ - Sample resources.
SADIR/ - Analysis file resources.
INCDIR/ - Files containing Csound code (.csd, .orc., .sco, .inc, etc.).
OPCODE6DIR64/triplet/ - Loadable modules.
MFDIR/ - MIDI files.
deb/triplet/ - Debian packages.
exe/triplet/ - Windows installers.
uwp/ - Universal Windows package.
dmg/triplet/ - MacOS installers.
apk/triplet/ - Android application packages.
npm/triplet/ - NPM packages.
NODE/triplet/ - Node.js and NW.js native addons.
```
### Manifest File Format

Manifest files for Csound packages are JSON files in the following format:

```
{
  name: "package_name";
  version: "1.2.3.alpha";
  author: "author name <email>";
  license: "standard license name";
  description: "What is this extension for?";
  source: "where is the source code repository?";
  tags: "comma-delimited list of searchable tags.";
  dependencies: {
    "triplet_1:" [list of dependencies];
    "triplet_2": [list of dependencies];
    ...
  }; 
};
```
The manifest must not list the contents of the package, that is inferred from 
the package archive.

## Examples

List all packages in all configured repositories:
```
csound -U cspm list
```
Contribute an extension package:
```
csound -U cspm contribute csound-vst3-opcodes
```
Install an extension package:
```
csound -U cspm install csound-vst3-opcodes
```

## Credits

Auothor: Michael Gogins
