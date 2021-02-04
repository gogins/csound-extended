# nudruz

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

This archive of Lisp code from [Drew Krause](http://www.drew-krause.com/) is 
hosted here with his permission. Drew's code is licensed under the terms of 
the GNU Lesser General Public License. The code has been edited to be more 
usable with different implementations of Common Lisp, and with the current 
version of Csound. In particular, there is now a nudruz.asd system 
definition that will load all dependencies of nudruz, including Common 
Music verison 2, Fomus, and others. This has been tested with Steel Bank 
Common Lisp.

It is a goal of this project for this code to run in OpenMusic, but at this 
time, the free version of OpenMusic uses a release of LispWorks that does not 
compile Lisp code, but only loads it. Therefore, ASDF cannot be used in OpenMusic. 
I have not been able to work around this limitation.

Please log any bug reports or requests for enhancements at 
https://github.com/gogins/csound-extended/issues.

### Documentation

[Drew's talk](http://files.meetup.com/1748515/Drew%20Krause%20slides.pdf) 
provides an overview and demonstration of the capabilities of nudruz. For more 
information, read the code.

### Changes

1.  All code has been brought into the Common Music package (that is, of 
course, the Lisp version of Common Music, version 2, available from 
https://github.com/andersvi/cm2). The reason for doing this is that some 
functions already were dependent upon Common Music code, and changing this 
would have required making many more changes. As a result, when you load this 
code, the effect is to vastly extend the power of Common Music.

2. Residual uses of Scheme syntax and of CLisp-compatible but Steel Bank 
Common Lisp-incompatible syntax have been made conditional, and `(define` 
has been replaced wth `(defparameter` or `(defun`.

3. Csound classes in nudruz have been replaced with the Csound FFI facility, 
because the use of Csound is no longer supported in the Lisp branch of Common 
Music (cm2).

4. In a few cases, redefined symbols have been disambiguated.

5. I have created a nudruz.asd system definition. It tries to include 
everything except for examples and files containing unfinished or unworkable 
code. 

6. I have had to re-implement some functions that were missing from Common 
Music, or sourced from CLOCC which is hard to install, in the fillins.lisp 
file.

See https://github.com/gogins/csound-extended/commits/develop for the commit 
log.

## Installation

In order to load nudruz.asd you must first install a number of its 
dependencies, listed in nudruz.asd. Some of these can be installed as Linux 
packages, some must be installed by cloning Git repositories, some must be 
installed using Quicklisp, and some must be installed by downloading archives. 
In all cases except for Linux packages, you must create a symbolic link 
to your ~/.local/share/common-lisp/source/ directory. For example, I have 
satisfied all the non-Linux package Common Lisp dependencies of nudruz as 
follows:
```
mkg@xenakis:~/.local/share/common-lisp/source$ ls -ll ~/.local/share/common-lisp/source/
total 64
lrwxrwxrwx 1 mkg mkg 68 Feb  3 19:14 alexandria -> /home/mkg/quicklisp/dists/quicklisp/software/alexandria-20191227-git
lrwxrwxrwx 1 mkg mkg 64 Feb  3 19:17 babel -> /home/mkg/quicklisp/dists/quicklisp/software/babel-20191130-git/
lrwxrwxrwx 1 mkg mkg 69 Feb  3 19:18 bordeaux-threads -> /home/mkg/quicklisp/dists/quicklisp/software/bordeaux-threads-v0.8.7/
lrwxrwxrwx 1 mkg mkg 57 Feb  3 19:20 cffi -> /home/mkg/quicklisp/dists/quicklisp/software/cffi_0.20.1/
lrwxrwxrwx 1 mkg mkg 68 Feb  2 10:56 cl-heredoc -> /home/mkg/quicklisp/dists/quicklisp/software/cl-heredoc-20101006-git
lrwxrwxrwx 1 mkg mkg 21 Feb  3 19:50 clm -> /home/mkg/lisp/clm-5/
lrwxrwxrwx 1 mkg mkg 17 Feb 14  2020 clmath -> /home/mkg/CLMath/
lrwxrwxrwx 1 mkg mkg 42 Feb 15  2020 cm -> /home/mkg/csound-extended/dependencies/cm2
lrwxrwxrwx 1 mkg mkg 43 Feb 14  2020 cm2 -> /home/mkg/csound-extended/dependencies/cm2/
lrwxrwxrwx 1 mkg mkg 48 Feb 15  2020 cm-fomus -> /home/mkg/csound-extended/dependencies/cm-fomus/
lrwxrwxrwx 1 mkg mkg 18 Feb  3 19:49 cmn -> /home/mkg/lisp/cmn
lrwxrwxrwx 1 mkg mkg 57 Feb 14  2020 csound -> /home/mkg/csound-extended/dependencies/csound/interfaces/
lrwxrwxrwx 1 mkg mkg 45 Feb 14  2020 fomus -> /home/mkg/csound-extended/dependencies/fomus/
lrwxrwxrwx 1 mkg mkg 41 Feb 14  2020 nudruz -> /home/mkg/csound-extended/nudruz/sources/
lrwxrwxrwx 1 mkg mkg 67 Feb  3 19:20 screamer -> /home/mkg/quicklisp/dists/quicklisp/software/screamer-20190710-git/
lrwxrwxrwx 1 mkg mkg 75 Feb  3 19:18 trivial-features -> /home/mkg/quicklisp/dists/quicklisp/software/trivial-features-20190710-git/
```

In general, you should use Quicklisp rather than ASDF to load packages and 
systems in Common Lisp, because Quicklisp can load ASDF systems, but ASDF cannot 
download ASDF systems.

When you first run a Lisp piece using the nudruz package, do not give up if 
there are errors, e.g. in loading or compiling Fomus. Just accept the error 
and continue...

The nudruz code and examples run in the package `:cm` although it should be 
possible to rewrite them to run them in other packages.

The local `example-csd.lisp` and `example-orc.lisp` files contain a canned Csound 
orchestra that is used to play the examples. As these files are not part of the 
nudruz system, they are run as source code and therefore edits will be playable 
immediately.
