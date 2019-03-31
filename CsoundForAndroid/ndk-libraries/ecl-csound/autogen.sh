#! /bin/bash
cd ../ecl
aclocal && automake --add-missing && autoconf
cd ../ecl-csound

