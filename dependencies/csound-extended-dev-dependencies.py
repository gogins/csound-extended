import string
import sys

rdepends_output = '''
csound-extended-dev
  Depends: ecl (>= 16.1.3+ds)
  Depends: libc6 (>= 2.29)
  Depends: libfltk-images1.3
  Depends: libgcc-s1 (>= 4.0)
  Depends: libpython3.9 (>= 3.9.5)
  Depends: libstdc++6 (>= 9)
  Depends: python3
ecl
  Depends: gcc
  Depends: libatomic-ops-dev
  Depends: libc6 (>= 2.29)
  Depends: libffi-dev
  Depends: libffi7 (>= 3.3~20180313)
  Depends: libgc-dev
  Depends: libgc1c2 (>= 1:7.4.2)
  Depends: libgmp-dev
  Depends: libgmp10
  Depends: libncurses5-dev
gcc
  Depends: cpp (= 4:9.3.0-1ubuntu2)
  Depends: gcc-9 (>= 9.3.0-3~)
cpp
  Depends: cpp-9 (>= 9.3.0-3~)
cpp-9
  Depends: gcc-9-base (= 9.3.0-17ubuntu1~20.04)
  Depends: libc6 (>= 2.14)
  Depends: libgmp10 (>= 2:5.0.1~)
  Depends: libisl22 (>= 0.15)
  Depends: libmpc3
  Depends: libmpfr6 (>= 3.1.3)
  Depends: zlib1g (>= 1:1.1.4)
gcc-9-base
libc6
  Depends: libcrypt1 (>= 1:4.4.10-10ubuntu4)
  Depends: libgcc-s1
libcrypt1
  Depends: libc6 (>= 2.25)
libgcc-s1
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.14)
gcc-10-base
libgmp10
  Depends: libc6 (>= 2.14)
libisl22
  Depends: libc6 (>= 2.14)
  Depends: libgmp10
libmpc3
  Depends: libc6 (>= 2.4)
  Depends: libgmp10
  Depends: libmpfr6 (>= 4.0.0)
libmpfr6
  Depends: libc6 (>= 2.14)
  Depends: libgmp10
zlib1g
  Depends: libc6 (>= 2.14)
gcc-9
  Depends: binutils (>= 2.34)
  Depends: cpp-9 (= 9.3.0-17ubuntu1~20.04)
  Depends: gcc-9-base (= 9.3.0-17ubuntu1~20.04)
  Depends: libc6 (>= 2.15)
  Depends: libcc1-0 (>= 9.3.0-17ubuntu1~20.04)
  Depends: libgcc-9-dev (= 9.3.0-17ubuntu1~20.04)
  Depends: libgcc-s1 (>= 3.0)
  Depends: libgmp10 (>= 2:5.0.1~)
  Depends: libisl22 (>= 0.15)
  Depends: libmpc3
  Depends: libmpfr6 (>= 3.1.3)
  Depends: libstdc++6 (>= 5)
  Depends: zlib1g (>= 1:1.1.4)
binutils
  Depends: binutils-common (= 2.34-6ubuntu1.1)
  Depends: binutils-x86-64-linux-gnu (= 2.34-6ubuntu1.1)
  Depends: libbinutils (= 2.34-6ubuntu1.1)
binutils-common
binutils-x86-64-linux-gnu
  Depends: binutils-common (= 2.34-6ubuntu1.1)
  Depends: libbinutils (= 2.34-6ubuntu1.1)
  Depends: libc6 (>= 2.27)
  Depends: libctf-nobfd0 (>= 2.33.50)
  Depends: libctf0 (>= 2.33.50)
  Depends: libgcc-s1 (>= 3.0)
  Depends: libstdc++6 (>= 5.2)
  Depends: zlib1g (>= 1:1.1.4)
libbinutils
  Depends: binutils-common (= 2.34-6ubuntu1.1)
  Depends: libc6 (>= 2.14)
  Depends: zlib1g (>= 1:1.2.0)
libctf-nobfd0
  Depends: libc6 (>= 2.14)
  Depends: zlib1g (>= 1:1.2.0)
libctf0
  Depends: libbinutils (= 2.34-6ubuntu1.1)
  Depends: libc6 (>= 2.14)
  Depends: zlib1g (>= 1:1.2.0)
libstdc++6
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.18)
  Depends: libgcc-s1 (>= 4.2)
libcc1-0
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.15)
  Depends: libgcc-s1 (>= 3.0)
  Depends: libstdc++6 (>= 5.2)
libgcc-9-dev
  Depends: gcc-9-base (= 9.3.0-17ubuntu1~20.04)
  Depends: libasan5 (>= 9.3.0-17ubuntu1~20.04)
  Depends: libatomic1 (>= 9.3.0-17ubuntu1~20.04)
  Depends: libgcc-s1 (>= 9.3.0-17ubuntu1~20.04)
  Depends: libgomp1 (>= 9.3.0-17ubuntu1~20.04)
  Depends: libitm1 (>= 9.3.0-17ubuntu1~20.04)
  Depends: liblsan0 (>= 9.3.0-17ubuntu1~20.04)
  Depends: libquadmath0 (>= 9.3.0-17ubuntu1~20.04)
  Depends: libtsan0 (>= 9.3.0-17ubuntu1~20.04)
  Depends: libubsan1 (>= 9.3.0-17ubuntu1~20.04)
libasan5
  Depends: gcc-9-base (= 9.3.0-17ubuntu1~20.04)
  Depends: libc6 (>= 2.23)
  Depends: libgcc-s1
libatomic1
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.14)
libgomp1
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.17)
libitm1
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.14)
liblsan0
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.17)
  Depends: libgcc-s1 (>= 3.3)
libquadmath0
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.23)
libtsan0
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.23)
  Depends: libgcc-s1 (>= 3.3)
libubsan1
  Depends: gcc-10-base (= 10.2.0-5ubuntu1~20.04)
  Depends: libc6 (>= 2.17)
  Depends: libgcc-s1 (>= 3.3)
  Depends: libstdc++6 (>= 4.1.1)
libatomic-ops-dev
libffi-dev
  Depends: libffi7 (= 3.3-4)
libffi7
  Depends: libc6 (>= 2.14)
libgc-dev
  Depends: libc-dev
  Depends: libgc1c2 (= 1:7.6.4-0.4ubuntu1)
libc-dev
libgc1c2
  Depends: libc6 (>= 2.14)
  Depends: libgcc-s1 (>= 3.0)
  Depends: libstdc++6 (>= 4.1.1)
libgmp-dev
  Depends: libgmp10 (= 2:6.2.0+dfsg-4)
  Depends: libgmpxx4ldbl (= 2:6.2.0+dfsg-4)
libgmpxx4ldbl
  Depends: libc6 (>= 2.4)
  Depends: libgcc-s1 (>= 3.0)
  Depends: libgmp10
  Depends: libstdc++6 (>= 5.2)
libncurses5-dev
  Depends: libncurses-dev (= 6.2-0ubuntu2)
  Depends: libtinfo6 (= 6.2-0ubuntu2)
libncurses-dev
  Depends: libc-dev
  Depends: libc6-dev
  Depends: libncurses6 (= 6.2-0ubuntu2)
  Depends: libncursesw6 (= 6.2-0ubuntu2)
  Depends: libtinfo6 (= 6.2-0ubuntu2)
  Depends: ncurses-bin (>= 6.0+20151017)
libc6-dev
  Depends: libc-dev-bin (= 2.31-0ubuntu9.3)
  Depends: libc6 (= 2.31-0ubuntu9.3)
  Depends: libcrypt-dev
  Depends: linux-libc-dev
libc-dev-bin
  Depends: libc6 (<< 2.32)
libcrypt-dev
  Depends: libcrypt1 (= 1:4.4.10-10ubuntu4)
linux-libc-dev
libncurses6
  Depends: libc6 (>= 2.14)
  Depends: libtinfo6 (= 6.2-0ubuntu2)
libtinfo6
  Depends: libc6 (>= 2.16)
libncursesw6
  Depends: libc6 (>= 2.14)
  Depends: libtinfo6 (= 6.2-0ubuntu2)
ncurses-bin
  PreDepends: libc6 (>= 2.14)
  PreDepends: libtinfo6 (>= 6.2)
libfltk-images1.3
  Depends: libc6 (>= 2.14)
  Depends: libfltk1.3 (= 1.3.4-10build1)
  Depends: libgcc-s1 (>= 3.0)
  Depends: libjpeg8 (>= 8c)
  Depends: libpng16-16 (>= 1.6.2-1)
  Depends: libstdc++6 (>= 5)
libfltk1.3
  Depends: libc6 (>= 2.29)
  Depends: libfontconfig1 (>= 2.12.6)
  Depends: libgcc-s1 (>= 3.0)
  Depends: libstdc++6 (>= 5)
  Depends: libx11-6
  Depends: libxcursor1 (>> 1.1.2)
  Depends: libxext6
  Depends: libxfixes3
  Depends: libxft2 (>> 2.1.1)
  Depends: libxinerama1
  Depends: libxrender1
libfontconfig1
  Depends: fontconfig-config (>= 2.13.1-2ubuntu3)
  Depends: libc6 (>= 2.14)
  Depends: libexpat1 (>= 2.0.1)
  Depends: libfreetype6 (>= 2.9.1)
  Depends: libuuid1 (>= 2.16)
fontconfig-config
  Depends: fonts-dejavu-core
  Depends: fonts-freefont
  Depends: fonts-liberation
  Depends: ttf-bitstream-vera
  Depends: ucf (>= 0.29)
fonts-dejavu-core
fonts-freefont
fonts-liberation
ttf-bitstream-vera
ucf
  Depends: coreutils (>= 5.91)
  Depends: debconf (>= 1.5.19)
  Depends: sensible-utils
coreutils
  PreDepends: libacl1 (>= 2.2.23)
  PreDepends: libattr1 (>= 1:2.4.44)
  PreDepends: libc6 (>= 2.28)
  PreDepends: libselinux1 (>= 2.1.13)
libacl1
  Depends: libc6 (>= 2.14)
libattr1
  Depends: libc6 (>= 2.4)
libselinux1
  Depends: libc6 (>= 2.30)
  Depends: libpcre2-8-0 (>= 10.22)
libpcre2-8-0
  Depends: libc6 (>= 2.14)
debconf
  PreDepends: perl-base (>= 5.20.1-3~)
perl-base
  PreDepends: dpkg (>= 1.17.17)
  PreDepends: libc6 (>= 2.29)
  PreDepends: libcrypt1 (>= 1:4.1.0)
dpkg
  Depends: tar (>= 1.28-1)
  PreDepends: libbz2-1.0
  PreDepends: libc6 (>= 2.15)
  PreDepends: liblzma5 (>= 5.2.2)
  PreDepends: libselinux1 (>= 2.3)
  PreDepends: libzstd1 (>= 1.3.2)
  PreDepends: zlib1g (>= 1:1.1.4)
tar
  PreDepends: libacl1 (>= 2.2.23)
  PreDepends: libc6 (>= 2.17)
  PreDepends: libselinux1 (>= 1.32)
libbz2-1.0
  Depends: libc6 (>= 2.4)
liblzma5
  Depends: libc6 (>= 2.17)
libzstd1
  Depends: libc6 (>= 2.14)
sensible-utils
libexpat1
  Depends: libc6 (>= 2.25)
libfreetype6
  Depends: libc6 (>= 2.14)
  Depends: libpng16-16 (>= 1.6.2-1)
  Depends: zlib1g (>= 1:1.1.4)
libpng16-16
  Depends: libc6 (>= 2.29)
  Depends: zlib1g (>= 1:1.2.11)
libuuid1
  Depends: libc6 (>= 2.25)
libx11-6
  Depends: libc6 (>= 2.26)
  Depends: libx11-data
  Depends: libxcb1 (>= 1.11.1)
libx11-data
libxcb1
  Depends: libc6 (>= 2.14)
  Depends: libxau6
  Depends: libxdmcp6
libxau6
  Depends: libc6 (>= 2.4)
libxdmcp6
  Depends: libbsd0 (>= 0.2.0)
  Depends: libc6 (>= 2.4)
libbsd0
  Depends: libc6 (>= 2.25)
libxcursor1
  Depends: libc6 (>= 2.4)
  Depends: libx11-6 (>= 2:1.4.99.1)
  Depends: libxfixes3
  Depends: libxrender1
libxfixes3
  Depends: libc6 (>= 2.14)
  Depends: libx11-6 (>= 2:1.6.0)
libxrender1
  Depends: libc6 (>= 2.14)
  Depends: libx11-6 (>= 2:1.6.0)
libxext6
  Depends: libc6 (>= 2.14)
  Depends: libx11-6 (>= 2:1.6.0)
libxft2
  Depends: libc6 (>= 2.14)
  Depends: libfontconfig1 (>= 2.12.6)
  Depends: libfreetype6 (>= 2.3.5)
  Depends: libx11-6
  Depends: libxrender1
libxinerama1
  Depends: libc6 (>= 2.4)
  Depends: libx11-6 (>= 2:1.6.0)
  Depends: libxext6
libjpeg8
  Depends: libjpeg-turbo8 (>= 1.1.90+svn722-1ubuntu6)
libjpeg-turbo8
  Depends: libc6 (>= 2.14)
libpython3.9
  Depends: libc6 (>= 2.29)
  Depends: libexpat1 (>= 2.1~beta3)
  Depends: libpython3.9-stdlib (= 3.9.5-1+focal1)
  Depends: zlib1g (>= 1:1.2.0)
libpython3.9-stdlib
  Depends: libbz2-1.0
  Depends: libc6 (>= 2.28)
  Depends: libcrypt1 (>= 1:4.1.0)
  Depends: libdb5.3
  Depends: libffi7 (>= 3.3~20180313)
  Depends: liblzma5 (>= 5.1.1alpha+20120614)
  Depends: libmpdec2
  Depends: libncursesw6 (>= 6)
  Depends: libpython3.9-minimal (= 3.9.5-1+focal1)
  Depends: libreadline8 (>= 7.0~beta)
  Depends: libsqlite3-0 (>= 3.7.15)
  Depends: libtinfo6 (>= 6)
  Depends: libuuid1 (>= 2.20.1)
  Depends: mime-support
  Depends: tzdata
libdb5.3
  Depends: libc6 (>= 2.17)
libmpdec2
  Depends: libc6 (>= 2.14)
libpython3.9-minimal
  Depends: libc6 (>= 2.4)
  Depends: libssl1.1 (>= 1.1.1)
libssl1.1
  Depends: debconf (>= 0.5)
  Depends: debconf-2.0
  Depends: libc6 (>= 2.25)
debconf-2.0
libreadline8
  Depends: libc6 (>= 2.15)
  Depends: libtinfo6 (>= 6)
  Depends: readline-common
readline-common
  Depends: dpkg (>= 1.15.4)
  Depends: install-info
install-info
  Depends: libc6 (>= 2.14)
  PreDepends: dpkg (>= 1.16.1)
libsqlite3-0
  Depends: libc6 (>= 2.29)
mime-support
tzdata
  Depends: debconf (>= 0.5)
  Depends: debconf-2.0
python3
  Depends: libpython3-stdlib (= 3.8.2-0ubuntu2)
  Depends: python3.8 (>= 3.8.2-1~)
  PreDepends: python3-minimal (= 3.8.2-0ubuntu2)
libpython3-stdlib
  Depends: libpython3.8-stdlib (>= 3.8.2-1~)
libpython3.8-stdlib
  Depends: libbz2-1.0
  Depends: libc6 (>= 2.28)
  Depends: libcrypt1 (>= 1:4.1.0)
  Depends: libdb5.3
  Depends: libffi7 (>= 3.3~20180313)
  Depends: liblzma5 (>= 5.1.1alpha+20120614)
  Depends: libmpdec2
  Depends: libncursesw6 (>= 6)
  Depends: libpython3.8-minimal (= 3.8.5-1~20.04.2)
  Depends: libreadline8 (>= 7.0~beta)
  Depends: libsqlite3-0 (>= 3.7.15)
  Depends: libtinfo6 (>= 6)
  Depends: libuuid1 (>= 2.20.1)
  Depends: mime-support
libpython3.8-minimal
  Depends: libc6 (>= 2.4)
  Depends: libssl1.1 (>= 1.1.1)
python3.8
  Depends: libpython3.8-stdlib (= 3.8.5-1~20.04.2)
  Depends: mime-support
  Depends: python3.8-minimal (= 3.8.5-1~20.04.2)
python3.8-minimal
  Depends: libexpat1 (>= 2.1~beta3)
  Depends: libpython3.8-minimal (= 3.8.5-1~20.04.2)
  Depends: zlib1g (>= 1:1.2.0)
  PreDepends: libc6 (>= 2.29)
python3-minimal
  Depends: dpkg (>= 1.13.20)
  PreDepends: python3.8-minimal (>= 3.8.2-1~)
'''

lines = rdepends_output.split("\n")
packages = set()
for line in lines:
    if len(line) > 1:
        line = line.strip()
        line = line.replace("Depends: ", "")
        line = line.split(" ")[0]
        packages.add(line)
        #print(line)
    
print("Recursive dependencies of csound-extended-dev:")
count = 0
for package in sorted(packages):
    count = count + 1
    print("{:4d} {}".format(count, package))
    
packages = set()
for line in lines:
    line = line.strip()
    if line.find("Depends: ") < 0:
        line = line.split(" ")[0]
        packages.add(line)
        #print(line)
print()
print("Primary dependencies of csound-extended-dev:")
count = 0
for package in sorted(packages):
    count = count + 1
    print("{:4d} {}".format(count, package))
    

