# Hello world example. Doesn't depend on any third party GUI framework.
# Tested with CEF Python v57.0+.
#
# ==== High DPI support on Windows ====
# To enable DPI awareness on Windows you have to either embed DPI aware manifest
# in your executable created with pyinstaller or change python.exe properties manually:
# Compatibility > High DPI scaling override > Application.
# Setting DPI awareness programmatically via a call to cef.DpiAware.EnableHighDpiSupport
# is problematic in Python, may not work and can cause display glitches.

import ctypes
from cefpython3 import cefpython as cef
import platform
import sys
import CsoundThreaded
import traceback
from ctypes import *

csound = CsoundThreaded.CsoundThread()
print(csound)

def main():
    cef.Initialize()
    check_versions()
    sys.excepthook = cef.ExceptHook  # To shutdown all CEF processes on error
    browser = cef.CreateBrowserSync(url="file:///home/mkg/csound-extended/playpen/cef_message.html",
                          window_title="Message from Another Planet")
    bindings = cef.JavascriptBindings(bindToFrames=True, bindToPopups=True)   
    bindings.SetObject("csound", csound)
    browser.SetJavascriptBindings(bindings)
    cef.MessageLoop()
    cef.Shutdown()

def check_versions():
    ver = cef.GetVersion()
    print("[hello_world.py] CEF Python {ver}".format(ver=ver["version"]))
    print("[hello_world.py] Chromium {ver}".format(ver=ver["chrome_version"]))
    print("[hello_world.py] CEF {ver}".format(ver=ver["cef_version"]))
    print("[hello_world.py] Python {ver} {arch}".format(
           ver=platform.python_version(),
           arch=platform.architecture()[0]))
    assert cef.__version__ >= "57.0", "CEF Python v57.0+ required to run this"

if __name__ == '__main__':
    main()
