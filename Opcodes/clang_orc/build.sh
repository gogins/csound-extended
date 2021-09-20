#!/bin/bash
export CLANGLIBS2="-lclangTooling -lclangFrontendTool -lclangFrontend -lclangDriver -lclangSerialization -lclangCodeGen -lclangParse -lclangSema -lclangStaticAnalyzerFrontend -lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore -lclangAnalysis -lclangARCMigrate -lclangRewrite -lclangRewriteFrontend -lclangEdit -lclangAST -lclangASTMatchers -lclangLex -lclangBasic -lclang"
g++ -v -Xlinker -g -O2 -fPIC -Wl,-export-dynamic -shared `llvm-config --cxxflags --ldflags` -I/usr/local/include -I/usr/local/include/csound clang_orc_opcode.cpp $CLANGLIBS2 `llvm-config --libs --system-libs` -o clang_orc_opcode.so
ls -ll
csound --opcode-lib="./clang_orc_opcode.so" clang_hello.csd
