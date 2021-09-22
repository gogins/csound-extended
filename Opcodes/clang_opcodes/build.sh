#!/bin/bash
export CLANGLIBS2="-lclangTooling -lclangFrontendTool -lclangFrontend -lclangDriver -lclangSerialization -lclangCodeGen -lclangParse -lclangSema -lclangStaticAnalyzerFrontend -lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore -lclangAnalysis -lclangARCMigrate -lclangRewrite -lclangRewriteFrontend -lclangEdit -lclangAST -lclangASTMatchers -lclangLex -lclangBasic -lclang"
clang++-13 -v -Xlinker -g -O2 -fPIC -Wl,-export-dynamic -shared `llvm-config-13 --cxxflags --ldflags` -I/usr/local/include -I/usr/local/include/csound clang_opcodes.cpp $CLANGLIBS2 `llvm-config-13 --libs --system-libs` -o clang_opcodes.so
ls -ll
csound --opcode-lib="./clang_opcodes.so" clang_hello.csd
