#!/bin/bash
export CLANGLIBS2="-lclangTooling -lclangFrontendTool -lclangFrontend -lclangDriver -lclangSerialization -lclangCodeGen -lclangParse -lclangSema -lclangStaticAnalyzerFrontend -lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore -lclangAnalysis -lclangARCMigrate -lclangRewrite -lclangRewriteFrontend -lclangEdit -lclangAST -lclangASTMatchers -lclangLex -lclangBasic -lclang"
g++ -v -Xlinker -g -O2 -Wl,-export-dynamic `llvm-config --cxxflags --ldflags` -I/usr/local/include -I/usr/local/include/csound clang_opcode.cpp $CLANGLIBS2 `llvm-config --libs --system-libs` -o main
ls -ll
./main hello.cxx