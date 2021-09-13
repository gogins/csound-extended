#!/bin/bash
export CLANGLIBS2="-lclangTooling -lclangFrontendTool -lclangFrontend -lclangDriver -lclangSerialization -lclangCodeGen -lclangParse -lclangSema -lclangStaticAnalyzerFrontend -lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore -lclangAnalysis -lclangARCMigrate -lclangRewrite -lclangRewriteFrontend -lclangEdit -lclangAST -lclangASTMatchers -lclangLex -lclangBasic -lclang"
clang++ -v -Xlinker --export-dynamic `llvm-config --cxxflags --ldflags` -I/usr/local/include -I/usr/local/include/csound clang_gen.cpp $CLANGLIBS2 `llvm-config --libs --system-libs` -g -O2 -o main
./main hello.cxx