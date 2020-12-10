/*
    TestPlugin.cpp

    Copyright (C) 2020 Michael Gogins

    This file is part of Csound.

    The Csound Library is free software; you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    Csound is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Csound; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
    02110-1301 USA
    
    BUILDING
    
    First make sure you have built the Gamma library with -fPIC.
    
    Then build for your build of Csound with C++17 and -lGamma -olibMVerb.so    
 */
#include "OpcodeBase.hpp"
#include <string>
#include <map>

class TestPlugin;

static std::multimap<std::string, TestPlugin*> &instances_for_names() {
    static std::multimap<std::string, TestPlugin*> instances_for_names_;
    return instances_for_names_;
}

class TestPlugin  : public csound::OpcodeBase<TestPlugin>
{
public:
    // Outputs.
    MYFLT *instance_count;
    // Inputs.
    STRINGDAT *name_;
    // State.
    std::string name;
    int init(CSOUND *csound)
    {
        name = name_->data;
        instances_for_names().insert({name, this});
        *instance_count = instances_for_names().size();
        return OK;
    }
    int kontrol(CSOUND *csound)
    {
        *instance_count = instances_for_names().size();
        return OK;
    }
};

extern "C" {

    PUBLIC int csoundModuleCreate(CSOUND *csound)
    {
        std::fprintf(stderr, "TestPlugin:csoundModuleCreate.\n");
        return 0;
    }

    PUBLIC int csoundModuleInit(CSOUND *csound)
    {
        std::fprintf(stderr, "TestPlugin:csoundModuleInit.\n");
        int status = csound->AppendOpcode(csound,
                                          (char*)"TestPlugin",
                                          sizeof(TestPlugin),
                                          0,
                                          3,
                                          (char*)"i",
                                          (char*)"S",
                                          (int (*)(CSOUND*,void*)) TestPlugin::init_,
                                          (int (*)(CSOUND*,void*)) TestPlugin::kontrol_,
                                          (int (*)(CSOUND*,void*)) 0);
        return status;
        return 0;
    }

    PUBLIC int csoundModuleDestroy(CSOUND *csound)
    {
        auto instances = instances_for_names();
        std::fprintf(stderr, "TestPlugin:csoundModuleDestroy: csound %p map %p has %ld instances.\n", csound, &instances, instances.size());
        int count = 0;
        for (auto value : instances) {
            count++;
            std::fprintf(stderr, "%s %3d: %p\n", value.first.c_str(), count, value.second);
        }
        return 0;
    }
}
