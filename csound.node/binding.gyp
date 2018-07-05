{
    "targets": [{
        "target_name": "csound",
        "sources": [
            "jscsound.cpp",
        ],
        'conditions': [
            ['OS=="linux"',
                {
                    'libraries': [
                        '-L$(CSOUND_HOME)/../cs6make -lcsound64',
                    ],
                    'include_dirs': [
                        '$(CSOUND_HOME)/include/',
                        '../build-linux',
                        '../dependencies/csound/include',
                    ],
                    'cflags_cc!': [
                        '-fno-exceptions',
                        '-Wno-deprecated-declarations',
                        '-std=c++11',
                    ],
                }
            ],
            ['OS=="win"',
                {
                    'msbuild_toolset': 'v140',
                    'libraries': [
                        '-l$(CSOUND_HOME)/msvc/csound-vs/RelWithDebInfo/csound64.lib',
                    ],
                    'include_dirs': [
                        '$(CSOUND_HOME)/include',
                    ],
                }
            ]
        ]
    }]
}
