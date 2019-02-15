{
    "dependencies": {
        "node-addon-api": "^1.3.0"
    },
    "targets": [{
        "target_name": "csound",
        "sources": [
            "jscsound.cpp",
        ],
        'conditions': [
            ['OS=="linux"',
                {
                    'libraries': [
                        '-lcsound64',
                    ],
                    'include_dirs': [
                        '/usr/include/csound',
                        '../CsoundAC'
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
