{
    'dependencies': 
    {
        'node-addon-api': '^1.3.0',
    },
    'target_defaults': 
    {
       "cflags!": [ "-fno-exceptions" ],
        "cflags_cc!": [ "-fno-exceptions" ],
        "include_dirs": 
        [
            "<!@(node -p \"require('node-addon-api').include\")"
        ],
         'conditions': 
        [
            ['OS=="linux"',
                {
                    'cflags': [
                        '-std=c++11',
                        '-fno-exceptions',
                        '-Wno-deprecated-declarations',
                        '-fPIC',
                    ],
                    'libraries': 
                    [
                        '-lcsound64 -lecl -lluajit-5.1 -lpython3.6m -lgc -lpthread',
                    ],
                    'include_dirs': 
                    [
                        '/usr/include/csound',
                        '/usr/include/python3.6',
                        '/usr/include/luajit-2.1',
                        '/usr/include/ecl',
                        '../CsoundAC',
                    ],
                }
            ],
            ['OS=="win"',
                {
                    'msbuild_toolset': 'v140',
                    'libraries': 
                    [
                        '-l$(CSOUND_HOME)/msvc/csound-vs/RelWithDebInfo/csound64.lib',
                    ],
                    'include_dirs': 
                    [
                        '$(CSOUND_HOME)/include',
                    ],
                }
            ]
        ]
    },
    'targets': 
    [
        {
            'target_name': 'csound',
            'sources': 
            [
                'node-addon-api-csound.cpp',
            ],
        },
    ]    
}
