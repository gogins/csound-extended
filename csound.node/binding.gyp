{
    'dependencies': 
    {
        'node-addon-api': '*',
    },
    'target_defaults': 
    {
       "cflags!": [ "-fno-exceptions" ],
        "cflags_cc!": [ "-fno-exceptions" ],
        "include_dirs": 
        [
            ## This is theoretically required but causes the build to fail: 
            ## "<!@(node -p \"require('node-addon-api').include\")",
            ## This does work but must be manually configured here:
            "/usr/local/lib/node-v12.14.1-linux-x64/lib/node_modules/node-addon-api",
        ],
         'conditions': 
        [
            ['OS=="linux"',
                {
                    'cflags': [
                        '-std=c++14',
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
               'jscsound.cpp',
            ],
        },
    ]    
}
