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
            ## This also does not work:
            ## "/usr/local/lib/nodejs/$(NODEJS_VERSION)-$(NODEJS_VERSION)/lib/node_modules/node-addon-api",
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
                        '-lcsound64 -lpython3.6m -lecl -lgc -lpthread',
                    ],
                    'include_dirs': 
                    [
                        '/usr/include/csound',
                        '/usr/include/python3.6',
                        '../CsoundAC',
                    ],
                }
            ],
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
