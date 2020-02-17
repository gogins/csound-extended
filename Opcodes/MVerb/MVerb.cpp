/*
    MVerb.cpp

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
#include <cmath>
#include <map>
#include <mutex>
#include <random>
#include <string>
#include <thread>
#include <vector>
#include "OpcodeBase.hpp"
#define GAMMA_H_INC_ALL 1
#include <Gamma/Gamma.h>


class MVerb;
class MVerbsForCsounds;
static MVerbsForCsounds &mverbs();

struct Preset {
    float N;
    float res1;
    float res2;
    float res3;
    float res4;
    float res5;
    float res6;
    float res7;
    float res8;
    float res9;
    float res10;
    float res11;
    float res12;
    float res13;
    float res14;
    float res15;
    float res16;
    float res17;
    float res18;
    float res19;
    float res20;
    float res21;
    float res22;
    float res23;
    float res24;
    float res25;
    float FB;
    float DFact;
    float Q;
    float ERamp;
    float ERSelect;
    float EQSelect;
};

static std::map<std::string, Preset> &presets() {
    static bool initialized = false;
    static std::map<std::string, Preset> presets_;
    if (initialized == false) {
        initialized = true;
        presets_["Small Hall"] = {1, 102, 435, 735, 76.8, 114, 669.2, 739.7, 843.6, 272.72, 114.3, 963.2, 250.3, 373.73, 842, 999, 621, 210, 183, 578, 313, 792, 159.3, 401.5, 733, 1010, .93, 1, 22, .9, 2, 1};
        presets_["Medium Hall"] = {2, 57.8, 461.1, 141.6, 442.9, 395.4, 384.7, 156.3, 31.7, 47.7, 181.4, 82.3, 470.7, 283.7, 133.7, 128.5, 426.9, 274.1, 495, 112.3, 401.9, 126.5, 218.9, 374.8, 140.3, 171.3, .943, 1, .5, .9, 3, 1};
        presets_["Large Hall"] = {3, 23.24, 100.43, 45.58, 105.62, 226.26, 65.66, 216.33, 32.41, 244.91, 84.61, 349.42, 134.08, 444.19, 51.83, 32.42, 42.73, 125.7, 83.25, 23.83, 170.28, 116.83, 40.96, 53, 78.42, 29.42, .95, 1, .5, .9, 4, 1};
        presets_["Huge Hall"] = {4, 3.24, 10.43, 5.58, 105.62, 16.26, 65.66, 216.33, 2.41, 244.91, 4.61, 349.42, 134.08, 444.191, 51.83, 32.42, 2.73, 25.7, 83.25, 3.83, 170.28, 6.83, 40.96, 13, 8.42, 20.42, .95, 1, .724, .8, 5, 1};
        presets_["Infinite Space"] = {5, 3.24, 10.43, 5.58, 105.62, 16.26, 65.66, 216.33, 2.41, 244.91, 4.61, 349.42, 134.08, 444.191, 51.83, 32.42, 2.73, 25.7, 83.25, 3.83, 170.28, 6.83, 40.96, 13, 8.42, 20.42, 1, 1, .5, .5, 5, 1};
        presets_["Dry Echoes"] = {6, 200, 2000, 2, 2000, 200, 2000, 2, 200, 2, 2000, 2, 200, 2, 200, 2, 2000, 2, 200, 2, 2000, 200, 2000, 2, 2000, 200, .85, 1, .5, .9, 1, 1};
        presets_["Right"] = {7, 4.4, 10.8, 33.9, 77.23, 243, 3.1, 7.5, 26.68, 64.9, 111.11, 2, 5.5, 17.3, 53, 97.2, 3.6, 7.53, 26.75, 67, 113.2, 4.57, 10.67, 33.25, 75.3, 248, .95, 1, 21, .9, 1, 2};
        presets_["Comby 1"] = {8, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, .99, 1, 3.85, .9, 1, 1};
        presets_["Comby 2"] = {9, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, 275, .995, 1, 2.85, .9, 1, 1};
        presets_["Octaves"] = {10, 320, 80, 320, 80, 2560, 640, 40, 2560, 10240, 80, 2560, 640, 160, 1280, 160, 640, 5120, 320, 40, 10240, 1280, 40, 5120, 320, 160, .995, 1, 70, .9, 1, 2};
        presets_["Tritones"] = {11, 369.99, 92.5, 370, 185, 65.41, 32.7, 261.63, 46.25, 261.63, 1046.5, 739.99, 1479.98, 130.81, 65.41, 23.13, 92.5, 46.25, 130.81, 185, 32.7, 65.41, 130.81, 523.25, 184.99, 92.5, .975, 1, 90, .9, 1, 3};
        presets_["Big Dark"] = {12, 41, 3.84, 50.28, 2.38, 19.71, 3.83, 83.27, 16.93, 27.95, 4.33, 88.41, 39.93, 95.02, 93.99, 84.19, 3.38, 79.5, 69.39, 32.79, 8.32, 97.16, 3.23, 28.84, 8.46, 71.69, .96, 1, 4, .5, 10, 2};
        presets_["Metallic"] = {13, 361.6, 66.9, 679.5, 251.6, 395.3, 166.4, 123.9, 314.6, 262.5, 182.9, 245.4, 40.4, 435.5, 253.9, 350.5, 527.1, 628.3, 365.2, 71.6, 699.6, 684.8, 560.1, 408.4, 55.4, 190, .995, 1, 35, .9, 3, 3};
        presets_["Weird 1"] = {14, 246.96, 246.84, 246.92, 247, 246.88, 164.83, 164.79, 164.81, 164.85, 164.77, 138.58, 138.57, 138.59, 138.61, 138.6, 196.06, 195.97, 196, 196.03, 195.94, 2.35, 6.62, 3.38, 5.01, 2.67, .99, 1, 16.75, .9, 1, 9};
        presets_["Weird 2"] = {15, 2, 150.1, 10.3, 150.3, 2.2, 150.5, 149.9, 149.5, 149.7, 150.8, 10.2, 109.2, 150, 149.3, 10.1, 150.7, 149.6, 149.4, 149.8, 150.6, 2.3, 150.4, 10, 150.2, 2.1, .995, 1, 25, .9, 9, 10};
        presets_["Weird 3"] = {16, 880, 587.33, 18.35, 523.25, 987.77, 698.46, 12.98, 9.72, 10.91, 739.99, 19.45, 9.18, 8.18, 8.66, 20.6, 783.99, 11.56, 10.3, 12.25, 659.25, 932.33, 554.36, 17.32, 622.25, 830.6, .995, 1, .5, 1.25, 6, 11};
        presets_["Large Cymbal A"] = {17, 562, 4047, 6112, 4211, 640, 4333, 661, 6276, 745, 6399, 860, 6487, 445, 1011, 915, 1498, 896, 2720, 4001, 2912, 3872, 6144, 4735, 9920, 2473, .99, 1, 15, 0, 1, 1};
        presets_["Large Cymbal B"] = {18, 6154, 4559, 5043, 4399, 4740, 3839, 4829, 3656, 4649, 2577, 5106, 4309, 6343, 3378, 5478, 3286, 5269, 4230, 4122, 3126, 4026, 3048, 3965, 2897, 5806, .99, 1, 15, 0, 1, 1};
        presets_["Splash Cymbal A"] = {19, 1868, 1136, 2557, 934, 1378, 1421, 487, 2026, 732, 3485, 1824, 2715, 2067, 2269, 1338, 2756, 891, 689, 3243, 1580, 1176, 2312, 1623, 2514, 2799, .993, 1, 15, 0, 1, 1};
        presets_["Splash Cymbal B"] = {20, 1135, 3815, 1442, 4126, 1857, 4623, 250, 2271, 287, 4681, 5151, 323, 2573, 381, 2990, 594, 5323, 627, 5761, 721, 3147, 3294, 4367, 3712, 3599, .99, 1, 15, 0, 1, 1};
        presets_["Turkish Cymbal"] = {21, 1699, 2867, 309, 1871, 3540, 399, 2047, 3644, 503, 2920, 3733, 667, 2196, 4556, 922, 2269, 4587, 1037, 2415, 4430, 1449, 100, 2720, 1524, 194, .99, 1, 15, 0, 1, 1};
        presets_["Gong"] = {22, 1397, 7046, 7145, 7302, 2803, 7346, 1395, 2442, 5618, 2793, 4198, 11111, 8845, 2435, 12675, 4135, 4693, 5563, 6715, 6740, 4733, 9008, 8700, 10315, 8957, .997, 1, 15, 0, 1, 1};
        presets_["Small Gong"] = {23, 1150, 2638, 4630, 1203, 2875, 4697, 1257, 547, 2917, 1357, 644, 3184, 1908, 717, 3532, 2014, 780, 3592, 2089, 991, 3834, 2167, 1082, 4280, 2551, .99, 1, 15, 0, 1, 1};
        presets_["Metallic"] = {24, 1538, 3268, 4500, 4511, 7620, 1818, 5762, 1313, 8902, 8123, 2222, 7200, 1435, 2253, 2345, 7652, 1111, 3342, 6671, 5669, 6359, 1515, 2626, 9821, 5589, .99, 1, 15, 0, 1, 1};
        presets_["Tubular Metallic"] = {24, 1538, 3268, 4500, 4511, 7620, 1818, 5762, 1313, 8902, 8123, 2222, 7200, 1435, 2253, 2345, 7652, 1111, 3342, 6671, 5669, 6359, 1515, 2626, 9821, 5589, .99, 1, 15, 0, 1, 1};
        presets_["25"] = {25, 5801, 2555, 1062, 2988, 1163, 3191, 3438, 1266, 5357, 1391, 1537, 3870, 6775, 1752, 162, 316, 1899, 2141, 520, 6189, 3957, 771, 4389, 974, 4870, .979, 1, 15, 0, 1, 1};
        presets_["26"] = {26, 43, 427.33, 469.63, 636.8, 879, 1194.3, 1285, 1525, 1605.7, 1692.6, 1834, 1913.4, 2058, 2131.5, 2553.4, 2697.3, 2845, 2971, 3094.5, 3421.33, 3517, 3704, 3824.36, 4026, 4103.5, .99, 1, 15, 0, 1, 1};
        presets_["27"] = {27, 50.613, 162.049, 235.200, 449.253, 649.376, 905.887, 1472.145, 1560.246, 1620.714, 1719.811, 2011.201, 2227.079, 2321.843, 2520.884, 3645.568, 3719.782, 4519.738, 5439.683, 5550.825, 5799.373, 5887.881, 5985.715, 8391.483, 8599.464, 8742.274, .99, 1, 15, 0, 1, 1};
        presets_["28"] = {28, 1105.304, 1103.750, 1794.986, 1953.592, 2048.571, 2151.073, 2212.690, 2294.689, 2456.427, 2479.858, 2588.582, 2939.855, 3128.298, 3338.705, 3994.719, 5093.901, 5302.248, 6016.276, 6633.354, 7061.309, 7204.633, 7280.145, 7396.092, 7451.345, 7596.472, .99, 1, 15, 0, 1, 1};
        presets_["29"] = {29, 50.613, 162.049, 235.200, 449.253, 649.376, 905.887, 1472.145, 1560.246, 1620.714, 1719.811, 2011.201, 2227.079, 2321.843, 2520.884, 3645.568, 3719.782, 4519.738, 5439.683, 5550.825, 5799.373, 5887.881, 5985.715, 8391.483, 8599.464, 8742.274, .99, 1, 15, 0, 1, 1};
        presets_["30"] = {30, 256.393, 395.910, 563.166, 828.052, 1184.536, 1335.321, 1546.818, 1827.957, 2480.316, 2773.729, 3196.038, 3341.018, 3464.887, 3685.670, 3978.220, 4078.167, 4271.071, 4423.289, 4519.296, 4722.426, 4897.136, 5013.317, 5107.348, 5175.045, 5274.198, .99, 1, 15, 0, 1, 1};
        presets_["31"] = {31, 62.317, 165.692, 274.382, 348.507, 415.855, 471.671, 528.891, 558.866, 656.594, 775.976, 876.664, 982.164, 1118.923, 1247.646, 1389.558, 1512.517, 1651.298, 1753.110, 1837.643, 1905.806, 1984.672, 2048.393, 2112.321, 2181.847, 2288.017, .99, 1, 15, 0, 1, 1};
        presets_["32"] = {32, 53.066, 203.032, 479.499, 543.879, 743.276, 801.769, 892.304, 960.408, 1056.683, 1207.074, 1959.924, 2512.418, 2796.932, 2962.237, 4128.661, 4757.159, 5226.305, 7228.675, 8082.551, 8150.188, 8996.844, 9060.971, 9135.351, 9440.564, 10176.578, .99, 1, 15, 0, 1, 1};
        presets_["33"] = {33, 59.069, 633.891, 735.346, 829.804, 1096.602, 1281.328, 1381.799, 1457.992, 1601.895, 1736.677, 1801.428, 1882.553, 2063.394, 2141.092, 2241.275, 2423.967, 2496.568, 2740.324, 3155.239, 3228.240, 3422.348, 3571.240, 3639.228, 3668.764, 3749.605, .99, 1, 15, 0, 1, 1};
        presets_["34"] = {34, 62.266, 120.836, 185.376, 299.858, 459.254, 540.377, 640.285, 831.450, 895.198, 991.975, 1107.400, 1266.421, 1415.155, 1466.344, 1641.805, 1747.948, 1899.558, 2223.494, 2309.037, 2359.031, 2439.546, 2524.965, 2662.755, 2801.177, 2884.055, .99, 1, 15, 0, 1, 1};
        presets_["35"] = {35, 1193.150, 1501.169, 1584.422, 1760.128, 1876.764, 2057.868, 2297.103, 2900.218, 3614.225, 3719.350, 3956.729, 5413.109, 6072.599, 6354.645, 6882.175, 7042.264, 7340.854, 8751.580, 9774.188, 10410.871, 10575.217, 11393.946, 15018.148, 16121.814, 18907.158, .99, 1, 15, 0, 1, 1};
    }
    return presets_;
};

// Different presets may have different numbers of taps.

struct EarlyReturnPreset {
    std::vector<float> taps_left;
    std::vector<float> taps_right;
};

static std::map<std::string, EarlyReturnPreset> &earlyReturnPresets() {
    static bool initialized = false;
    static std::map<std::string, EarlyReturnPreset> presets_;
    if (initialized == false) {
        initialized = true;
        presets_["None"] = {{}, {}};
        presets_["Small"] = {{0.0070, 0.5281, 0.0156, 0.5038, 0.0233, 0.3408, 0.0287, 0.1764, 0.0362, 0.2514, 0.0427, 0.1855, 0.0475, 0.2229, 0.0526, 0.1345, 0.0567, 0.1037, 0.0616, 0.0837, 0.0658, 0.0418, 0.0687, 0.0781, 0.0727, 0.0654, 0.0762, 0.0369, 0.0796, 0.0192, 0.0817, 0.0278, 0.0839, 0.0132, 0.0862, 0.0073, 0.0888, 0.0089, 0.0909, 0.0087, 0.0924, 0.0065, 0.0937, 0.0015, 0.0957, 0.0019, 0.0968, 0.0012, 0.0982, 0.0004},
            {0.0097, 0.3672, 0.0147, 0.3860, 0.0208, 0.4025, 0.0274, 0.3310, 0.0339, 0.2234, 0.0383, 0.1326, 0.0441, 0.1552, 0.0477, 0.1477, 0.0533, 0.2054, 0.0582, 0.1242, 0.0631, 0.0707, 0.0678, 0.1061, 0.0702, 0.0331, 0.0735, 0.0354, 0.0758, 0.0478, 0.0778, 0.0347, 0.0814, 0.0185, 0.0836, 0.0157, 0.0855, 0.0197, 0.0877, 0.0171, 0.0902, 0.0078, 0.0915, 0.0032, 0.0929, 0.0026, 0.0947, 0.0014, 0.0958, 0.0018, 0.0973, 0.0007, 0.0990, 0.0007}
        };
        presets_["Medium"] = {{0.0215, 0.3607, 0.0435, 0.2480, 0.0566, 0.3229, 0.0691, 0.5000, 0.0842, 0.1881, 0.1010, 0.2056, 0.1140, 0.1224, 0.1224, 0.3358, 0.1351, 0.3195, 0.1442, 0.2803, 0.1545, 0.1909, 0.1605, 0.0535, 0.1680, 0.0722, 0.1788, 0.1138, 0.1886, 0.0467, 0.1945, 0.1731, 0.2010, 0.0580, 0.2096, 0.0392, 0.2148, 0.0314, 0.2201, 0.0301, 0.2278, 0.0798, 0.2357, 0.0421, 0.2450, 0.0208, 0.2530, 0.0484, 0.2583, 0.0525, 0.2636, 0.0335, 0.2694, 0.0311, 0.2764, 0.0455, 0.2817, 0.0362, 0.2874, 0.0252, 0.2914, 0.0113, 0.2954, 0.0207, 0.2977, 0.0120, 0.3029, 0.0067, 0.3054, 0.0094, 0.3084, 0.0135, 0.3127, 0.0095, 0.3157, 0.0111, 0.3178, 0.0036, 0.3202, 0.0064, 0.3221, 0.0025, 0.3252, 0.0016, 0.3268, 0.0051, 0.3297, 0.0029, 0.3318, 0.0038, 0.3345, 0.0016, 0.3366, 0.0013, 0.3386, 0.0009, 0.3401, 0.0019, 0.3416, 0.0012, 0.3431, 0.0015, 0.3452, 0.0011, 0.3471, 0.0007, 0.3488, 0.0003},
            {0.0146, 0.5281, 0.0295, 0.3325, 0.0450, 0.3889, 0.0605, 0.2096, 0.0792, 0.5082, 0.0881, 0.1798, 0.1051, 0.3287, 0.1132, 0.1872, 0.1243, 0.1184, 0.1338, 0.1134, 0.1480, 0.1400, 0.1594, 0.2602, 0.1721, 0.0610, 0.1821, 0.1736, 0.1908, 0.0738, 0.1978, 0.1547, 0.2084, 0.0842, 0.2187, 0.0505, 0.2256, 0.0906, 0.2339, 0.0996, 0.2428, 0.0490, 0.2493, 0.0186, 0.2558, 0.0164, 0.2596, 0.0179, 0.2658, 0.0298, 0.2698, 0.0343, 0.2750, 0.0107, 0.2789, 0.0417, 0.2817, 0.0235, 0.2879, 0.0238, 0.2938, 0.0202, 0.2965, 0.0242, 0.3015, 0.0209, 0.3050, 0.0139, 0.3097, 0.0039, 0.3137, 0.0039, 0.3165, 0.0096, 0.3205, 0.0073, 0.3231, 0.0052, 0.3255, 0.0069, 0.3273, 0.0044, 0.3298, 0.0041, 0.3326, 0.0026, 0.3348, 0.0028, 0.3372, 0.0014, 0.3389, 0.0023, 0.3413, 0.0012, 0.3428, 0.0014, 0.3443, 0.0006, 0.3458, 0.0003, 0.3474, 0.0004, 0.3486, 0.0005 }
        };
        presets_["Large"] = {{0.0473, 0.1344, 0.0725, 0.5048, 0.0997, 0.2057, 0.1359, 0.2231, 0.1716, 0.4355, 0.1963, 0.1904, 0.2168, 0.2274, 0.2508, 0.0604, 0.2660, 0.1671, 0.2808, 0.1725, 0.3023, 0.0481, 0.3154, 0.1940, 0.3371, 0.0651, 0.3579, 0.0354, 0.3718, 0.0504, 0.3935, 0.1609, 0.4041, 0.1459, 0.4166, 0.1355, 0.4344, 0.0747, 0.4524, 0.0173, 0.4602, 0.0452, 0.4679, 0.0643, 0.4795, 0.0377, 0.4897, 0.0159, 0.4968, 0.0433, 0.5104, 0.0213, 0.5170, 0.0115, 0.5282, 0.0102, 0.5390, 0.0091, 0.5451, 0.0146, 0.5552, 0.0371, 0.5594, 0.0192, 0.5667, 0.0218, 0.5740, 0.0176, 0.5806, 0.0242, 0.5871, 0.0167, 0.5931, 0.0184, 0.6000, 0.0075, 0.6063, 0.0060, 0.6121, 0.0068, 0.6149, 0.0138, 0.6183, 0.0044, 0.6217, 0.0035, 0.6243, 0.0026, 0.6274, 0.0017, 0.6295, 0.0098, 0.6321, 0.0054, 0.6352, 0.0022, 0.6380, 0.0011, 0.6414, 0.0012, 0.6432, 0.0062, 0.6462, 0.0024, 0.6478, 0.0032, 0.6506, 0.0009},
            {0.0271, 0.5190, 0.0558, 0.1827, 0.0776, 0.3068, 0.1186, 0.2801, 0.1421, 0.1526, 0.1698, 0.3249, 0.1918, 0.1292, 0.2178, 0.2828, 0.2432, 0.1128, 0.2743, 0.1884, 0.2947, 0.2023, 0.3121, 0.1118, 0.3338, 0.0660, 0.3462, 0.0931, 0.3680, 0.1295, 0.3889, 0.1430, 0.4040, 0.0413, 0.4218, 0.1122, 0.4381, 0.1089, 0.4553, 0.0691, 0.4718, 0.0699, 0.4832, 0.0375, 0.4925, 0.0119, 0.5065, 0.0181, 0.5180, 0.0500, 0.5281, 0.0228, 0.5365, 0.0072, 0.5458, 0.0366, 0.5520, 0.0065, 0.5597, 0.0115, 0.5644, 0.0105, 0.5724, 0.0063, 0.5801, 0.0118, 0.5836, 0.0198, 0.5886, 0.0172, 0.5938, 0.0081, 0.5987, 0.0094, 0.6033, 0.0029, 0.6060, 0.0078, 0.6096, 0.0149, 0.6122, 0.0102, 0.6171, 0.0144, 0.6204, 0.0014, 0.6243, 0.0038, 0.6284, 0.0111, 0.6309, 0.0107, 0.6338, 0.0036, 0.6374, 0.0035, 0.6401, 0.0015, 0.6417, 0.0052, 0.6433, 0.0019, 0.6461, 0.0033, 0.6485, 0.0007}
        };
        presets_["Huge"] = {{0.0588, 0.6917, 0.1383, 0.2512, 0.2158, 0.5546, 0.2586, 0.2491, 0.3078, 0.1830, 0.3731, 0.3712, 0.4214, 0.1398, 0.4622, 0.1870, 0.5004, 0.1652, 0.5365, 0.2254, 0.5604, 0.1423, 0.5950, 0.1355, 0.6233, 0.1282, 0.6486, 0.1312, 0.6725, 0.1009, 0.7063, 0.0324, 0.7380, 0.0968, 0.7602, 0.0169, 0.7854, 0.0530, 0.8097, 0.0342, 0.8303, 0.0370, 0.8404, 0.0173, 0.8587, 0.0281, 0.8741, 0.0164, 0.8825, 0.0045, 0.8945, 0.0181, 0.9063, 0.0057, 0.9136, 0.0030, 0.9214, 0.0065, 0.9296, 0.0059, 0.9373, 0.0021, 0.9462, 0.0087, 0.9541, 0.0035, 0.9605, 0.0013, 0.9648, 0.0043, 0.9691, 0.0014, 0.9746, 0.0011, 0.9774, 0.0032, 0.9818, 0.0020, 0.9853, 0.0042, 0.9889, 0.0030, 0.9923, 0.0034, 0.9941, 0.0021, 0.9976, 0.0009, 0.9986, 0.0008},
            {0.0665, 0.4406, 0.1335, 0.6615, 0.1848, 0.2284, 0.2579, 0.4064, 0.3293, 0.1433, 0.3756, 0.3222, 0.4157, 0.3572, 0.4686, 0.3280, 0.5206, 0.1134, 0.5461, 0.0540, 0.5867, 0.0473, 0.6281, 0.1018, 0.6516, 0.1285, 0.6709, 0.0617, 0.6979, 0.0360, 0.7173, 0.1026, 0.7481, 0.0621, 0.7690, 0.0585, 0.7943, 0.0340, 0.8072, 0.0170, 0.8177, 0.0092, 0.8345, 0.0369, 0.8511, 0.0369, 0.8621, 0.0251, 0.8740, 0.0109, 0.8849, 0.0135, 0.8956, 0.0118, 0.9026, 0.0187, 0.9110, 0.0182, 0.9225, 0.0034, 0.9310, 0.0083, 0.9354, 0.0058, 0.9420, 0.0040, 0.9464, 0.0028, 0.9549, 0.0090, 0.9590, 0.0076, 0.9654, 0.0030, 0.9691, 0.0041, 0.9729, 0.0009, 0.9757, 0.0024, 0.9787, 0.0049, 0.9823, 0.0040, 0.9847, 0.0025, 0.9898, 0.0005, 0.9922, 0.0022, 0.9935, 0.0025, 0.9964, 0.0027, 0.9992, 0.0007}
        };
        presets_["Long Random"] = {{0.0131, 0.6191, 0.0518, 0.4595, 0.0800, 0.4688, 0.2461, 0.2679, 0.3826, 0.1198, 0.5176, 0.2924, 0.6806, 0.0293, 0.8211, 0.0327, 1.0693, 0.3318, 1.2952, 0.1426, 1.3079, 0.1021, 1.4337, 0.1293, 1.4977, 0.2383, 1.6702, 0.0181, 1.7214, 0.2042, 1.8849, 0.0780, 2.1279, 0.0160, 2.2836, 0.0061, 2.4276, 0.0390, 2.5733, 0.1090, 2.7520, 0.0047, 2.8650, 0.0077, 3.1026, 0.0005},
            {0.1591, 0.4892, 0.2634, 0.1430, 0.3918, 0.0978, 0.5004, 0.0675, 0.7004, 0.1285, 0.7251, 0.0251, 0.9368, 0.4531, 1.0770, 0.0022, 1.1426, 0.0132, 1.3189, 0.1608, 1.3550, 0.0512, 1.4347, 0.0224, 1.4739, 0.1401, 1.6996, 0.1680, 1.9292, 0.1481, 2.1435, 0.2463, 2.1991, 0.1748, 2.3805, 0.1802, 2.4796, 0.0105, 2.6615, 0.0049, 2.8115, 0.0517, 2.9687, 0.0468, 2.9899, 0.0095, 3.1554, 0.0496}
        };
        presets_["Short Backwards"] = {{0.0022, 0.0070, 0.0040, 0.0014, 0.0054, 0.0120, 0.0085, 0.0075, 0.0106, 0.0156, 0.0141, 0.0089, 0.0176, 0.0083, 0.0200, 0.0227, 0.0225, 0.0189, 0.0253, 0.0121, 0.0284, 0.0118, 0.0367, 0.0193, 0.0431, 0.0163, 0.0477, 0.0260, 0.0558, 0.0259, 0.0632, 0.0515, 0.0694, 0.0266, 0.0790, 0.0279, 0.0873, 0.0712, 0.1075, 0.1212, 0.1286, 0.0938, 0.1433, 0.1305, 0.1591, 0.0929, 0.1713, 0.2410, 0.1982, 0.1409, 0.2144, 0.3512, 0.2672, 0.5038, 0.3293, 0.3827},
            {0.0019, 0.0107, 0.0030, 0.0031, 0.0049, 0.0068, 0.0066, 0.0050, 0.0098, 0.0090, 0.0132, 0.0080, 0.0165, 0.0085, 0.0196, 0.0071, 0.0221, 0.0143, 0.0247, 0.0086, 0.0316, 0.0164, 0.0374, 0.0160, 0.0416, 0.0110, 0.0511, 0.0167, 0.0619, 0.0191, 0.0730, 0.0233, 0.0887, 0.0313, 0.1037, 0.0484, 0.1114, 0.0912, 0.1219, 0.0980, 0.1482, 0.1220, 0.1806, 0.2021, 0.2057, 0.2059, 0.2382, 0.2379, 0.2550, 0.2536, 0.3112, 0.6474}
        };
        presets_["Long Backwards"] = {{0.0021, 0.0008, 0.0050, 0.0006, 0.0065, 0.0007, 0.0092, 0.0014, 0.0124, 0.0028, 0.0145, 0.0032, 0.0166, 0.0015, 0.0225, 0.0018, 0.0294, 0.0030, 0.0345, 0.0077, 0.0405, 0.0056, 0.0454, 0.0096, 0.0508, 0.0088, 0.0593, 0.0082, 0.0643, 0.0074, 0.0743, 0.0182, 0.0874, 0.0103, 0.0986, 0.0270, 0.1143, 0.0135, 0.1370, 0.0327, 0.1633, 0.0420, 0.1823, 0.0708, 0.2028, 0.0842, 0.2258, 0.0962, 0.2482, 0.0513, 0.2856, 0.1035, 0.3132, 0.1229, 0.3398, 0.0721, 0.3742, 0.0996, 0.4199, 0.1817, 0.4914, 0.3000, 0.5557, 0.1649, 0.6181, 0.4180, 0.6689, 0.5216, 0.7310, 0.5185},
            {0.0024, 0.0007, 0.0053, 0.0006, 0.0090, 0.0034, 0.0138, 0.0026, 0.0196, 0.0016, 0.0250, 0.0080, 0.0292, 0.0051, 0.0346, 0.0039, 0.0409, 0.0089, 0.0459, 0.0067, 0.0589, 0.0132, 0.0702, 0.0192, 0.0781, 0.0211, 0.0964, 0.0239, 0.1052, 0.0201, 0.1212, 0.0226, 0.1428, 0.0147, 0.1547, 0.0418, 0.1849, 0.0232, 0.2110, 0.0975, 0.2425, 0.0620, 0.2851, 0.0963, 0.3366, 0.1248, 0.3645, 0.1321, 0.4079, 0.1293, 0.4433, 0.1425, 0.5031, 0.3787, 0.5416, 0.5061, 0.6336, 0.2865, 0.7434, 0.6477}
        };
        presets_["Strange1"] = {{0.0137, 0.2939, 0.0763, 0.8381, 0.2189, 0.7019, 0.2531, 0.2366, 0.3822, 0.3756, 0.4670, 0.0751, 0.4821, 0.0870, 0.4930, 0.0794, 0.5087, 0.1733, 0.5633, 0.0657, 0.6078, 0.0218, 0.6410, 0.0113, 0.6473, 0.0246, 0.6575, 0.0513, 0.6669, 0.0431, 0.6693, 0.0392, 0.6916, 0.0050, 0.6997, 0.0192, 0.7091, 0.0186, 0.7174, 0.0105, 0.7284, 0.0254, 0.7366, 0.0221, 0.7390, 0.0112, 0.7446, 0.0029, 0.7470, 0.0211, 0.7495, 0.0006},
            {0.0036, 0.0052, 0.0069, 0.0105, 0.0096, 0.0190, 0.0138, 0.0235, 0.0150, 0.0018, 0.0231, 0.0012, 0.0340, 0.0022, 0.0355, 0.0154, 0.0415, 0.0057, 0.0538, 0.0237, 0.0722, 0.0037, 0.0839, 0.0291, 0.1027, 0.0500, 0.1163, 0.0367, 0.1375, 0.0114, 0.1749, 0.0156, 0.2002, 0.0635, 0.2215, 0.0660, 0.2777, 0.0517, 0.3481, 0.1666, 0.3871, 0.2406, 0.4851, 0.1022, 0.5305, 0.2043, 0.5910, 0.4109, 0.6346, 0.5573, 0.7212, 0.5535, 0.8981, 0.5854}
        };
        presets_["Strange2"] = {{0.0306, 0.3604, 0.2779, 0.6327, 0.3687, 0.2979, 0.5186, 0.4202, 0.6927, 0.3695, 0.7185, 0.2370, 0.8703, 0.3283, 0.9138, 0.1334, 0.9610, 0.1183, 1.0656, 0.2089, 1.1153, 0.0835, 1.1933, 0.0954, 1.1974, 0.0609, 1.2972, 0.1078, 1.3243, 0.0720, 1.3498, 0.0840, 1.4191, 0.0694, 1.4479, 0.0572, 1.4992, 0.0449, 1.5256, 0.0186, 1.5704, 0.0470, 1.5852, 0.0202, 1.6090, 0.0106, 1.6165, 0.0302, 1.6440, 0.0204, 1.6557, 0.0042, 1.6582, 0.0223, 1.6810, 0.0054, 1.6814, 0.0064, 1.6943, 0.0075, 1.6988, 0.0032, 1.7064, 0.0027, 1.7073, 0.0064, 1.7124, 0.0091, 1.7150, 0.0015, 1.7218, 0.0043, 1.7308, 0.0116, 1.7335, 0.0122, 1.7355, 0.0011, 1.7433, 0.0154, 1.7466, 0.0084, 1.7487, 0.0139, 1.7503, 0.0123, 1.7520, 0.0036, 1.7561, 0.0097, 1.7565, 0.0041, 1.7586, 0.0016, 1.7657, 0.0132, 1.7704, 0.0038, 1.7748, 0.0020, 1.7777, 0.0053, 1.7783, 0.0056, 1.7791, 0.0017, 1.7818, 0.0058, 1.7822, 0.0089, 1.7844, 0.0074, 1.7863, 0.0009, 1.7878, 0.0016, 1.7899, 0.0061, 1.7919, 0.0073, 1.7925, 0.0025, 1.7941, 0.0045, 1.7956, 0.0060, 1.7958, 0.0088, 1.7963, 0.0010, 1.7965, 0.0006, 1.7977, 0.0078, 1.7988, 0.0026},
            {0.0011, 0.0055, 0.0022, 0.0063, 0.0027, 0.0089, 0.0034, 0.0009, 0.0049, 0.0010, 0.0064, 0.0005, 0.0069, 0.0044, 0.0091, 0.0027, 0.0103, 0.0099, 0.0112, 0.0017, 0.0131, 0.0018, 0.0142, 0.0008, 0.0159, 0.0010, 0.0188, 0.0034, 0.0207, 0.0055, 0.0245, 0.0005, 0.0262, 0.0094, 0.0312, 0.0057, 0.0344, 0.0051, 0.0402, 0.0044, 0.0404, 0.0102, 0.0433, 0.0044, 0.0435, 0.0034, 0.0489, 0.0087, 0.0512, 0.0108, 0.0605, 0.0046, 0.0702, 0.0010, 0.0734, 0.0121, 0.0839, 0.0135, 0.0985, 0.0151, 0.1014, 0.0203, 0.1041, 0.0043, 0.1114, 0.0150, 0.1216, 0.0182, 0.1293, 0.0220, 0.1299, 0.0169, 0.1312, 0.0046, 0.1453, 0.0046, 0.1527, 0.0062, 0.1545, 0.0192, 0.1578, 0.0092, 0.1655, 0.0053, 0.1754, 0.0301, 0.1967, 0.0122, 0.2289, 0.0233, 0.2353, 0.0131, 0.2632, 0.0396, 0.2873, 0.0171, 0.3348, 0.0454, 0.3872, 0.0398, 0.4484, 0.0244, 0.4913, 0.0693, 0.5424, 0.0820, 0.5668, 0.1112, 0.6054, 0.0635, 0.6669, 0.1016, 0.7211, 0.1217, 0.7541, 0.1756, 0.8759, 0.1688, 0.9106, 0.1932, 1.0384, 0.1542, 1.0732, 0.1598, 1.0767, 0.2409, 1.0988, 0.1879, 1.2422, 0.3049, 1.3480, 0.3001, 1.4961, 0.3374, 1.5886, 0.2791, 1.5957, 0.3366, 1.8248, 0.2962}
        };
    }
    return presets_;
};

struct EqualizerPreset {
    float gain[10];
};

struct MasterPreset {
    std::string presetName;
    Preset preset;
    std::string earlyReturnPresetName;
    EarlyReturnPreset earlyReturnPreset;
    std::string equalizerPresetName;
    EqualizerPreset equalizerPreset;
    // Control parameters not in sub-presets.
    float effect_mix_wet = 0.25;
    float effect_mix_dry = 0.75;
    float clear_delays = 0.;  
    float krand = 0.;
    float krslow = 0.;
    float krfast = 0.;
    float krmax = 0.;
    float random_seed = -1.;
    float print = 0.;
};

static std::map<std::string, EqualizerPreset> &equalizerPresets() {
    static bool initialized = false;
    static std::map<std::string, EqualizerPreset> presets_;
    if (initialized == false) {
        initialized = true;
        presets_["flat"] =        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
        presets_["high cut 1"] =  {1, 1, 1, 1, 1, .99, .98, .97, .96, .95};
        presets_["high cut 2"] =  {1, 1, 1, .95, .9, .85, .8, .75, .7, .65};
        presets_["low cut 1"] =   {.9, .93, .96, .98, 1, 1, 1, 1, 1, 1};
        presets_["low cut 2"] =   {.65, .7, .75, .8, .85, .9, .95, 1, 1, 1};
        presets_["band pass 1"] = {.25, .75, 1, 1, 1, 1, 1, 1, .75, .25};
        presets_["band pass 2"] = {.1, .25, .5, .75, 1, 1, .75, .5, .25, .1};
        presets_["2 bands"] =     {.25, .75, 1, .75, .25, .25, .75, 1, .75, .25};
        presets_["3 bands"] =     {25, 1, 1, .25, 1, 1, .25, 1, 1, .25};
        presets_["evens"] =       {.1, 1, .1, 1, .1, 1, .1, 1, .1, 1};
        presets_["odds"] =        {1, .1, 1, .1, 1, .1, 1, .1, 1, .1};
    }
    return presets_;
};

/**
 * Adds interpolated random deviations to a value. The timing and magnitude of 
 * deviations is set by the user.
 */
struct RandomDeviation {
    const MasterPreset *masterPreset;
    std::default_random_engine *generator;
    std::uniform_real_distribution<float> deviation_frequency_distribution;
    std::uniform_real_distribution<float> deviation_magnitude_distribution;
    float samples_per_second = 0.;
    float seconds_per_sample = 0.;
    float frequency_sampling_phase = 1.;
    float frequency_sampling_phase_increment = 0.;
    float deviation_sampling_phase = 1.;
    float deviation_sampling_phase_increment = 0.;
    float current_deviation_sampling_frequency = 0.;
    float prior_deviation_sampling_frequency = 0.;
    float next_deviation_sampling_frequency = 0.;
    float current_deviation_magnitude = 0.;
    float prior_deviation_magnitude = 0.;
    float next_deviation_magnitude = 0.;
    void initialize(CSOUND *csound, const MasterPreset &masterPreset_, std::default_random_engine *generator_) {
        masterPreset = &masterPreset_;
        generator = generator_;
        samples_per_second = csound->GetSr(csound);
        seconds_per_sample = 1. / samples_per_second;
        // Set up the phasors to start on the first "tick."
        frequency_sampling_phase = 1.;
        deviation_sampling_phase = 1.;
        // Set up the zeroth breakpoints.
        deviation_frequency_distribution.param(std::uniform_real_distribution<float>::param_type(masterPreset->krslow, masterPreset->krfast));
        current_deviation_sampling_frequency = deviation_frequency_distribution(*generator);
        deviation_magnitude_distribution.param(std::uniform_real_distribution<float>::param_type(-masterPreset->krmax, masterPreset->krmax));
        current_deviation_magnitude = deviation_magnitude_distribution(*generator);
    };
    // This can be implemented with simple difference equations, because 
    // rounding errors should not be a problem at these low frequencies.
    float operator () (float value) {
        if (masterPreset->krand == true) {
            value = value + (value * current_deviation_magnitude);
            frequency_sampling_phase_increment = masterPreset->krfast / samples_per_second;
            if (frequency_sampling_phase >= 1.) {
                frequency_sampling_phase = 0.;
                prior_deviation_sampling_frequency = current_deviation_sampling_frequency;
                next_deviation_sampling_frequency = deviation_frequency_distribution(*generator, std::uniform_real_distribution<float>::param_type(masterPreset->krslow, masterPreset->krfast));
            }
            deviation_sampling_phase_increment = next_deviation_sampling_frequency / samples_per_second;                
            current_deviation_sampling_frequency = prior_deviation_sampling_frequency + ((next_deviation_sampling_frequency - prior_deviation_sampling_frequency) * frequency_sampling_phase);
            if (deviation_sampling_phase >= 1.) {
                deviation_sampling_phase = 0.;
               prior_deviation_magnitude = current_deviation_magnitude;
                next_deviation_magnitude = deviation_magnitude_distribution(*generator, std::uniform_real_distribution<float>::param_type(-masterPreset->krmax, masterPreset->krmax));
            }
            current_deviation_magnitude = prior_deviation_magnitude + ((next_deviation_magnitude - prior_deviation_magnitude) * deviation_sampling_phase);   
            frequency_sampling_phase += frequency_sampling_phase_increment;
            deviation_sampling_phase += deviation_sampling_phase_increment;
            return value;
        } else {
            return value;
        }
    };
};

struct Multitaps {
    gam::Delay<> delay;
    std::vector<float> times;
    std::vector<float> gains;
    int tap_count;
    MasterPreset *master_preset;
    void initialize(CSOUND *csound, std::vector<float> &parameters, MasterPreset &master_preset_) {
        master_preset = &master_preset_;
        times.resize(0);
        gains.resize(0);
        float maximum_delay = 0;
        tap_count = 0;
        for (int i = 0, n = parameters.size(); i < n; ) {
            if (maximum_delay < parameters[i]) {
                maximum_delay = parameters[i];
            }
            times.push_back((float) parameters[i]);
            ++i;
            gains.push_back((float) parameters[i]);
            ++i;
            ++tap_count;
        }
        delay.maxDelay(maximum_delay + .1);
    }
    virtual float operator () (float input) {
        // This increments phase...
        delay.write(input);
        float output = 0;
        for (int tap = 0; tap < tap_count; ++tap) {
            output += (delay.read(times[tap]) * gains[tap] * master_preset->preset.ERamp);
        }
        return output;
    }
};

struct Balance {
    gam::EnvFollow<> signal_envelope_follower;
    gam::EnvFollow<> comparator_envelope_follower;
     float operator () (float signal, float comparator) {
        float signal_envelope = signal_envelope_follower(signal);
        float comparator_envelope = comparator_envelope_follower(comparator);
        if (comparator_envelope != 0. && signal_envelope != 0.) {
            float gain_factor = comparator_envelope / signal_envelope;
            return signal * gain_factor;
        } else {
            return signal;
        }
    };
};

struct Equalizer {
    gam::Biquad<> biquad[10];
    Balance balance;
    gam::BlockDC<> blockdc;
    void initialize(CSOUND *csound, const MasterPreset &masterPreset) {
        // Adjust nominal Q.
        float adjusted_q = (masterPreset.preset.Q * .1) * .5;
        biquad[0].type(gam::LOW_SHELF);
        biquad[0].freq(   40.);
        biquad[0].res(adjusted_q);
        biquad[0].level(masterPreset.equalizerPreset.gain[0]);
        biquad[1].type(gam::PEAKING);
        biquad[1].freq(   80.);
        biquad[1].res(masterPreset.preset.Q);
        biquad[1].level(masterPreset.equalizerPreset.gain[1]);
        biquad[2].type(gam::PEAKING);
        biquad[2].freq(  160.);
        biquad[2].res(adjusted_q);
        biquad[2].level(masterPreset.equalizerPreset.gain[2]);
        biquad[3].type(gam::PEAKING);
        biquad[3].freq(  320.);
        biquad[3].res(adjusted_q);
        biquad[3].level(masterPreset.equalizerPreset.gain[3]);
        biquad[4].type(gam::PEAKING);
        biquad[4].freq(  640.);
        biquad[4].res(adjusted_q);
        biquad[4].level(masterPreset.equalizerPreset.gain[4]);
        biquad[5].type(gam::PEAKING);
        biquad[5].freq( 1280.);
        biquad[5].res(adjusted_q);
        biquad[5].level(masterPreset.equalizerPreset.gain[5]);
        biquad[6].type(gam::PEAKING);
        biquad[6].freq( 2560.);
        biquad[6].res(adjusted_q);
        biquad[6].level(masterPreset.equalizerPreset.gain[6]);
        biquad[7].type(gam::PEAKING);
        biquad[7].freq( 5120.);
        biquad[7].res(adjusted_q);
        biquad[7].level(masterPreset.equalizerPreset.gain[7]);
        biquad[8].type(gam::PEAKING);
        biquad[8].freq(10240.);
        biquad[8].res(adjusted_q);
        biquad[8].level(masterPreset.equalizerPreset.gain[8]);
        biquad[9].type(gam::HIGH_SHELF);
        biquad[9].freq(20480.);
        biquad[9].res(adjusted_q);
        biquad[9].level(masterPreset.equalizerPreset.gain[9]);
    };
    float operator () (float input) {
        float output = 0.;
        for (int i = 0; i < 10; ++i) {
            output += biquad[i](input);
        }
        output = balance(output, input);
        output = blockdc(output);
        return output;
    }
};

struct MeshEqualizer {
    gam::Delay<> delay[4];
    Equalizer equalizer[4];
    void initialize(CSOUND *csound, const MasterPreset &masterPreset_) {
        delay[0].maxDelay(1.);
        delay[1].maxDelay(1.);
        delay[2].maxDelay(1.);
        delay[3].maxDelay(1.);
        equalizer[0].initialize(csound, masterPreset_);
        equalizer[1].initialize(csound, masterPreset_);
        equalizer[2].initialize(csound, masterPreset_);
        equalizer[3].initialize(csound, masterPreset_);
    };
    void operator () (
        /* Outputs: */
        float &aUout, float &aRout, float &aDout, float &aLout,
        /* Inputs: */
        float aUin, float aRin, float aDin, float aLin, float delay_variation, float kFB) {
        float afactor = (aUin + aRin + aDin + aLin) * -.5;
        delay[0].write(aUin + afactor);
        aUout = delay[0].read(delay_variation);
        delay[1].write(aRin + afactor);
        aRout = delay[1].read(delay_variation);
        delay[2].write(aDin + afactor);
        aDout = delay[2].read(delay_variation);
        delay[3].write(aLin + afactor);
        aLout = delay[3].read(delay_variation);
        aUout = equalizer[0](aUout) * kFB;
        aRout = equalizer[1](aRout) * kFB;
        aDout = equalizer[2](aDout) * kFB;
        aLout = equalizer[3](aLout) * kFB;
    };
};

struct MVerb {
    bool initialized = false;
    CSOUND *csound = nullptr;
    int frames_per_second = 0;
    MYFLT seconds_per_frame = 0;
    int frames_per_block = 0;
    std::default_random_engine generator;
    int random_seed = 1;
    MasterPreset master_preset;
    std::vector<std::string> early_return_presets_for_numbers = {"None", "Small", "Medium", "Large", "Huge", "Long Random", "Short Backwards", "Long Backwards", "Strange1", "Strange2"}; 
    std::vector<std::string> equalizer_presets_for_numbers = {"flat", "high cut 1", "high cut 2", "low cut 1", "low cut 2", "band pass 1", "band pass 2", "2 bands", "3 bands", "evens", "odds"};
    gam::BlockDC<> blockdc_in_left;
    gam::BlockDC<> blockdc_in_right;
    Multitaps multitaps_left;
    Multitaps multitaps_right;
    RandomDeviation randomize_delay[25];
    MeshEqualizer mesh_equalizer[25];
    gam::BlockDC<> blockdc_out_left;
    gam::BlockDC<> blockdc_out_right;
    // State variables.
    float input_left = 0;
    float input_right = 0;
    float aL = 0.;
    float aR = 0.;
    float aAU = 0.;
    float aAR = 0.;
    float aAD = 0.;
    float aAL = 0.;
    float aBU = 0.;
    float aBR = 0.;
    float aBD = 0.;
    float aBL = 0.;
    float aCU = 0.;
    float aCR = 0.;
    float aCD = 0.;
    float aCL = 0.;
    float aDU = 0.;
    float aDR = 0.;
    float aDD = 0.;
    float aDL = 0.;
    float aEU = 0.;
    float aER = 0.;
    float aED = 0.;
    float aEL = 0.;
    float aFU = 0.;
    float aFR = 0.;
    float aFD = 0.;
    float aFL = 0.;
    float aGU = 0.;
    float aGR = 0.;
    float aGD = 0.;
    float aGL = 0.;
    float aHU = 0.;
    float aHR = 0.;
    float aHD = 0.;
    float aHL = 0.;
    float aIU = 0.;
    float aIR = 0.;
    float aID = 0.;
    float aIL = 0.;
    float aJU = 0.;
    float aJR = 0.;
    float aJD = 0.;
    float aJL = 0.;
    float aKU = 0.;
    float aKR = 0.;
    float aKD = 0.;
    float aKL = 0.;
    float aLU = 0.;
    float aLR = 0.;
    float aLD = 0.;
    float aLL = 0.;
    float aMU = 0.;
    float aMR = 0.;
    float aMD = 0.;
    float aML = 0.;
    float aNU = 0.;
    float aNR = 0.;
    float aND = 0.;
    float aNL = 0.;
    float aOU = 0.;
    float aOR = 0.;
    float aOD = 0.;
    float aOL = 0.;
    float aPU = 0.;
    float aPR = 0.;
    float aPD = 0.;
    float aPL = 0.;
    float aQU = 0.;
    float aQR = 0.;
    float aQD = 0.;
    float aQL = 0.;
    float aRU = 0.;
    float aRR = 0.;
    float aRD = 0.;
    float aRL = 0.;
    float aSU = 0.;
    float aSR = 0.;
    float aSD = 0.;
    float aSL = 0.;
    float aTU = 0.;
    float aTR = 0.;
    float aTD = 0.;
    float aTL = 0.;
    float aUU = 0.;
    float aUR = 0.;
    float aUD = 0.;
    float aUL = 0.;
    float aVU = 0.;
    float aVR = 0.;
    float aVD = 0.;
    float aVL = 0.;
    float aWU = 0.;
    float aWR = 0.;
    float aWD = 0.;
    float aWL = 0.;
    float aXU = 0.;
    float aXR = 0.;
    float aXD = 0.;
    float aXL = 0.;
    float aYU = 0.;
    float aYR = 0.;
    float aYD = 0.;
    float aYL = 0.;
    float delay_variation1 = 0.;
    float delay_variation2 = 0.;
    float delay_variation3 = 0.;
    float delay_variation4 = 0.;
    float delay_variation5 = 0.;
    float delay_variation6 = 0.;
    float delay_variation7 = 0.;
    float delay_variation8 = 0.;
    float delay_variation9 = 0.;
    float delay_variation10 = 0.;
    float delay_variation11 = 0.;
    float delay_variation12 = 0.;
    float delay_variation13 = 0.;
    float delay_variation14 = 0.;
    float delay_variation15 = 0.;
    float delay_variation16 = 0.;
    float delay_variation17 = 0.;
    float delay_variation18 = 0.;
    float delay_variation19 = 0.;
    float delay_variation20 = 0.;
    float delay_variation21 = 0.;
    float delay_variation22 = 0.;
    float delay_variation23 = 0.;
    float delay_variation24 = 0.;
    float delay_variation25 = 0.;
    float reverberated_left = 0.;
    float reverberated_right = 0.;
    // All user-controllable parameters, whether in a preset struct or not,
    // are updated from here by reference.
    std::map<std::string, float *> parameter_values_for_names;
    bool printed = false;
    void initialize(CSOUND *csound_) {
        if (initialized == false) {
            initialized = true;
            csound = csound_;
            frames_per_second = csound->GetSr(csound);
            seconds_per_frame = 1.0 / MYFLT(frames_per_second);
            frames_per_block = csound->GetKsmps(csound);
            gam::sampleRate(frames_per_second);
            printed = false;
            // Preset.
            parameter_values_for_names["res1"] = &master_preset.preset.res1;
            parameter_values_for_names["res2"] = &master_preset.preset.res2;
            parameter_values_for_names["res3"] = &master_preset.preset.res3;
            parameter_values_for_names["res4"] = &master_preset.preset.res4;
            parameter_values_for_names["res5"] = &master_preset.preset.res5;
            parameter_values_for_names["res6"] = &master_preset.preset.res6;
            parameter_values_for_names["res7"] = &master_preset.preset.res7;
            parameter_values_for_names["res8"] = &master_preset.preset.res8;
            parameter_values_for_names["res9"] = &master_preset.preset.res9;
            parameter_values_for_names["res10"] = &master_preset.preset.res10;
            parameter_values_for_names["res11"] = &master_preset.preset.res11;
            parameter_values_for_names["res12"] = &master_preset.preset.res12;
            parameter_values_for_names["res13"] = &master_preset.preset.res13;
            parameter_values_for_names["res14"] = &master_preset.preset.res14;
            parameter_values_for_names["res15"] = &master_preset.preset.res15;
            parameter_values_for_names["res16"] = &master_preset.preset.res16;
            parameter_values_for_names["res17"] = &master_preset.preset.res17;
            parameter_values_for_names["res18"] = &master_preset.preset.res18;
            parameter_values_for_names["res19"] = &master_preset.preset.res19;
            parameter_values_for_names["res20"] = &master_preset.preset.res20;
            parameter_values_for_names["res21"] = &master_preset.preset.res21;
            parameter_values_for_names["res22"] = &master_preset.preset.res22;
            parameter_values_for_names["res23"] = &master_preset.preset.res23;
            parameter_values_for_names["res24"] = &master_preset.preset.res24;
            parameter_values_for_names["res25"] = &master_preset.preset.res25;
            parameter_values_for_names["FB"] = &master_preset.preset.FB;
            parameter_values_for_names["DFact"] = &master_preset.preset.DFact;
            parameter_values_for_names["Q"] = &master_preset.preset.Q;
            parameter_values_for_names["ERamp"] = &master_preset.preset.ERamp;
            parameter_values_for_names["ERselect"] = &master_preset.preset.ERSelect;
            parameter_values_for_names["EQselect"] = &master_preset.preset.EQSelect;
            // EqualizerPreset.
            parameter_values_for_names["eq1"] = &master_preset.equalizerPreset.gain[0];
            parameter_values_for_names["eq2"] = &master_preset.equalizerPreset.gain[1];
            parameter_values_for_names["eq3"] = &master_preset.equalizerPreset.gain[2];
            parameter_values_for_names["eq4"] = &master_preset.equalizerPreset.gain[3];
            parameter_values_for_names["eq5"] = &master_preset.equalizerPreset.gain[4];
            parameter_values_for_names["eq6"] = &master_preset.equalizerPreset.gain[5];
            parameter_values_for_names["eq7"] = &master_preset.equalizerPreset.gain[6];
            parameter_values_for_names["eq8"] = &master_preset.equalizerPreset.gain[7];
            parameter_values_for_names["eq9"] = &master_preset.equalizerPreset.gain[8];
            parameter_values_for_names["eq10"] = &master_preset.equalizerPreset.gain[9];
            // EarlyReturnPreset values are not user-configurable.
            // The following parameters are not stored in a preset struct.
            parameter_values_for_names["wet"] = &master_preset.effect_mix_wet;
            parameter_values_for_names["random"] = &master_preset.krand;
            parameter_values_for_names["rslow"] = &master_preset.krslow;
            parameter_values_for_names["rfast"] = &master_preset.krfast;
            parameter_values_for_names["rmax"] = &master_preset.krmax;
            parameter_values_for_names["FBclear"] = &master_preset.clear_delays;
            parameter_values_for_names["random_seed"] = &master_preset.random_seed;
            parameter_values_for_names["print"] = &master_preset.print;
            input_left = 0;
            input_right = 0;
            aL = 0.;
            aR = 0.;
            aAU = 0.;
            aAR = 0.;
            aAD = 0.;
            aAL = 0.;
            aBU = 0.;
            aBR = 0.;
            aBD = 0.;
            aBL = 0.;
            aCU = 0.;
            aCR = 0.;
            aCD = 0.;
            aCL = 0.;
            aDU = 0.;
            aDR = 0.;
            aDD = 0.;
            aDL = 0.;
            aEU = 0.;
            aER = 0.;
            aED = 0.;
            aEL = 0.;
            aFU = 0.;
            aFR = 0.;
            aFD = 0.;
            aFL = 0.;
            aGU = 0.;
            aGR = 0.;
            aGD = 0.;
            aGL = 0.;
            aHU = 0.;
            aHR = 0.;
            aHD = 0.;
            aHL = 0.;
            aIU = 0.;
            aIR = 0.;
            aID = 0.;
            aIL = 0.;
            aJU = 0.;
            aJR = 0.;
            aJD = 0.;
            aJL = 0.;
            aKU = 0.;
            aKR = 0.;
            aKD = 0.;
            aKL = 0.;
            aLU = 0.;
            aLR = 0.;
            aLD = 0.;
            aLL = 0.;
            aMU = 0.;
            aMR = 0.;
            aMD = 0.;
            aML = 0.;
            aNU = 0.;
            aNR = 0.;
            aND = 0.;
            aNL = 0.;
            aOU = 0.;
            aOR = 0.;
            aOD = 0.;
            aOL = 0.;
            aPU = 0.;
            aPR = 0.;
            aPD = 0.;
            aPL = 0.;
            aQU = 0.;
            aQR = 0.;
            aQD = 0.;
            aQL = 0.;
            aRU = 0.;
            aRR = 0.;
            aRD = 0.;
            aRL = 0.;
            aSU = 0.;
            aSR = 0.;
            aSD = 0.;
            aSL = 0.;
            aTU = 0.;
            aTR = 0.;
            aTD = 0.;
            aTL = 0.;
            aUU = 0.;
            aUR = 0.;
            aUD = 0.;
            aUL = 0.;
            aVU = 0.;
            aVR = 0.;
            aVD = 0.;
            aVL = 0.;
            aWU = 0.;
            aWR = 0.;
            aWD = 0.;
            aWL = 0.;
            aXU = 0.;
            aXR = 0.;
            aXD = 0.;
            aXL = 0.;
            aYU = 0.;
            aYR = 0.;
            aYD = 0.;
            aYL = 0.;
            delay_variation1 = 0.;
            delay_variation2 = 0.;
            delay_variation3 = 0.;
            delay_variation4 = 0.;
            delay_variation5 = 0.;
            delay_variation6 = 0.;
            delay_variation7 = 0.;
            delay_variation8 = 0.;
            delay_variation9 = 0.;
            delay_variation10 = 0.;
            delay_variation11 = 0.;
            delay_variation12 = 0.;
            delay_variation13 = 0.;
            delay_variation14 = 0.;
            delay_variation15 = 0.;
            delay_variation16 = 0.;
            delay_variation17 = 0.;
            delay_variation18 = 0.;
            delay_variation19 = 0.;
            delay_variation20 = 0.;
            delay_variation21 = 0.;
            delay_variation22 = 0.;
            delay_variation23 = 0.;
            delay_variation24 = 0.;
            delay_variation25 = 0.;
            reverberated_left = 0.;
            reverberated_right = 0.;
        };
    };
    void read_opcode_parameters(CSOUND *csound, MYFLT* parameters[VARGMAX-4], int parameter_count) {
        for (int parameter_index = 0; parameter_index < (parameter_count - 3); ) {
            STRINGDAT *name_stringdat = (STRINGDAT *) parameters[parameter_index];
            std::string name = name_stringdat->data;
            // We only update a parameter if its value has changed.
            ++parameter_index;
            if (name == "ERSelect") {
                STRINGDAT *value_stringdat = (STRINGDAT *) parameters[parameter_index];
                if (master_preset.earlyReturnPresetName == value_stringdat->data) {
                    set_early_return_preset(value_stringdat->data);
                }
            } else if (name == "EQSelect") {
                STRINGDAT *value_stringdat = (STRINGDAT *) parameters[parameter_index];
                if (master_preset.equalizerPresetName == value_stringdat->data) {
                    set_equalizer_preset(value_stringdat->data);
                }
            } else {
                MYFLT *new_value_ = parameters[parameter_index];
                float new_value = (float) *new_value_;
                auto current_value = parameter_values_for_names[name];
                if (new_value != *current_value) {
                    *current_value = new_value;
                }
            }
            ++parameter_index;
        }        
        if (master_preset.print != 0. && printed == false) {
            print_parameters();
            printed = true;
        }
    };
    void set_preset(const char *name) {
        master_preset.preset = presets()[name];
        master_preset.presetName = name;
        auto early_return_preset_index = int(master_preset.preset.ERSelect) - 1;
        auto early_return_preset_name = early_return_presets_for_numbers[early_return_preset_index];
        set_early_return_preset(early_return_preset_name.c_str());
        auto equalizer_preset_index = int(master_preset.preset.EQSelect) - 1;
        auto equalizer_preset_name = equalizer_presets_for_numbers[equalizer_preset_index];
        set_equalizer_preset(equalizer_preset_name.c_str());
        if (master_preset.random_seed > 0) {
            generator.seed((unsigned int) master_preset.random_seed);
        }
        for (int i = 0; i < 25; ++i) {
            randomize_delay[i].initialize(csound, master_preset, &generator);
            mesh_equalizer[i].initialize(csound, master_preset);
        }
    };
    void print_parameters() {
        csound->Message(csound, "MVerb preset: %s\n", master_preset.presetName.c_str());
        for (auto entry : parameter_values_for_names) {
            csound->Message(csound, "  %15s: %12.6f\n", entry.first.c_str(), *entry.second);
        };
        auto early_return_preset_index = int(master_preset.preset.ERSelect) - 1;
        auto early_return_preset_name = early_return_presets_for_numbers[early_return_preset_index];
        csound->Message(csound, "  Early reflections: %s\n", early_return_preset_name.c_str());
        for (int i = 0, n = master_preset.earlyReturnPreset.taps_left.size(); i < n; ++i) {
            csound->Message(csound, "    Left tap  %3d: %12.6f\n", i, master_preset.earlyReturnPreset.taps_left[i]);
        }
        for (int i = 0, n = master_preset.earlyReturnPreset.taps_right.size(); i < n; ++i) {
            csound->Message(csound, "    Right tap %3d: %12.6f\n", i, master_preset.earlyReturnPreset.taps_right[i]);
        }
        auto equalizer_preset_index = int(master_preset.preset.EQSelect) - 1;
        auto equalizer_preset_name = equalizer_presets_for_numbers[equalizer_preset_index];
        csound->Message(csound, "  Equalizer: %s\n", equalizer_preset_name.c_str());
        for (int i = 0; i < 10; i++) {
            csound->Message(csound, "         Band %3d: %12.6f\n", i, master_preset.equalizerPreset.gain[i]);
        }         
    };
    void set_early_return_preset(const char *name) {
        master_preset.earlyReturnPreset = earlyReturnPresets()[name];
        multitaps_left.initialize(csound, master_preset.earlyReturnPreset.taps_left, master_preset);
        multitaps_right.initialize(csound, master_preset.earlyReturnPreset.taps_right, master_preset);
    };
    void set_equalizer_preset(const char *name) {
        master_preset.equalizerPreset = equalizerPresets()[name];
    };
    void operator () (CSOUND *csound,
                      /* Outputs: */
                      MYFLT &out_left,
                      MYFLT &out_right,
                      /* Inputs: */
                      MYFLT a1,
                      MYFLT a2) {
        master_preset.effect_mix_dry = 1. - master_preset.effect_mix_wet;
        if (master_preset.effect_mix_wet == 0.) {
            out_left = a1;
            out_right = a2;
        } else {
            master_preset.preset.FB = master_preset.preset.FB * (1. - master_preset.clear_delays);       
            input_left = blockdc_in_left((float) a1);
            input_right = blockdc_in_right((float) a2);
            aL = multitaps_left(input_left);
            aR = multitaps_right(input_right);
            // CSD uses vdelay with delay time in milliseconds, but here we 
            // use gam::Delay with delay time in seconds, so the code must 
            // change.
            delay_variation1  = master_preset.preset.DFact * (1. / master_preset.preset.res1 );
            delay_variation1  = randomize_delay[0 ](delay_variation1 );
            delay_variation2  = master_preset.preset.DFact * (1. / master_preset.preset.res2 );
            delay_variation2  = randomize_delay[1 ](delay_variation2 );
            delay_variation3  = master_preset.preset.DFact * (1. / master_preset.preset.res3 );
            delay_variation3  = randomize_delay[2 ](delay_variation3 );
            delay_variation4  = master_preset.preset.DFact * (1. / master_preset.preset.res4 );
            delay_variation4  = randomize_delay[3 ](delay_variation4 );
            delay_variation5  = master_preset.preset.DFact * (1. / master_preset.preset.res5 );
            delay_variation5  = randomize_delay[4 ](delay_variation5 );
            delay_variation6  = master_preset.preset.DFact * (1. / master_preset.preset.res6 );
            delay_variation6  = randomize_delay[5 ](delay_variation6 );
            delay_variation7  = master_preset.preset.DFact * (1. / master_preset.preset.res7 );
            delay_variation7  = randomize_delay[6 ](delay_variation7 );
            delay_variation8  = master_preset.preset.DFact * (1. / master_preset.preset.res8 );
            delay_variation8  = randomize_delay[7 ](delay_variation8 );
            delay_variation9  = master_preset.preset.DFact * (1. / master_preset.preset.res9 );
            delay_variation9  = randomize_delay[8 ](delay_variation9 );
            delay_variation10 = master_preset.preset.DFact * (1. / master_preset.preset.res10);
            delay_variation10 = randomize_delay[9 ](delay_variation10);
            delay_variation11 = master_preset.preset.DFact * (1. / master_preset.preset.res11);
            delay_variation11 = randomize_delay[10](delay_variation11);
            delay_variation12 = master_preset.preset.DFact * (1. / master_preset.preset.res12);
            delay_variation12 = randomize_delay[11](delay_variation12);
            delay_variation13 = master_preset.preset.DFact * (1. / master_preset.preset.res13);
            delay_variation13 = randomize_delay[12](delay_variation13);
            delay_variation14 = master_preset.preset.DFact * (1. / master_preset.preset.res14);
            delay_variation14 = randomize_delay[13](delay_variation14);
            delay_variation15 = master_preset.preset.DFact * (1. / master_preset.preset.res15);
            delay_variation15 = randomize_delay[14](delay_variation15);
            delay_variation16 = master_preset.preset.DFact * (1. / master_preset.preset.res16);
            delay_variation16 = randomize_delay[15](delay_variation16);
            delay_variation17 = master_preset.preset.DFact * (1. / master_preset.preset.res17);
            delay_variation17 = randomize_delay[16](delay_variation17);
            delay_variation18 = master_preset.preset.DFact * (1. / master_preset.preset.res18);
            delay_variation18 = randomize_delay[17](delay_variation18);
            delay_variation19 = master_preset.preset.DFact * (1. / master_preset.preset.res19);
            delay_variation19 = randomize_delay[18](delay_variation19);
            delay_variation20 = master_preset.preset.DFact * (1. / master_preset.preset.res20);
            delay_variation20 = randomize_delay[19](delay_variation20);
            delay_variation21 = master_preset.preset.DFact * (1. / master_preset.preset.res21);
            delay_variation21 = randomize_delay[20](delay_variation21);
            delay_variation22 = master_preset.preset.DFact * (1. / master_preset.preset.res22);
            delay_variation22 = randomize_delay[21](delay_variation22);
            delay_variation23 = master_preset.preset.DFact * (1. / master_preset.preset.res23);
            delay_variation23 = randomize_delay[22](delay_variation23);
            delay_variation24 = master_preset.preset.DFact * (1. / master_preset.preset.res24);
            delay_variation24 = randomize_delay[23](delay_variation24);
            delay_variation25 = master_preset.preset.DFact * (1. / master_preset.preset.res25);
            delay_variation25 = randomize_delay[24](delay_variation25);
            mesh_equalizer[ 0](aAU, aAR, aAD, aAL, aAU, aBL, aFU, aAL, delay_variation1, master_preset.preset.FB);
            mesh_equalizer[ 1](aBU, aBR, aBD, aBL, aBU, aCL, aGU, aAR, delay_variation2, master_preset.preset.FB);
            mesh_equalizer[ 2](aCU, aCR, aCD, aCL, aCU, aDL, aHU, aBR, delay_variation3, master_preset.preset.FB);
            mesh_equalizer[ 3](aDU, aDR, aDD, aDL, aDU, aEL, aIU, aCR, delay_variation4, master_preset.preset.FB);
            mesh_equalizer[ 4](aEU, aER, aED, aEL, aEU, aER, aJU, aDR, delay_variation5, master_preset.preset.FB);
            mesh_equalizer[ 5](aFU, aFR, aFD, aFL, aAD, aGL, aKU, aFL, delay_variation6, master_preset.preset.FB);
            mesh_equalizer[ 6](aGU, aGR, aGD, aGL, aBD, aHL, aLU, aFR, delay_variation7, master_preset.preset.FB);
            mesh_equalizer[ 7](aHU, aHR, aHD, aHL, aCD, aIL, aMU, aGR, delay_variation8, master_preset.preset.FB);
            mesh_equalizer[ 8](aIU, aIR, aID, aIL, aDD, aJL, aNU, aHR, delay_variation9, master_preset.preset.FB);
            mesh_equalizer[ 9](aJU, aJR, aJD, aJL, aED, aJR, aOU, aIR, delay_variation10, master_preset.preset.FB);
            mesh_equalizer[10](aKU, aKR, aKD, aKL, aFD, aLL, aPU, aKL, delay_variation11, master_preset.preset.FB);
            mesh_equalizer[11](aLU, aLR, aLD, aLL, input_left  + aL + aGD, aML, aQU, aKR, delay_variation12, master_preset.preset.FB);
            mesh_equalizer[12](aMU, aMR, aMD, aML, aHD, aNL, aRU, aLR, delay_variation13, master_preset.preset.FB);
            mesh_equalizer[13](aNU, aNR, aND, aNL, input_right + aR + aID, aOL, aSU, aMR, delay_variation14, master_preset.preset.FB);
            mesh_equalizer[14](aOU, aOR, aOD, aOL, aJD, aOR, aTU, aNR, delay_variation15, master_preset.preset.FB);
            mesh_equalizer[15](aPU, aPR, aPD, aPL, aKD, aQL, aUU, aPL, delay_variation16, master_preset.preset.FB);
            mesh_equalizer[16](aQU, aQR, aQD, aQL, aLD, aRL, aVU, aPR, delay_variation17, master_preset.preset.FB);
            mesh_equalizer[17](aRU, aRR, aRD, aRL, aMD, aSL, aWU, aQR, delay_variation18, master_preset.preset.FB);
            mesh_equalizer[18](aSU, aSR, aSD, aSL, aND, aTL, aXU, aRR, delay_variation19, master_preset.preset.FB);
            mesh_equalizer[19](aTU, aTR, aTD, aTL, aOD, aTR, aYU, aSR, delay_variation20, master_preset.preset.FB);
            mesh_equalizer[20](aUU, aUR, aUD, aUL, aPD, aVL, aUD, aUL, delay_variation21, master_preset.preset.FB);
            mesh_equalizer[21](aVU, aVR, aVD, aVL, aQD, aWL, aVD, aUR, delay_variation22, master_preset.preset.FB);
            mesh_equalizer[22](aWU, aWR, aWD, aWL, aRD, aXL, aWD, aVR, delay_variation23, master_preset.preset.FB);
            mesh_equalizer[23](aXU, aXR, aXD, aXL, aSD, aYL, aXD, aWR, delay_variation24, master_preset.preset.FB);
            mesh_equalizer[24](aYU, aYR, aYD, aYL, aTD, aYR, aYD, aXR, delay_variation25, master_preset.preset.FB);
            reverberated_left = blockdc_out_left(aGL);
            reverberated_right = blockdc_out_right(aIR);
            out_left  = (reverberated_left * master_preset.effect_mix_wet) + (input_left * master_preset.effect_mix_dry);
            out_right = (reverberated_right * master_preset.effect_mix_wet) + (input_right * master_preset.effect_mix_dry);
       }
     };
};

struct MVerbsForCsounds 
{
    std::mutex mutex_;
    std::multimap<CSOUND *, MVerb *> mverbs_for_csounds;
    void add(CSOUND *csound, MVerb *mverb) {
        std::lock_guard<std::mutex> guard(mutex_);
        mverbs_for_csounds.insert({csound, mverb});
    }
    void del(CSOUND *csound) {
        std::lock_guard<std::mutex> guard(mutex_);
        auto begin_ = mverbs_for_csounds.lower_bound(csound);
        auto end_ = mverbs_for_csounds.upper_bound(csound);
        for (auto it = begin_; it != end_; ++it) {
            delete it->second;
        }
        mverbs_for_csounds.erase(csound);
    }
};

static MVerbsForCsounds &mverbs() {
    static MVerbsForCsounds mverbs_;
    return mverbs_;
};

class MVerbOpcode  : public csound::OpcodeBase<MVerbOpcode>
{
public:
    // Outputs.
    MYFLT *aout_left;
    MYFLT *aout_right;
    // Inputs.
    MYFLT *ain_left;
    MYFLT *ain_right;
    STRINGDAT *preset;
    // These will be arbitrary name-value pairs.
    MYFLT *parameters[VARGMAX-4];
    // State. This C++ object does all the real work.
    MVerb *mverb;
    int init(CSOUND *csound)
    {
        if (mverb == nullptr) {
            mverb = new MVerb();
            mverbs().add(csound, mverb);
        }
        mverb->initialize(csound);
        mverb->set_preset(preset->data);
        return OK;
    }
    int kontrol(CSOUND *csound)
    {
        // The Csound control channels in the Cabbage version of MVerb here 
        // become opcode parameters. Many have default values or are also 
        // set upon choice of preset. Hence the MVerb opcode signature is 
        // "aaSN", i.e. the input signal, the initial preset name, and any 
        // combination of what used to be channels, as name-value pairs. The 
        // prints opcode shows how to handle "N", i.e. an arbitrary list of 
        // opcode parameters.
        if (mverb != nullptr) {
            mverb->read_opcode_parameters(csound, parameters, opds.optext->t.inArgCount);
        }
        int frame_index = 0;
        for( ; frame_index < kperiodOffset(); ++frame_index) {
            aout_right[frame_index] = 0;
            aout_left[frame_index] = 0;
        }
        for( ; frame_index < kperiodEnd(); ++frame_index) {
            if (mverb != nullptr) {
                (*mverb)(csound,
                         aout_left[frame_index],
                         aout_right[frame_index],
                         ain_left[frame_index],
                         ain_right[frame_index]);
            } else {
                aout_left[frame_index] = ain_left[frame_index];
                aout_right[frame_index] = ain_right[frame_index];
            }
        }
        for( ; frame_index < ksmps(); ++frame_index) {
            aout_right[frame_index] = 0;
            aout_left[frame_index] = 0;
        }
        return OK;
    }
};

extern "C" {

    PUBLIC int csoundModuleInit_mverb(CSOUND *csound)
    {
        int status = csound->AppendOpcode(csound,
                                          (char*)"MVerb",
                                          sizeof(MVerbOpcode),
                                          0,
                                          3,
                                          (char*)"aa",
                                          (char*)"aaSN",
                                          (int(*)(CSOUND*,void*)) MVerbOpcode::init_,
                                          (int(*)(CSOUND*,void*)) MVerbOpcode::kontrol_,
                                          (int (*)(CSOUND*,void*)) 0);
        return status;
    }

    PUBLIC int csoundModuleDestroy_mverb(CSOUND *csound)
    {
        mverbs().del(csound);
        return 0;
    }

#ifndef INIT_STATIC_MODULES
    PUBLIC int csoundModuleCreate(CSOUND *csound)
    {
        return 0;
    }

    PUBLIC int csoundModuleInit(CSOUND *csound)
    {
        return csoundModuleInit_mverb(csound);
    }

    PUBLIC int csoundModuleDestroy(CSOUND *csound)
    {
        return csoundModuleDestroy_mverb(csound);
    }
#endif
}
