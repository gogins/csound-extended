var numeric = {
    version: "1.2.6",
    bench: function(r, n) {
        var e, i, t;
        for (void 0 === n && (n = 15), i = .5, e = new Date;;) {
            for (t = i *= 2; t > 3; t -= 4) r(), r(), r(), r();
            for (; t > 0;) r(), t--;
            if (new Date - e > n) break
        }
        for (t = i; t > 3; t -= 4) r(), r(), r(), r();
        for (; t > 0;) r(), t--;
        return 1e3 * (3 * i - 1) / (new Date - e)
    },
    _myIndexOf: function(r) {
        var n, e = this.length;
        for (n = 0; n < e; ++n)
            if (this[n] === r) return n;
        return -1
    }
};
numeric.myIndexOf = Array.prototype.indexOf ? Array.prototype.indexOf : numeric._myIndexOf, numeric.Function = Function, numeric.precision = 4, numeric.largeArray = 50, numeric.prettyPrint = function(r) {
        var n = [];
        return function r(e) {
            var i;
            if (void 0 === e) return n.push(Array(numeric.precision + 8).join(" ")), !1;
            if ("string" == typeof e) return n.push('"' + e + '"'), !1;
            if ("boolean" == typeof e) return n.push(e.toString()), !1;
            if ("number" == typeof e) {
                var t = function r(n) {
                        if (0 === n) return "0";
                        if (isNaN(n)) return "NaN";
                        if (n < 0) return "-" + r(-n);
                        if (isFinite(n)) {
                            var e = Math.floor(Math.log(n) / Math.log(10)),
                                i = n / Math.pow(10, e),
                                t = i.toPrecision(numeric.precision);
                            return 10 === parseFloat(t) && (e++, t = (i = 1).toPrecision(numeric.precision)), parseFloat(t).toString() + "e" + e.toString()
                        }
                        return "Infinity"
                    }(e),
                    u = e.toPrecision(numeric.precision),
                    o = parseFloat(e.toString()).toString(),
                    c = [t, u, o, parseFloat(u).toString(), parseFloat(o).toString()];
                for (i = 1; i < c.length; i++) c[i].length < t.length && (t = c[i]);
                return n.push(Array(numeric.precision + 8 - t.length).join(" ") + t), !1
            }
            if (null === e) return n.push("null"), !1;
            if ("function" == typeof e) {
                n.push(e.toString());
                var m = !1;
                for (i in e) e.hasOwnProperty(i) && (n.push(m ? ",\n" : "\n{"), m = !0, n.push(i), n.push(": \n"), r(e[i]));
                return m && n.push("}\n"), !0
            }
            if (e instanceof Array) {
                if (e.length > numeric.largeArray) return n.push("...Large Array..."), !0;
                for (m = !1, n.push("["), i = 0; i < e.length; i++) i > 0 && (n.push(","), m && n.push("\n ")), m = r(e[i]);
                return n.push("]"), !0
            }
            for (i in n.push("{"), m = !1, e) e.hasOwnProperty(i) && (m && n.push(",\n"), m = !0, n.push(i), n.push(": \n"), r(e[i]));
            return n.push("}"), !0
        }(r), n.join("")
    }, numeric.parseDate = function(r) {
        return function r(n) {
            if ("string" == typeof n) return Date.parse(n.replace(/-/g, "/"));
            if (!(n instanceof Array)) throw new Error("parseDate: parameter must be arrays of strings");
            var e, i = [];
            for (e = 0; e < n.length; e++) i[e] = r(n[e]);
            return i
        }(r)
    }, numeric.parseFloat = function(r) {
        return function r(n) {
            if ("string" == typeof n) return parseFloat(n);
            if (!(n instanceof Array)) throw new Error("parseFloat: parameter must be arrays of strings");
            var e, i = [];
            for (e = 0; e < n.length; e++) i[e] = r(n[e]);
            return i
        }(r)
    }, numeric.parseCSV = function(r) {
        var n, e, i, t = r.split("\n"),
            u = [],
            o = /(([^'",]*)|('[^']*')|("[^"]*")),/g,
            c = /^\s*(([+-]?[0-9]+(\.[0-9]*)?(e[+-]?[0-9]+)?)|([+-]?[0-9]*(\.[0-9]+)?(e[+-]?[0-9]+)?))\s*$/,
            m = 0;
        for (e = 0; e < t.length; e++) {
            var a, f = (t[e] + ",").match(o);
            if (f.length > 0) {
                for (u[m] = [], n = 0; n < f.length; n++) a = (i = f[n]).substr(0, i.length - 1), u[m][n] = c.test(a) ? parseFloat(a) : a;
                m++
            }
        }
        return u
    }, numeric.toCSV = function(r) {
        var n, e, i, t, u;
        for (i = numeric.dim(r)[0], u = [], n = 0; n < i; n++) {
            for (t = [], e = 0; e < i; e++) t[e] = r[n][e].toString();
            u[n] = t.join(", ")
        }
        return u.join("\n") + "\n"
    }, numeric.getURL = function(r) {
        var n = new XMLHttpRequest;
        return n.open("GET", r, !1), n.send(), n
    }, numeric.imageURL = function(r) {
        function n(r, n, e) {
            void 0 === n && (n = 0), void 0 === e && (e = r.length);
            var i, t = [0, 1996959894, 3993919788, 2567524794, 124634137, 1886057615, 3915621685, 2657392035, 249268274, 2044508324, 3772115230, 2547177864, 162941995, 2125561021, 3887607047, 2428444049, 498536548, 1789927666, 4089016648, 2227061214, 450548861, 1843258603, 4107580753, 2211677639, 325883990, 1684777152, 4251122042, 2321926636, 335633487, 1661365465, 4195302755, 2366115317, 997073096, 1281953886, 3579855332, 2724688242, 1006888145, 1258607687, 3524101629, 2768942443, 901097722, 1119000684, 3686517206, 2898065728, 853044451, 1172266101, 3705015759, 2882616665, 651767980, 1373503546, 3369554304, 3218104598, 565507253, 1454621731, 3485111705, 3099436303, 671266974, 1594198024, 3322730930, 2970347812, 795835527, 1483230225, 3244367275, 3060149565, 1994146192, 31158534, 2563907772, 4023717930, 1907459465, 112637215, 2680153253, 3904427059, 2013776290, 251722036, 2517215374, 3775830040, 2137656763, 141376813, 2439277719, 3865271297, 1802195444, 476864866, 2238001368, 4066508878, 1812370925, 453092731, 2181625025, 4111451223, 1706088902, 314042704, 2344532202, 4240017532, 1658658271, 366619977, 2362670323, 4224994405, 1303535960, 984961486, 2747007092, 3569037538, 1256170817, 1037604311, 2765210733, 3554079995, 1131014506, 879679996, 2909243462, 3663771856, 1141124467, 855842277, 2852801631, 3708648649, 1342533948, 654459306, 3188396048, 3373015174, 1466479909, 544179635, 3110523913, 3462522015, 1591671054, 702138776, 2966460450, 3352799412, 1504918807, 783551873, 3082640443, 3233442989, 3988292384, 2596254646, 62317068, 1957810842, 3939845945, 2647816111, 81470997, 1943803523, 3814918930, 2489596804, 225274430, 2053790376, 3826175755, 2466906013, 167816743, 2097651377, 4027552580, 2265490386, 503444072, 1762050814, 4150417245, 2154129355, 426522225, 1852507879, 4275313526, 2312317920, 282753626, 1742555852, 4189708143, 2394877945, 397917763, 1622183637, 3604390888, 2714866558, 953729732, 1340076626, 3518719985, 2797360999, 1068828381, 1219638859, 3624741850, 2936675148, 906185462, 1090812512, 3747672003, 2825379669, 829329135, 1181335161, 3412177804, 3160834842, 628085408, 1382605366, 3423369109, 3138078467, 570562233, 1426400815, 3317316542, 2998733608, 733239954, 1555261956, 3268935591, 3050360625, 752459403, 1541320221, 2607071920, 3965973030, 1969922972, 40735498, 2617837225, 3943577151, 1913087877, 83908371, 2512341634, 3803740692, 2075208622, 213261112, 2463272603, 3855990285, 2094854071, 198958881, 2262029012, 4057260610, 1759359992, 534414190, 2176718541, 4139329115, 1873836001, 414664567, 2282248934, 4279200368, 1711684554, 285281116, 2405801727, 4167216745, 1634467795, 376229701, 2685067896, 3608007406, 1308918612, 956543938, 2808555105, 3495958263, 1231636301, 1047427035, 2932959818, 3654703836, 1088359270, 936918e3, 2847714899, 3736837829, 1202900863, 817233897, 3183342108, 3401237130, 1404277552, 615818150, 3134207493, 3453421203, 1423857449, 601450431, 3009837614, 3294710456, 1567103746, 711928724, 3020668471, 3272380065, 1510334235, 755167117],
                u = -1;
            for (i = n; i < e; i++) u = u >>> 8 ^ t[255 & (u ^ r[i])];
            return -1 ^ u
        }
        var e, i, t, u, o, c, m, a, f, s, h = r[0].length,
            p = r[0][0].length,
            l = [137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, p >> 24 & 255, p >> 16 & 255, p >> 8 & 255, 255 & p, h >> 24 & 255, h >> 16 & 255, h >> 8 & 255, 255 & h, 8, 2, 0, 0, 0, -1, -2, -3, -4, -5, -6, -7, -8, 73, 68, 65, 84, 8, 29];
        for (s = n(l, 12, 29), l[29] = s >> 24 & 255, l[30] = s >> 16 & 255, l[31] = s >> 8 & 255, l[32] = 255 & s, e = 1, i = 0, m = 0; m < h; m++) {
            for (l.push(m < h - 1 ? 0 : 1), c = 3 * p + 1 + (0 === m) >> 8 & 255, l.push(o = 3 * p + 1 + (0 === m) & 255), l.push(c), l.push(255 & ~o), l.push(255 & ~c), 0 === m && l.push(0), a = 0; a < p; a++)
                for (t = 0; t < 3; t++) i = (i + (e = (e + (o = (o = r[t][m][a]) > 255 ? 255 : o < 0 ? 0 : Math.round(o))) % 65521)) % 65521, l.push(o);
            l.push(0)
        }
        return l.push((f = (i << 16) + e) >> 24 & 255), l.push(f >> 16 & 255), l.push(f >> 8 & 255), l.push(255 & f), l[33] = (u = l.length - 41) >> 24 & 255, l[34] = u >> 16 & 255, l[35] = u >> 8 & 255, l[36] = 255 & u, s = n(l, 37), l.push(s >> 24 & 255), l.push(s >> 16 & 255), l.push(s >> 8 & 255), l.push(255 & s), l.push(0), l.push(0), l.push(0), l.push(0), l.push(73), l.push(69), l.push(78), l.push(68), l.push(174), l.push(66), l.push(96), l.push(130), "data:image/png;base64," + function(r) {
            var n, e, i, t, u, o, c, m = r.length,
                a = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",
                f = "";
            for (n = 0; n < m; n += 3) u = ((3 & (e = r[n])) << 4) + ((i = r[n + 1]) >> 4), o = ((15 & i) << 2) + ((t = r[n + 2]) >> 6), c = 63 & t, n + 1 >= m ? o = c = 64 : n + 2 >= m && (c = 64), f += a.charAt(e >> 2) + a.charAt(u) + a.charAt(o) + a.charAt(c);
            return f
        }(l)
    }, numeric._dim = function(r) {
        for (var n = [];
            "object" == typeof r;) n.push(r.length), r = r[0];
        return n
    }, numeric.dim = function(r) {
        var n;
        return "object" == typeof r ? "object" == typeof(n = r[0]) ? "object" == typeof n[0] ? numeric._dim(r) : [r.length, n.length] : [r.length] : []
    }, numeric.mapreduce = function(r, n) {
        return Function("x", "accum", "_s", "_k", 'if(typeof accum === "undefined") accum = ' + n + ';\nif(typeof x === "number") { var xi = x; ' + r + '; return accum; }\nif(typeof _s === "undefined") _s = numeric.dim(x);\nif(typeof _k === "undefined") _k = 0;\nvar _n = _s[_k];\nvar i,xi;\nif(_k < _s.length-1) {\n    for(i=_n-1;i>=0;i--) {\n        accum = arguments.callee(x[i],accum,_s,_k+1);\n    }    return accum;\n}\nfor(i=_n-1;i>=1;i-=2) { \n    xi = x[i];\n    ' + r + ";\n    xi = x[i-1];\n    " + r + ";\n}\nif(i === 0) {\n    xi = x[i];\n    " + r + "\n}\nreturn accum;")
    }, numeric.mapreduce2 = function(r, n) {
        return Function("x", "var n = x.length;\nvar i,xi;\n" + n + ";\nfor(i=n-1;i!==-1;--i) { \n    xi = x[i];\n    " + r + ";\n}\nreturn accum;")
    }, numeric.same = function r(n, e) {
        var i, t;
        if (!(n instanceof Array && e instanceof Array)) return !1;
        if ((t = n.length) !== e.length) return !1;
        for (i = 0; i < t; i++)
            if (n[i] !== e[i]) {
                if ("object" != typeof n[i]) return !1;
                if (!r(n[i], e[i])) return !1
            } return !0
    }, numeric.rep = function(r, n, e) {
        void 0 === e && (e = 0);
        var i, t = r[e],
            u = Array(t);
        if (e === r.length - 1) {
            for (i = t - 2; i >= 0; i -= 2) u[i + 1] = n, u[i] = n;
            return -1 === i && (u[0] = n), u
        }
        for (i = t - 1; i >= 0; i--) u[i] = numeric.rep(r, n, e + 1);
        return u
    }, numeric.dotMMsmall = function(r, n) {
        var e, i, t, u, o, c, m, a, f, s, h;
        for (u = r.length, o = n.length, c = n[0].length, m = Array(u), e = u - 1; e >= 0; e--) {
            for (a = Array(c), f = r[e], t = c - 1; t >= 0; t--) {
                for (s = f[o - 1] * n[o - 1][t], i = o - 2; i >= 1; i -= 2) s += f[i] * n[i][t] + f[h = i - 1] * n[h][t];
                0 === i && (s += f[0] * n[0][t]), a[t] = s
            }
            m[e] = a
        }
        return m
    }, numeric._getCol = function(r, n, e) {
        var i;
        for (i = r.length - 1; i > 0; --i) e[i] = r[i][n], e[--i] = r[i][n];
        0 === i && (e[0] = r[0][n])
    }, numeric.dotMMbig = function(r, n) {
        var e, i, t = numeric._getCol,
            u = n.length,
            o = Array(u),
            c = r.length,
            m = n[0].length,
            a = new Array(c),
            f = numeric.dotVV;
        for (--u, e = --c; - 1 !== e; --e) a[e] = Array(m);
        for (e = --m; - 1 !== e; --e)
            for (t(n, e, o), i = c; - 1 !== i; --i) a[i][e] = f(r[i], o);
        return a
    }, numeric.dotMV = function(r, n) {
        var e, i = r.length,
            t = Array(i),
            u = numeric.dotVV;
        for (e = i - 1; e >= 0; e--) t[e] = u(r[e], n);
        return t
    }, numeric.dotVM = function(r, n) {
        var e, i, t, u, o, c, m;
        for (t = r.length, u = n[0].length, o = Array(u), i = u - 1; i >= 0; i--) {
            for (c = r[t - 1] * n[t - 1][i], e = t - 2; e >= 1; e -= 2) c += r[e] * n[e][i] + r[m = e - 1] * n[m][i];
            0 === e && (c += r[0] * n[0][i]), o[i] = c
        }
        return o
    }, numeric.dotVV = function(r, n) {
        var e, i, t = r.length,
            u = r[t - 1] * n[t - 1];
        for (e = t - 2; e >= 1; e -= 2) u += r[e] * n[e] + r[i = e - 1] * n[i];
        return 0 === e && (u += r[0] * n[0]), u
    }, numeric.dot = function(r, n) {
        var e = numeric.dim;
        switch (1e3 * e(r).length + e(n).length) {
            case 2002:
                return n.length < 10 ? numeric.dotMMsmall(r, n) : numeric.dotMMbig(r, n);
            case 2001:
                return numeric.dotMV(r, n);
            case 1002:
                return numeric.dotVM(r, n);
            case 1001:
                return numeric.dotVV(r, n);
            case 1e3:
                return numeric.mulVS(r, n);
            case 1:
                return numeric.mulSV(r, n);
            case 0:
                return r * n;
            default:
                throw new Error("numeric.dot only works on vectors and matrices")
        }
    }, numeric.diag = function(r) {
        var n, e, i, t, u = r.length,
            o = Array(u);
        for (n = u - 1; n >= 0; n--) {
            for (t = Array(u), e = n + 2, i = u - 1; i >= e; i -= 2) t[i] = 0, t[i - 1] = 0;
            for (i > n && (t[i] = 0), t[n] = r[n], i = n - 1; i >= 1; i -= 2) t[i] = 0, t[i - 1] = 0;
            0 === i && (t[0] = 0), o[n] = t
        }
        return o
    }, numeric.getDiag = function(r) {
        var n, e = Math.min(r.length, r[0].length),
            i = Array(e);
        for (n = e - 1; n >= 1; --n) i[n] = r[n][n], i[--n] = r[n][n];
        return 0 === n && (i[0] = r[0][0]), i
    }, numeric.identity = function(r) {
        return numeric.diag(numeric.rep([r], 1))
    }, numeric.pointwise = function(r, n, e) {
        void 0 === e && (e = "");
        var i, t, u = [],
            o = /\[i\]$/,
            c = "",
            m = !1;
        for (i = 0; i < r.length; i++) o.test(r[i]) ? c = t = r[i].substring(0, r[i].length - 3) : t = r[i], "ret" === t && (m = !0), u.push(t);
        return u[r.length] = "_s", u[r.length + 1] = "_k", u[r.length + 2] = 'if(typeof _s === "undefined") _s = numeric.dim(' + c + ');\nif(typeof _k === "undefined") _k = 0;\nvar _n = _s[_k];\nvar i' + (m ? "" : ", ret = Array(_n)") + ";\nif(_k < _s.length-1) {\n    for(i=_n-1;i>=0;i--) ret[i] = arguments.callee(" + r.join(",") + ",_s,_k+1);\n    return ret;\n}\n" + e + "\nfor(i=_n-1;i!==-1;--i) {\n    " + n + "\n}\nreturn ret;", Function.apply(null, u)
    }, numeric.pointwise2 = function(r, n, e) {
        void 0 === e && (e = "");
        var i, t, u = [],
            o = /\[i\]$/,
            c = "",
            m = !1;
        for (i = 0; i < r.length; i++) o.test(r[i]) ? c = t = r[i].substring(0, r[i].length - 3) : t = r[i], "ret" === t && (m = !0), u.push(t);
        return u[r.length] = "var _n = " + c + ".length;\nvar i" + (m ? "" : ", ret = Array(_n)") + ";\n" + e + "\nfor(i=_n-1;i!==-1;--i) {\n" + n + "\n}\nreturn ret;", Function.apply(null, u)
    }, numeric._biforeach = function r(n, e, i, t, u) {
        var o;
        if (t !== i.length - 1)
            for (o = i[t] - 1; o >= 0; o--) r("object" == typeof n ? n[o] : n, "object" == typeof e ? e[o] : e, i, t + 1, u);
        else u(n, e)
    }, numeric._biforeach2 = function r(n, e, i, t, u) {
        if (t === i.length - 1) return u(n, e);
        var o, c = i[t],
            m = Array(c);
        for (o = c - 1; o >= 0; --o) m[o] = r("object" == typeof n ? n[o] : n, "object" == typeof e ? e[o] : e, i, t + 1, u);
        return m
    }, numeric._foreach = function r(n, e, i, t) {
        var u;
        if (i !== e.length - 1)
            for (u = e[i] - 1; u >= 0; u--) r(n[u], e, i + 1, t);
        else t(n)
    }, numeric._foreach2 = function r(n, e, i, t) {
        if (i === e.length - 1) return t(n);
        var u, o = e[i],
            c = Array(o);
        for (u = o - 1; u >= 0; u--) c[u] = r(n[u], e, i + 1, t);
        return c
    }, numeric.ops2 = {
        add: "+",
        sub: "-",
        mul: "*",
        div: "/",
        mod: "%",
        and: "&&",
        or: "||",
        eq: "===",
        neq: "!==",
        lt: "<",
        gt: ">",
        leq: "<=",
        geq: ">=",
        band: "&",
        bor: "|",
        bxor: "^",
        lshift: "<<",
        rshift: ">>",
        rrshift: ">>>"
    }, numeric.opseq = {
        addeq: "+=",
        subeq: "-=",
        muleq: "*=",
        diveq: "/=",
        modeq: "%=",
        lshifteq: "<<=",
        rshifteq: ">>=",
        rrshifteq: ">>>=",
        bandeq: "&=",
        boreq: "|=",
        bxoreq: "^="
    }, numeric.mathfuns = ["abs", "acos", "asin", "atan", "ceil", "cos", "exp", "floor", "log", "round", "sin", "sqrt", "tan", "isNaN", "isFinite"], numeric.mathfuns2 = ["atan2", "pow", "max", "min"], numeric.ops1 = {
        neg: "-",
        not: "!",
        bnot: "~",
        clone: ""
    }, numeric.mapreducers = {
        any: ["if(xi) return true;", "var accum = false;"],
        all: ["if(!xi) return false;", "var accum = true;"],
        sum: ["accum += xi;", "var accum = 0;"],
        prod: ["accum *= xi;", "var accum = 1;"],
        norm2Squared: ["accum += xi*xi;", "var accum = 0;"],
        norminf: ["accum = max(accum,abs(xi));", "var accum = 0, max = Math.max, abs = Math.abs;"],
        norm1: ["accum += abs(xi)", "var accum = 0, abs = Math.abs;"],
        sup: ["accum = max(accum,xi);", "var accum = -Infinity, max = Math.max;"],
        inf: ["accum = min(accum,xi);", "var accum = Infinity, min = Math.min;"]
    },
    function() {
        var r, n;
        for (r = 0; r < numeric.mathfuns2.length; ++r) numeric.ops2[n = numeric.mathfuns2[r]] = n;
        for (r in numeric.ops2)
            if (numeric.ops2.hasOwnProperty(r)) {
                n = numeric.ops2[r];
                var e, i, t = ""; - 1 !== numeric.myIndexOf.call(numeric.mathfuns2, r) ? (t = "var " + n + " = Math." + n + ";\n", e = function(r, e, i) {
                    return r + " = " + n + "(" + e + "," + i + ")"
                }, i = function(r, e) {
                    return r + " = " + n + "(" + r + "," + e + ")"
                }) : (e = function(r, e, i) {
                    return r + " = " + e + " " + n + " " + i
                }, i = numeric.opseq.hasOwnProperty(r + "eq") ? function(r, e) {
                    return r + " " + n + "= " + e
                } : function(r, e) {
                    return r + " = " + r + " " + n + " " + e
                }), numeric[r + "VV"] = numeric.pointwise2(["x[i]", "y[i]"], e("ret[i]", "x[i]", "y[i]"), t), numeric[r + "SV"] = numeric.pointwise2(["x", "y[i]"], e("ret[i]", "x", "y[i]"), t), numeric[r + "VS"] = numeric.pointwise2(["x[i]", "y"], e("ret[i]", "x[i]", "y"), t), numeric[r] = Function("var n = arguments.length, i, x = arguments[0], y;\nvar VV = numeric." + r + "VV, VS = numeric." + r + "VS, SV = numeric." + r + 'SV;\nvar dim = numeric.dim;\nfor(i=1;i!==n;++i) { \n  y = arguments[i];\n  if(typeof x === "object") {\n      if(typeof y === "object") x = numeric._biforeach2(x,y,dim(x),0,VV);\n      else x = numeric._biforeach2(x,y,dim(x),0,VS);\n  } else if(typeof y === "object") x = numeric._biforeach2(x,y,dim(y),0,SV);\n  else ' + i("x", "y") + "\n}\nreturn x;\n"), numeric[n] = numeric[r], numeric[r + "eqV"] = numeric.pointwise2(["ret[i]", "x[i]"], i("ret[i]", "x[i]"), t), numeric[r + "eqS"] = numeric.pointwise2(["ret[i]", "x"], i("ret[i]", "x"), t), numeric[r + "eq"] = Function("var n = arguments.length, i, x = arguments[0], y;\nvar V = numeric." + r + "eqV, S = numeric." + r + 'eqS\nvar s = numeric.dim(x);\nfor(i=1;i!==n;++i) { \n  y = arguments[i];\n  if(typeof y === "object") numeric._biforeach(x,y,s,0,V);\n  else numeric._biforeach(x,y,s,0,S);\n}\nreturn x;\n')
            } for (r = 0; r < numeric.mathfuns2.length; ++r) delete numeric.ops2[n = numeric.mathfuns2[r]];
        for (r = 0; r < numeric.mathfuns.length; ++r) numeric.ops1[n = numeric.mathfuns[r]] = n;
        for (r in numeric.ops1) numeric.ops1.hasOwnProperty(r) && (t = "", n = numeric.ops1[r], -1 !== numeric.myIndexOf.call(numeric.mathfuns, r) && Math.hasOwnProperty(n) && (t = "var " + n + " = Math." + n + ";\n"), numeric[r + "eqV"] = numeric.pointwise2(["ret[i]"], "ret[i] = " + n + "(ret[i]);", t), numeric[r + "eq"] = Function("x", 'if(typeof x !== "object") return ' + n + "x\nvar i;\nvar V = numeric." + r + "eqV;\nvar s = numeric.dim(x);\nnumeric._foreach(x,s,0,V);\nreturn x;\n"), numeric[r + "V"] = numeric.pointwise2(["x[i]"], "ret[i] = " + n + "(x[i]);", t), numeric[r] = Function("x", 'if(typeof x !== "object") return ' + n + "(x)\nvar i;\nvar V = numeric." + r + "V;\nvar s = numeric.dim(x);\nreturn numeric._foreach2(x,s,0,V);\n"));
        for (r = 0; r < numeric.mathfuns.length; ++r) delete numeric.ops1[n = numeric.mathfuns[r]];
        for (r in numeric.mapreducers) numeric.mapreducers.hasOwnProperty(r) && (numeric[r + "V"] = numeric.mapreduce2((n = numeric.mapreducers[r])[0], n[1]), numeric[r] = Function("x", "s", "k", n[1] + 'if(typeof x !== "object") {    xi = x;\n' + n[0] + ';\n    return accum;\n}if(typeof s === "undefined") s = numeric.dim(x);\nif(typeof k === "undefined") k = 0;\nif(k === s.length-1) return numeric.' + r + "V(x);\nvar xi;\nvar n = x.length, i;\nfor(i=n-1;i!==-1;--i) {\n   xi = arguments.callee(x[i]);\n" + n[0] + ";\n}\nreturn accum;\n"))
    }(), numeric.truncVV = numeric.pointwise(["x[i]", "y[i]"], "ret[i] = round(x[i]/y[i])*y[i];", "var round = Math.round;"), numeric.truncVS = numeric.pointwise(["x[i]", "y"], "ret[i] = round(x[i]/y)*y;", "var round = Math.round;"), numeric.truncSV = numeric.pointwise(["x", "y[i]"], "ret[i] = round(x/y[i])*y[i];", "var round = Math.round;"), numeric.trunc = function(r, n) {
        return "object" == typeof r ? "object" == typeof n ? numeric.truncVV(r, n) : numeric.truncVS(r, n) : "object" == typeof n ? numeric.truncSV(r, n) : Math.round(r / n) * n
    }, numeric.inv = function(r) {
        var n, e, i, t, u, o, c, m = numeric.dim(r),
            a = Math.abs,
            f = m[0],
            s = m[1],
            h = numeric.clone(r),
            p = numeric.identity(f);
        for (o = 0; o < s; ++o) {
            var l = -1,
                y = -1;
            for (u = o; u !== f; ++u)(c = a(h[u][o])) > y && (l = u, y = c);
            for (e = h[l], h[l] = h[o], h[o] = e, t = p[l], p[l] = p[o], p[o] = t, r = e[o], c = o; c !== s; ++c) e[c] /= r;
            for (c = s - 1; - 1 !== c; --c) t[c] /= r;
            for (u = f - 1; - 1 !== u; --u)
                if (u !== o) {
                    for (i = p[u], r = (n = h[u])[o], c = o + 1; c !== s; ++c) n[c] -= e[c] * r;
                    for (c = s - 1; c > 0; --c) i[c] -= t[c] * r, i[--c] -= t[c] * r;
                    0 === c && (i[0] -= t[0] * r)
                }
        }
        return p
    }, numeric.det = function(r) {
        var n = numeric.dim(r);
        if (2 !== n.length || n[0] !== n[1]) throw new Error("numeric: det() only works on square matrices");
        var e, i, t, u, o, c, m, a, f = n[0],
            s = 1,
            h = numeric.clone(r);
        for (i = 0; i < f - 1; i++) {
            for (t = i, e = i + 1; e < f; e++) Math.abs(h[e][i]) > Math.abs(h[t][i]) && (t = e);
            for (t !== i && (m = h[t], h[t] = h[i], h[i] = m, s *= -1), u = h[i], e = i + 1; e < f; e++) {
                for (c = (o = h[e])[i] / u[i], t = i + 1; t < f - 1; t += 2) a = t + 1, o[t] -= u[t] * c, o[a] -= u[a] * c;
                t !== f && (o[t] -= u[t] * c)
            }
            if (0 === u[i]) return 0;
            s *= u[i]
        }
        return s * h[i][i]
    }, numeric.transpose = function(r) {
        var n, e, i, t, u, o = r.length,
            c = r[0].length,
            m = Array(c);
        for (e = 0; e < c; e++) m[e] = Array(o);
        for (n = o - 1; n >= 1; n -= 2) {
            for (t = r[n], i = r[n - 1], e = c - 1; e >= 1; --e)(u = m[e])[n] = t[e], u[n - 1] = i[e], (u = m[--e])[n] = t[e], u[n - 1] = i[e];
            0 === e && ((u = m[0])[n] = t[0], u[n - 1] = i[0])
        }
        if (0 === n) {
            for (i = r[0], e = c - 1; e >= 1; --e) m[e][0] = i[e], m[--e][0] = i[e];
            0 === e && (m[0][0] = i[0])
        }
        return m
    }, numeric.negtranspose = function(r) {
        var n, e, i, t, u, o = r.length,
            c = r[0].length,
            m = Array(c);
        for (e = 0; e < c; e++) m[e] = Array(o);
        for (n = o - 1; n >= 1; n -= 2) {
            for (t = r[n], i = r[n - 1], e = c - 1; e >= 1; --e)(u = m[e])[n] = -t[e], u[n - 1] = -i[e], (u = m[--e])[n] = -t[e], u[n - 1] = -i[e];
            0 === e && ((u = m[0])[n] = -t[0], u[n - 1] = -i[0])
        }
        if (0 === n) {
            for (i = r[0], e = c - 1; e >= 1; --e) m[e][0] = -i[e], m[--e][0] = -i[e];
            0 === e && (m[0][0] = -i[0])
        }
        return m
    }, numeric._random = function r(n, e) {
        var i, t, u = n[e],
            o = Array(u);
        if (e === n.length - 1) {
            for (t = Math.random, i = u - 1; i >= 1; i -= 2) o[i] = t(), o[i - 1] = t();
            return 0 === i && (o[0] = t()), o
        }
        for (i = u - 1; i >= 0; i--) o[i] = r(n, e + 1);
        return o
    }, numeric.random = function(r) {
        return numeric._random(r, 0)
    }, numeric.norm2 = function(r) {
        return Math.sqrt(numeric.norm2Squared(r))
    }, numeric.linspace = function(r, n, e) {
        if (void 0 === e && (e = Math.max(Math.round(n - r) + 1, 1)), e < 2) return 1 === e ? [r] : [];
        var i, t = Array(e);
        for (i = --e; i >= 0; i--) t[i] = (i * n + (e - i) * r) / e;
        return t
    }, numeric.getBlock = function(r, n, e) {
        var i = numeric.dim(r);
        return function r(t, u) {
            var o, c = n[u],
                m = e[u] - c,
                a = Array(m);
            if (u === i.length - 1) {
                for (o = m; o >= 0; o--) a[o] = t[o + c];
                return a
            }
            for (o = m; o >= 0; o--) a[o] = r(t[o + c], u + 1);
            return a
        }(r, 0)
    }, numeric.setBlock = function(r, n, e, i) {
        var t = numeric.dim(r);
        return function r(i, u, o) {
            var c, m = n[o],
                a = e[o] - m;
            if (o === t.length - 1)
                for (c = a; c >= 0; c--) i[c + m] = u[c];
            for (c = a; c >= 0; c--) r(i[c + m], u[c], o + 1)
        }(r, i, 0), r
    }, numeric.getRange = function(r, n, e) {
        var i, t, u, o, c = n.length,
            m = e.length,
            a = Array(c);
        for (i = c - 1; - 1 !== i; --i)
            for (a[i] = Array(m), u = a[i], o = r[n[i]], t = m - 1; - 1 !== t; --t) u[t] = o[e[t]];
        return a
    }, numeric.blockMatrix = function(r) {
        var n = numeric.dim(r);
        if (n.length < 4) return numeric.blockMatrix([r]);
        var e, i, t, u, o, c = n[0],
            m = n[1];
        for (e = 0, i = 0, t = 0; t < c; ++t) e += r[t][0].length;
        for (u = 0; u < m; ++u) i += r[0][u][0].length;
        var a = Array(e);
        for (t = 0; t < e; ++t) a[t] = Array(i);
        var f, s, h, p, l, y = 0;
        for (t = 0; t < c; ++t) {
            for (f = i, u = m - 1; - 1 !== u; --u)
                for (f -= (o = r[t][u])[0].length, h = o.length - 1; - 1 !== h; --h)
                    for (s = a[y + h], p = (l = o[h]).length - 1; - 1 !== p; --p) s[f + p] = l[p];
            y += r[t][0].length
        }
        return a
    }, numeric.tensor = function(r, n) {
        if ("number" == typeof r || "number" == typeof n) return numeric.mul(r, n);
        var e = numeric.dim(r),
            i = numeric.dim(n);
        if (1 !== e.length || 1 !== i.length) throw new Error("numeric: tensor product is only defined for vectors");
        var t, u, o, c, m = e[0],
            a = i[0],
            f = Array(m);
        for (u = m - 1; u >= 0; u--) {
            for (t = Array(a), c = r[u], o = a - 1; o >= 3; --o) t[o] = c * n[o], t[--o] = c * n[o], t[--o] = c * n[o], t[--o] = c * n[o];
            for (; o >= 0;) t[o] = c * n[o], --o;
            f[u] = t
        }
        return f
    }, numeric.T = function(r, n) {
        this.x = r, this.y = n
    }, numeric.t = function(r, n) {
        return new numeric.T(r, n)
    }, numeric.Tbinop = function(r, n, e, i, t) {
        var u;
        if ("string" != typeof t)
            for (u in t = "", numeric) numeric.hasOwnProperty(u) && (r.indexOf(u) >= 0 || n.indexOf(u) >= 0 || e.indexOf(u) >= 0 || i.indexOf(u) >= 0) && u.length > 1 && (t += "var " + u + " = numeric." + u + ";\n");
        return Function(["y"], "var x = this;\nif(!(y instanceof numeric.T)) { y = new numeric.T(y); }\n" + t + "\nif(x.y) {  if(y.y) {    return new numeric.T(" + i + ");\n  }\n  return new numeric.T(" + e + ");\n}\nif(y.y) {\n  return new numeric.T(" + n + ");\n}\nreturn new numeric.T(" + r + ");\n")
    }, numeric.T.prototype.add = numeric.Tbinop("add(x.x,y.x)", "add(x.x,y.x),y.y", "add(x.x,y.x),x.y", "add(x.x,y.x),add(x.y,y.y)"), numeric.T.prototype.sub = numeric.Tbinop("sub(x.x,y.x)", "sub(x.x,y.x),neg(y.y)", "sub(x.x,y.x),x.y", "sub(x.x,y.x),sub(x.y,y.y)"), numeric.T.prototype.mul = numeric.Tbinop("mul(x.x,y.x)", "mul(x.x,y.x),mul(x.x,y.y)", "mul(x.x,y.x),mul(x.y,y.x)", "sub(mul(x.x,y.x),mul(x.y,y.y)),add(mul(x.x,y.y),mul(x.y,y.x))"), numeric.T.prototype.reciprocal = function() {
        var r = numeric.mul,
            n = numeric.div;
        if (this.y) {
            var e = numeric.add(r(this.x, this.x), r(this.y, this.y));
            return new numeric.T(n(this.x, e), n(numeric.neg(this.y), e))
        }
        return new T(n(1, this.x))
    }, numeric.T.prototype.div = function(r) {
        if (r instanceof numeric.T || (r = new numeric.T(r)), r.y) return this.mul(r.reciprocal());
        var n = numeric.div;
        return this.y ? new numeric.T(n(this.x, r.x), n(this.y, r.x)) : new numeric.T(n(this.x, r.x))
    }, numeric.T.prototype.dot = numeric.Tbinop("dot(x.x,y.x)", "dot(x.x,y.x),dot(x.x,y.y)", "dot(x.x,y.x),dot(x.y,y.x)", "sub(dot(x.x,y.x),dot(x.y,y.y)),add(dot(x.x,y.y),dot(x.y,y.x))"), numeric.T.prototype.transpose = function() {
        var r = numeric.transpose,
            n = this.x,
            e = this.y;
        return e ? new numeric.T(r(n), r(e)) : new numeric.T(r(n))
    }, numeric.T.prototype.transjugate = function() {
        var r = numeric.transpose,
            n = this.x,
            e = this.y;
        return e ? new numeric.T(r(n), numeric.negtranspose(e)) : new numeric.T(r(n))
    }, numeric.Tunop = function(r, n, e) {
        return "string" != typeof e && (e = ""), Function("var x = this;\n" + e + "\nif(x.y) {  " + n + ";\n}\n" + r + ";\n")
    }, numeric.T.prototype.exp = numeric.Tunop("return new numeric.T(ex)", "return new numeric.T(mul(cos(x.y),ex),mul(sin(x.y),ex))", "var ex = numeric.exp(x.x), cos = numeric.cos, sin = numeric.sin, mul = numeric.mul;"), numeric.T.prototype.conj = numeric.Tunop("return new numeric.T(x.x);", "return new numeric.T(x.x,numeric.neg(x.y));"), numeric.T.prototype.neg = numeric.Tunop("return new numeric.T(neg(x.x));", "return new numeric.T(neg(x.x),neg(x.y));", "var neg = numeric.neg;"), numeric.T.prototype.sin = numeric.Tunop("return new numeric.T(numeric.sin(x.x))", "return x.exp().sub(x.neg().exp()).div(new numeric.T(0,2));"), numeric.T.prototype.cos = numeric.Tunop("return new numeric.T(numeric.cos(x.x))", "return x.exp().add(x.neg().exp()).div(2);"), numeric.T.prototype.abs = numeric.Tunop("return new numeric.T(numeric.abs(x.x));", "return new numeric.T(numeric.sqrt(numeric.add(mul(x.x,x.x),mul(x.y,x.y))));", "var mul = numeric.mul;"), numeric.T.prototype.log = numeric.Tunop("return new numeric.T(numeric.log(x.x));", "var theta = new numeric.T(numeric.atan2(x.y,x.x)), r = x.abs();\nreturn new numeric.T(numeric.log(r.x),theta.x);"), numeric.T.prototype.norm2 = numeric.Tunop("return numeric.norm2(x.x);", "var f = numeric.norm2Squared;\nreturn Math.sqrt(f(x.x)+f(x.y));"), numeric.T.prototype.inv = function() {
        var r = this;
        if (void 0 === r.y) return new numeric.T(numeric.inv(r.x));
        var n, e, i, t, u, o, c, m, a, f, s, h, p, l, y, g, v, d, x = r.x.length,
            w = numeric.identity(x),
            b = numeric.rep([x, x], 0),
            k = numeric.clone(r.x),
            T = numeric.clone(r.y);
        for (a = 0; a < x; a++) {
            for (h = (l = k[a][a]) * l + (y = T[a][a]) * y, s = a, f = a + 1; f < x; f++)(p = (l = k[f][a]) * l + (y = T[f][a]) * y) > h && (s = f, h = p);
            for (s !== a && (d = k[a], k[a] = k[s], k[s] = d, d = T[a], T[a] = T[s], T[s] = d, d = w[a], w[a] = w[s], w[s] = d, d = b[a], b[a] = b[s], b[s] = d), u = w[a], o = b[a], l = (n = k[a])[a], y = (e = T[a])[a], f = a + 1; f < x; f++) n[f] = ((g = n[f]) * l + (v = e[f]) * y) / h, e[f] = (v * l - g * y) / h;
            for (f = 0; f < x; f++) u[f] = ((g = u[f]) * l + (v = o[f]) * y) / h, o[f] = (v * l - g * y) / h;
            for (f = a + 1; f < x; f++) {
                for (c = w[f], m = b[f], l = (i = k[f])[a], y = (t = T[f])[a], s = a + 1; s < x; s++) i[s] -= (g = n[s]) * l - (v = e[s]) * y, t[s] -= v * l + g * y;
                for (s = 0; s < x; s++) c[s] -= (g = u[s]) * l - (v = o[s]) * y, m[s] -= v * l + g * y
            }
        }
        for (a = x - 1; a > 0; a--)
            for (u = w[a], o = b[a], f = a - 1; f >= 0; f--)
                for (c = w[f], m = b[f], l = k[f][a], y = T[f][a], s = x - 1; s >= 0; s--) c[s] -= l * (g = u[s]) - y * (v = o[s]), m[s] -= l * v + y * g;
        return new numeric.T(w, b)
    }, numeric.T.prototype.get = function(r) {
        var n, e = this.x,
            i = this.y,
            t = 0,
            u = r.length;
        if (i) {
            for (; t < u;) e = e[n = r[t]], i = i[n], t++;
            return new numeric.T(e, i)
        }
        for (; t < u;) e = e[n = r[t]], t++;
        return new numeric.T(e)
    }, numeric.T.prototype.set = function(r, n) {
        var e, i = this.x,
            t = this.y,
            u = 0,
            o = r.length,
            c = n.x,
            m = n.y;
        if (0 === o) return m ? this.y = m : t && (this.y = void 0), this.x = i, this;
        if (m) {
            for (t || (t = numeric.rep(numeric.dim(i), 0), this.y = t); u < o - 1;) i = i[e = r[u]], t = t[e], u++;
            return i[e = r[u]] = c, t[e] = m, this
        }
        if (t) {
            for (; u < o - 1;) i = i[e = r[u]], t = t[e], u++;
            return i[e = r[u]] = c, t[e] = c instanceof Array ? numeric.rep(numeric.dim(c), 0) : 0, this
        }
        for (; u < o - 1;) i = i[e = r[u]], u++;
        return i[e = r[u]] = c, this
    }, numeric.T.prototype.getRows = function(r, n) {
        var e, i, t = n - r + 1,
            u = Array(t),
            o = this.x,
            c = this.y;
        for (e = r; e <= n; e++) u[e - r] = o[e];
        if (c) {
            for (i = Array(t), e = r; e <= n; e++) i[e - r] = c[e];
            return new numeric.T(u, i)
        }
        return new numeric.T(u)
    }, numeric.T.prototype.setRows = function(r, n, e) {
        var i, t = this.x,
            u = this.y,
            o = e.x,
            c = e.y;
        for (i = r; i <= n; i++) t[i] = o[i - r];
        if (c)
            for (u || (u = numeric.rep(numeric.dim(t), 0), this.y = u), i = r; i <= n; i++) u[i] = c[i - r];
        else if (u)
            for (i = r; i <= n; i++) u[i] = numeric.rep([o[i - r].length], 0);
        return this
    }, numeric.T.prototype.getRow = function(r) {
        var n = this.x,
            e = this.y;
        return e ? new numeric.T(n[r], e[r]) : new numeric.T(n[r])
    }, numeric.T.prototype.setRow = function(r, n) {
        var e = this.x,
            i = this.y,
            t = n.x,
            u = n.y;
        return e[r] = t, u ? (i || (i = numeric.rep(numeric.dim(e), 0), this.y = i), i[r] = u) : i && (i = numeric.rep([t.length], 0)), this
    }, numeric.T.prototype.getBlock = function(r, n) {
        var e = this.x,
            i = this.y,
            t = numeric.getBlock;
        return i ? new numeric.T(t(e, r, n), t(i, r, n)) : new numeric.T(t(e, r, n))
    }, numeric.T.prototype.setBlock = function(r, n, e) {
        e instanceof numeric.T || (e = new numeric.T(e));
        var i = this.x,
            t = this.y,
            u = numeric.setBlock,
            o = e.x,
            c = e.y;
        if (c) return t || (this.y = numeric.rep(numeric.dim(this), 0), t = this.y), u(i, r, n, o), u(t, r, n, c), this;
        u(i, r, n, o), t && u(t, r, n, numeric.rep(numeric.dim(o), 0))
    }, numeric.T.rep = function(r, n) {
        var e = numeric.T;
        n instanceof e || (n = new e(n));
        var i = n.x,
            t = n.y,
            u = numeric.rep;
        return t ? new e(u(r, i), u(r, t)) : new e(u(r, i))
    }, numeric.T.diag = function(r) {
        r instanceof numeric.T || (r = new numeric.T(r));
        var n = r.x,
            e = r.y,
            i = numeric.diag;
        return e ? new numeric.T(i(n), i(e)) : new numeric.T(i(n))
    }, numeric.T.eig = function() {
        if (this.y) throw new Error("eig: not implemented for complex matrices.");
        return numeric.eig(this.x)
    }, numeric.T.identity = function(r) {
        return new numeric.T(numeric.identity(r))
    }, numeric.T.prototype.getDiag = function() {
        var r = numeric,
            n = this.x,
            e = this.y;
        return e ? new r.T(r.getDiag(n), r.getDiag(e)) : new r.T(r.getDiag(n))
    }, numeric.house = function(r) {
        var n = numeric.clone(r),
            e = (r[0] >= 0 ? 1 : -1) * numeric.norm2(r);
        n[0] += e;
        var i = numeric.norm2(n);
        if (0 === i) throw new Error("eig: internal error");
        return numeric.div(n, i)
    }, numeric.toUpperHessenberg = function(r) {
        var n = numeric.dim(r);
        if (2 !== n.length || n[0] !== n[1]) throw new Error("numeric: toUpperHessenberg() only works on square matrices");
        var e, i, t, u, o, c, m, a, f, s, h = n[0],
            p = numeric.clone(r),
            l = numeric.identity(h);
        for (i = 0; i < h - 2; i++) {
            for (u = Array(h - i - 1), e = i + 1; e < h; e++) u[e - i - 1] = p[e][i];
            if (numeric.norm2(u) > 0) {
                for (o = numeric.house(u), c = numeric.getBlock(p, [i + 1, i], [h - 1, h - 1]), m = numeric.tensor(o, numeric.dot(o, c)), e = i + 1; e < h; e++)
                    for (a = p[e], f = m[e - i - 1], t = i; t < h; t++) a[t] -= 2 * f[t - i];
                for (c = numeric.getBlock(p, [0, i + 1], [h - 1, h - 1]), m = numeric.tensor(numeric.dot(c, o), o), e = 0; e < h; e++)
                    for (a = p[e], f = m[e], t = i + 1; t < h; t++) a[t] -= 2 * f[t - i - 1];
                for (c = Array(h - i - 1), e = i + 1; e < h; e++) c[e - i - 1] = l[e];
                for (m = numeric.tensor(o, numeric.dot(o, c)), e = i + 1; e < h; e++)
                    for (s = l[e], f = m[e - i - 1], t = 0; t < h; t++) s[t] -= 2 * f[t]
            }
        }
        return {
            H: p,
            Q: l
        }
    }, numeric.epsilon = 2.220446049250313e-16, numeric.QRFrancis = function(r, n) {
        void 0 === n && (n = 1e4), r = numeric.clone(r), numeric.clone(r);
        var e, i, t, u, o, c, m, a, f, s, h, p, l, y, g, v, d = numeric.dim(r)[0],
            x = numeric.identity(d);
        if (d < 3) return {
            Q: x,
            B: [
                [0, d - 1]
            ]
        };
        var w = numeric.epsilon;
        for (v = 0; v < n; v++) {
            for (y = 0; y < d - 1; y++)
                if (Math.abs(r[y + 1][y]) < w * (Math.abs(r[y][y]) + Math.abs(r[y + 1][y + 1]))) {
                    var b = numeric.QRFrancis(numeric.getBlock(r, [0, 0], [y, y]), n),
                        k = numeric.QRFrancis(numeric.getBlock(r, [y + 1, y + 1], [d - 1, d - 1]), n);
                    for (s = Array(y + 1), l = 0; l <= y; l++) s[l] = x[l];
                    for (h = numeric.dot(b.Q, s), l = 0; l <= y; l++) x[l] = h[l];
                    for (s = Array(d - y - 1), l = y + 1; l < d; l++) s[l - y - 1] = x[l];
                    for (h = numeric.dot(k.Q, s), l = y + 1; l < d; l++) x[l] = h[l - y - 1];
                    return {
                        Q: x,
                        B: b.B.concat(numeric.add(k.B, y + 1))
                    }
                } var T, A, j;
            for (c = (t = r[d - 2][d - 2]) + (u = r[d - 1][d - 1]), o = t * u - r[d - 2][d - 1] * r[d - 1][d - 2], m = numeric.getBlock(r, [0, 0], [2, 2]), c * c >= 4 * o ? (T = .5 * (c + Math.sqrt(c * c - 4 * o)), A = .5 * (c - Math.sqrt(c * c - 4 * o)), m = numeric.add(numeric.sub(numeric.dot(m, m), numeric.mul(m, T + A)), numeric.diag(numeric.rep([3], T * A)))) : m = numeric.add(numeric.sub(numeric.dot(m, m), numeric.mul(m, c)), numeric.diag(numeric.rep([3], o))), i = numeric.house(e = [m[0][0], m[1][0], m[2][0]]), h = numeric.tensor(i, numeric.dot(i, s = [r[0], r[1], r[2]])), l = 0; l < 3; l++)
                for (f = r[l], p = h[l], g = 0; g < d; g++) f[g] -= 2 * p[g];
            for (s = numeric.getBlock(r, [0, 0], [d - 1, 2]), h = numeric.tensor(numeric.dot(s, i), i), l = 0; l < d; l++)
                for (f = r[l], p = h[l], g = 0; g < 3; g++) f[g] -= 2 * p[g];
            for (h = numeric.tensor(i, numeric.dot(i, s = [x[0], x[1], x[2]])), l = 0; l < 3; l++)
                for (a = x[l], p = h[l], g = 0; g < d; g++) a[g] -= 2 * p[g];
            for (y = 0; y < d - 2; y++) {
                for (g = y; g <= y + 1; g++)
                    if (Math.abs(r[g + 1][g]) < w * (Math.abs(r[g][g]) + Math.abs(r[g + 1][g + 1]))) {
                        for (b = numeric.QRFrancis(numeric.getBlock(r, [0, 0], [g, g]), n), k = numeric.QRFrancis(numeric.getBlock(r, [g + 1, g + 1], [d - 1, d - 1]), n), s = Array(g + 1), l = 0; l <= g; l++) s[l] = x[l];
                        for (h = numeric.dot(b.Q, s), l = 0; l <= g; l++) x[l] = h[l];
                        for (s = Array(d - g - 1), l = g + 1; l < d; l++) s[l - g - 1] = x[l];
                        for (h = numeric.dot(k.Q, s), l = g + 1; l < d; l++) x[l] = h[l - g - 1];
                        return {
                            Q: x,
                            B: b.B.concat(numeric.add(k.B, g + 1))
                        }
                    } for (j = Math.min(d - 1, y + 3), e = Array(j - y), l = y + 1; l <= j; l++) e[l - y - 1] = r[l][y];
                for (i = numeric.house(e), s = numeric.getBlock(r, [y + 1, y], [j, d - 1]), h = numeric.tensor(i, numeric.dot(i, s)), l = y + 1; l <= j; l++)
                    for (f = r[l], p = h[l - y - 1], g = y; g < d; g++) f[g] -= 2 * p[g - y];
                for (s = numeric.getBlock(r, [0, y + 1], [d - 1, j]), h = numeric.tensor(numeric.dot(s, i), i), l = 0; l < d; l++)
                    for (f = r[l], p = h[l], g = y + 1; g <= j; g++) f[g] -= 2 * p[g - y - 1];
                for (s = Array(j - y), l = y + 1; l <= j; l++) s[l - y - 1] = x[l];
                for (h = numeric.tensor(i, numeric.dot(i, s)), l = y + 1; l <= j; l++)
                    for (a = x[l], p = h[l - y - 1], g = 0; g < d; g++) a[g] -= 2 * p[g]
            }
        }
        throw new Error("numeric: eigenvalue iteration does not converge -- increase maxiter?")
    }, numeric.eig = function(r, n) {
        var e, i, t, u, o, c, m, a, f, s, h, p, l, y, g, v, d, x = numeric.toUpperHessenberg(r),
            w = numeric.QRFrancis(x.H, n),
            b = numeric.T,
            k = w.B,
            T = numeric.dot(w.Q, numeric.dot(x.H, numeric.transpose(w.Q))),
            A = new b(numeric.dot(w.Q, x.Q)),
            j = k.length,
            M = Math.sqrt;
        for (i = 0; i < j; i++)
            if ((e = k[i][0]) === k[i][1]);
            else {
                if (o = T[e][e], m = T[u = e + 1][e], a = T[u][u], 0 === (c = T[e][u]) && 0 === m) continue;
                (s = (f = -o - a) * f - 4 * (o * a - c * m)) >= 0 ? ((g = (o - (h = f < 0 ? -.5 * (f - M(s)) : -.5 * (f + M(s)))) * (o - h) + c * c) > (v = m * m + (a - h) * (a - h)) ? (l = (o - h) / (g = M(g)), y = c / g) : (l = m / (v = M(v)), y = (a - h) / v), t = new b([
                    [y, -l],
                    [l, y]
                ]), A.setRows(e, u, t.dot(A.getRows(e, u)))) : (h = -.5 * f, p = .5 * M(-s), (g = (o - h) * (o - h) + c * c) > (v = m * m + (a - h) * (a - h)) ? (l = (o - h) / (g = M(g + p * p)), y = c / g, h = 0, p /= g) : (l = m / (v = M(v + p * p)), y = (a - h) / v, h = p / v, p = 0), t = new b([
                    [y, -l],
                    [l, y]
                ], [
                    [h, p],
                    [p, -h]
                ]), A.setRows(e, u, t.dot(A.getRows(e, u))))
            } var _ = A.dot(r).dot(A.transjugate()),
            S = numeric.T.identity(d = r.length);
        for (u = 0; u < d; u++)
            if (u > 0)
                for (i = u - 1; i >= 0; i--) {
                    var V = _.get([i, i]),
                        P = _.get([u, u]);
                    numeric.neq(V.x, P.x) || numeric.neq(V.y, P.y) ? (h = _.getRow(i).getBlock([i], [u - 1]), p = S.getRow(u).getBlock([i], [u - 1]), S.set([u, i], _.get([i, u]).neg().sub(h.dot(p)).div(V.sub(P)))) : S.setRow(u, S.getRow(i))
                }
        for (u = 0; u < d; u++) h = S.getRow(u), S.setRow(u, h.div(h.norm2()));
        return S = S.transpose(), S = A.transjugate().dot(S), {
            lambda: _.getDiag(),
            E: S
        }
    }, numeric.ccsSparse = function(r) {
        var n, e, i, t = r.length,
            u = [];
        for (e = t - 1; - 1 !== e; --e)
            for (i in n = r[e]) {
                for (i = parseInt(i); i >= u.length;) u[u.length] = 0;
                0 !== n[i] && u[i]++
            }
        var o = u.length,
            c = Array(o + 1);
        for (c[0] = 0, e = 0; e < o; ++e) c[e + 1] = c[e] + u[e];
        var m = Array(c[o]),
            a = Array(c[o]);
        for (e = t - 1; - 1 !== e; --e)
            for (i in n = r[e]) 0 !== n[i] && (u[i]--, m[c[i] + u[i]] = e, a[c[i] + u[i]] = n[i]);
        return [c, m, a]
    }, numeric.ccsFull = function(r) {
        var n, e, i, t = r[0],
            u = r[1],
            o = r[2],
            c = numeric.ccsDim(r),
            m = c[1],
            a = numeric.rep([c[0], m], 0);
        for (n = 0; n < m; n++)
            for (i = t[n + 1], e = t[n]; e < i; ++e) a[u[e]][n] = o[e];
        return a
    }, numeric.ccsTSolve = function(r, n, e, i, t) {
        var u, o, c, m, a, f, s, h = r[0],
            p = r[1],
            l = r[2],
            y = Math.max,
            g = 0;

        function v(r) {
            var n;
            if (0 === e[r]) {
                for (e[r] = 1, n = h[r]; n < h[r + 1]; ++n) v(p[n]);
                t[g] = r, ++g
            }
        }
        for (void 0 === i && (e = numeric.rep([h.length - 1], 0)), void 0 === i && (i = numeric.linspace(0, e.length - 1)), void 0 === t && (t = []), u = i.length - 1; - 1 !== u; --u) v(i[u]);
        for (t.length = g, u = t.length - 1; - 1 !== u; --u) e[t[u]] = 0;
        for (u = i.length - 1; - 1 !== u; --u) e[o = i[u]] = n[o];
        for (u = t.length - 1; - 1 !== u; --u) {
            for (m = y(h[(o = t[u]) + 1], c = h[o]), a = c; a !== m; ++a)
                if (p[a] === o) {
                    e[o] /= l[a];
                    break
                } for (s = e[o], a = c; a !== m; ++a)(f = p[a]) !== o && (e[f] -= s * l[a])
        }
        return e
    }, numeric.ccsDFS = function(r) {
        this.k = Array(r), this.k1 = Array(r), this.j = Array(r)
    }, numeric.ccsDFS.prototype.dfs = function(r, n, e, i, t, u) {
        var o, c, m, a = 0,
            f = t.length,
            s = this.k,
            h = this.k1,
            p = this.j;
        if (0 === i[r])
            for (i[r] = 1, p[0] = r, s[0] = c = n[r], h[0] = m = n[r + 1];;)
                if (c >= m) {
                    if (t[f] = p[a], 0 === a) return;
                    ++f, c = s[--a], m = h[a]
                } else 0 === i[o = u[e[c]]] ? (i[o] = 1, s[a] = c, p[++a] = o, c = n[o], h[a] = m = n[o + 1]) : ++c
    }, numeric.ccsLPSolve = function(r, n, e, i, t, u, o) {
        var c, m, a, f, s, h, p, l, y, g = r[0],
            v = r[1],
            d = r[2],
            x = n[0],
            w = n[1],
            b = n[2];
        for (m = x[t], a = x[t + 1], i.length = 0, c = m; c < a; ++c) o.dfs(u[w[c]], g, v, e, i, u);
        for (c = i.length - 1; - 1 !== c; --c) e[i[c]] = 0;
        for (c = m; c !== a; ++c) e[f = u[w[c]]] = b[c];
        for (c = i.length - 1; - 1 !== c; --c) {
            for (h = g[(f = i[c]) + 1], p = s = g[f]; p < h; ++p)
                if (u[v[p]] === f) {
                    e[f] /= d[p];
                    break
                } for (y = e[f], p = s; p < h; ++p)(l = u[v[p]]) !== f && (e[l] -= y * d[p])
        }
        return e
    }, numeric.ccsLUP1 = function(r, n) {
        var e, i, t, u, o, c, m, a = r[0].length - 1,
            f = [numeric.rep([a + 1], 0), [],
                []
            ],
            s = [numeric.rep([a + 1], 0), [],
                []
            ],
            h = f[0],
            p = f[1],
            l = f[2],
            y = s[0],
            g = s[1],
            v = s[2],
            d = numeric.rep([a], 0),
            x = numeric.rep([a], 0),
            w = numeric.ccsLPSolve,
            b = Math.abs,
            k = numeric.linspace(0, a - 1),
            T = numeric.linspace(0, a - 1),
            A = new numeric.ccsDFS(a);
        for (void 0 === n && (n = 1), e = 0; e < a; ++e) {
            for (w(f, r, d, x, e, T, A), u = -1, o = -1, i = x.length - 1; - 1 !== i; --i)(t = x[i]) <= e || (c = b(d[t])) > u && (o = t, u = c);
            for (b(d[e]) < n * u && (i = k[e], k[e] = u = k[o], T[u] = e, k[o] = i, T[i] = o, u = d[e], d[e] = d[o], d[o] = u), o = y[e], m = d[e], p[u = h[e]] = k[e], l[u] = 1, ++u, i = x.length - 1; - 1 !== i; --i) c = d[t = x[i]], x[i] = 0, d[t] = 0, t <= e ? (g[o] = t, v[o] = c, ++o) : (p[u] = k[t], l[u] = c / m, ++u);
            h[e + 1] = u, y[e + 1] = o
        }
        for (i = p.length - 1; - 1 !== i; --i) p[i] = T[p[i]];
        return {
            L: f,
            U: s,
            P: k,
            Pinv: T
        }
    }, numeric.ccsDFS0 = function(r) {
        this.k = Array(r), this.k1 = Array(r), this.j = Array(r)
    }, numeric.ccsDFS0.prototype.dfs = function(r, n, e, i, t, u, o) {
        var c, m, a, f = 0,
            s = t.length,
            h = this.k,
            p = this.k1,
            l = this.j;
        if (0 === i[r])
            for (i[r] = 1, l[0] = r, h[0] = m = n[u[r]], p[0] = a = n[u[r] + 1];;) {
                if (isNaN(m)) throw new Error("Ow!");
                if (m >= a) {
                    if (t[s] = u[l[f]], 0 === f) return;
                    ++s, m = h[--f], a = p[f]
                } else 0 === i[c = e[m]] ? (i[c] = 1, h[f] = m, l[++f] = c, m = n[c = u[c]], p[f] = a = n[c + 1]) : ++m
            }
    }, numeric.ccsLPSolve0 = function(r, n, e, i, t, u, o, c) {
        var m, a, f, s, h, p, l, y, g, v = r[0],
            d = r[1],
            x = r[2],
            w = n[0],
            b = n[1],
            k = n[2];
        for (a = w[t], f = w[t + 1], i.length = 0, m = a; m < f; ++m) c.dfs(b[m], v, d, e, i, u, o);
        for (m = i.length - 1; - 1 !== m; --m) e[o[s = i[m]]] = 0;
        for (m = a; m !== f; ++m) e[s = b[m]] = k[m];
        for (m = i.length - 1; - 1 !== m; --m) {
            for (y = o[s = i[m]], p = v[s + 1], l = h = v[s]; l < p; ++l)
                if (d[l] === y) {
                    e[y] /= x[l];
                    break
                } for (g = e[y], l = h; l < p; ++l) e[d[l]] -= g * x[l];
            e[y] = g
        }
    }, numeric.ccsLUP0 = function(r, n) {
        var e, i, t, u, o, c, m, a = r[0].length - 1,
            f = [numeric.rep([a + 1], 0), [],
                []
            ],
            s = [numeric.rep([a + 1], 0), [],
                []
            ],
            h = f[0],
            p = f[1],
            l = f[2],
            y = s[0],
            g = s[1],
            v = s[2],
            d = numeric.rep([a], 0),
            x = numeric.rep([a], 0),
            w = numeric.ccsLPSolve0,
            b = Math.abs,
            k = numeric.linspace(0, a - 1),
            T = numeric.linspace(0, a - 1),
            A = new numeric.ccsDFS0(a);
        for (void 0 === n && (n = 1), e = 0; e < a; ++e) {
            for (w(f, r, d, x, e, T, k, A), u = -1, o = -1, i = x.length - 1; - 1 !== i; --i)(t = x[i]) <= e || (c = b(d[k[t]])) > u && (o = t, u = c);
            for (b(d[k[e]]) < n * u && (i = k[e], k[e] = u = k[o], T[u] = e, k[o] = i, T[i] = o), o = y[e], m = d[k[e]], p[u = h[e]] = k[e], l[u] = 1, ++u, i = x.length - 1; - 1 !== i; --i) c = d[k[t = x[i]]], x[i] = 0, d[k[t]] = 0, t <= e ? (g[o] = t, v[o] = c, ++o) : (p[u] = k[t], l[u] = c / m, ++u);
            h[e + 1] = u, y[e + 1] = o
        }
        for (i = p.length - 1; - 1 !== i; --i) p[i] = T[p[i]];
        return {
            L: f,
            U: s,
            P: k,
            Pinv: T
        }
    }, numeric.ccsLUP = numeric.ccsLUP0, numeric.ccsDim = function(r) {
        return [numeric.sup(r[1]) + 1, r[0].length - 1]
    }, numeric.ccsGetBlock = function(r, n, e) {
        var i = numeric.ccsDim(r),
            t = i[0],
            u = i[1];
        void 0 === n ? n = numeric.linspace(0, t - 1) : "number" == typeof n && (n = [n]), void 0 === e ? e = numeric.linspace(0, u - 1) : "number" == typeof e && (e = [e]);
        var o, c, m, a, f = n.length,
            s = e.length,
            h = numeric.rep([u], 0),
            p = [],
            l = [],
            y = [h, p, l],
            g = r[0],
            v = r[1],
            d = r[2],
            x = numeric.rep([t], 0),
            w = 0,
            b = numeric.rep([t], 0);
        for (c = 0; c < s; ++c) {
            var k = g[a = e[c]],
                T = g[a + 1];
            for (o = k; o < T; ++o) b[m = v[o]] = 1, x[m] = d[o];
            for (o = 0; o < f; ++o) b[n[o]] && (p[w] = o, l[w] = x[n[o]], ++w);
            for (o = k; o < T; ++o) b[m = v[o]] = 0;
            h[c + 1] = w
        }
        return y
    }, numeric.ccsDot = function(r, n) {
        var e, i, t, u, o, c, m, a, f, s, h = r[0],
            p = r[1],
            l = r[2],
            y = n[0],
            g = n[1],
            v = n[2],
            d = numeric.ccsDim(r),
            x = numeric.ccsDim(n),
            w = d[0],
            b = x[1],
            k = numeric.rep([w], 0),
            T = numeric.rep([w], 0),
            A = Array(w),
            j = numeric.rep([b], 0),
            M = [],
            _ = [],
            S = [j, M, _];
        for (t = 0; t !== b; ++t) {
            for (o = y[t + 1], a = 0, i = y[t]; i < o; ++i)
                for (s = v[i], c = h[(f = g[i]) + 1], e = h[f]; e < c; ++e) 0 === T[m = p[e]] && (A[a] = m, T[m] = 1, a += 1), k[m] = k[m] + l[e] * s;
            for (j[t + 1] = o = (u = j[t]) + a, i = a - 1; - 1 !== i; --i) M[s = u + i] = e = A[i], _[s] = k[e], T[e] = 0, k[e] = 0;
            j[t + 1] = j[t] + a
        }
        return S
    }, numeric.ccsLUPSolve = function(r, n) {
        var e = r.L,
            i = r.U,
            t = n[0],
            u = !1;
        "object" != typeof t && (t = (n = [
            [0, n.length], numeric.linspace(0, n.length - 1), n
        ])[0], u = !0);
        var o, c, m, a, f, s = n[1],
            h = n[2],
            p = e[0].length - 1,
            l = t.length - 1,
            y = numeric.rep([p], 0),
            g = Array(p),
            v = numeric.rep([p], 0),
            d = Array(p),
            x = numeric.rep([l + 1], 0),
            w = [],
            b = [],
            k = numeric.ccsTSolve,
            T = 0;
        for (o = 0; o < l; ++o) {
            for (a = 0, m = t[o + 1], c = t[o]; c < m; ++c) d[a] = f = r.Pinv[s[c]], v[f] = h[c], ++a;
            for (d.length = a, k(e, v, y, d, g), c = d.length - 1; - 1 !== c; --c) v[d[c]] = 0;
            if (k(i, y, v, g, d), u) return v;
            for (c = g.length - 1; - 1 !== c; --c) y[g[c]] = 0;
            for (c = d.length - 1; - 1 !== c; --c) w[T] = f = d[c], b[T] = v[f], v[f] = 0, ++T;
            x[o + 1] = T
        }
        return [x, w, b]
    }, numeric.ccsbinop = function(r, n) {
        return void 0 === n && (n = ""), Function("X", "Y", "var Xi = X[0], Xj = X[1], Xv = X[2];\nvar Yi = Y[0], Yj = Y[1], Yv = Y[2];\nvar n = Xi.length-1,m = Math.max(numeric.sup(Xj),numeric.sup(Yj))+1;\nvar Zi = numeric.rep([n+1],0), Zj = [], Zv = [];\nvar x = numeric.rep([m],0),y = numeric.rep([m],0);\nvar xk,yk,zk;\nvar i,j,j0,j1,k,p=0;\n" + n + "for(i=0;i<n;++i) {\n  j0 = Xi[i]; j1 = Xi[i+1];\n  for(j=j0;j!==j1;++j) {\n    k = Xj[j];\n    x[k] = 1;\n    Zj[p] = k;\n    ++p;\n  }\n  j0 = Yi[i]; j1 = Yi[i+1];\n  for(j=j0;j!==j1;++j) {\n    k = Yj[j];\n    y[k] = Yv[j];\n    if(x[k] === 0) {\n      Zj[p] = k;\n      ++p;\n    }\n  }\n  Zi[i+1] = p;\n  j0 = Xi[i]; j1 = Xi[i+1];\n  for(j=j0;j!==j1;++j) x[Xj[j]] = Xv[j];\n  j0 = Zi[i]; j1 = Zi[i+1];\n  for(j=j0;j!==j1;++j) {\n    k = Zj[j];\n    xk = x[k];\n    yk = y[k];\n" + r + "\n    Zv[j] = zk;\n  }\n  j0 = Xi[i]; j1 = Xi[i+1];\n  for(j=j0;j!==j1;++j) x[Xj[j]] = 0;\n  j0 = Yi[i]; j1 = Yi[i+1];\n  for(j=j0;j!==j1;++j) y[Yj[j]] = 0;\n}\nreturn [Zi,Zj,Zv];")
    },
    function() {
        var k, A, B, C;
        for (k in numeric.ops2) A = isFinite(eval("1" + numeric.ops2[k] + "0")) ? "[Y[0],Y[1],numeric." + k + "(X,Y[2])]" : "NaN", B = isFinite(eval("0" + numeric.ops2[k] + "1")) ? "[X[0],X[1],numeric." + k + "(X[2],Y)]" : "NaN", C = isFinite(eval("1" + numeric.ops2[k] + "0")) && isFinite(eval("0" + numeric.ops2[k] + "1")) ? "numeric.ccs" + k + "MM(X,Y)" : "NaN", numeric["ccs" + k + "MM"] = numeric.ccsbinop("zk = xk " + numeric.ops2[k] + "yk;"), numeric["ccs" + k] = Function("X", "Y", 'if(typeof X === "number") return ' + A + ';\nif(typeof Y === "number") return ' + B + ";\nreturn " + C + ";\n")
    }(), numeric.ccsScatter = function(r) {
        var n, e = r[0],
            i = r[1],
            t = r[2],
            u = numeric.sup(i) + 1,
            o = e.length,
            c = numeric.rep([u], 0),
            m = Array(o),
            a = Array(o),
            f = numeric.rep([u], 0);
        for (n = 0; n < o; ++n) f[i[n]]++;
        for (n = 0; n < u; ++n) c[n + 1] = c[n] + f[n];
        var s, h, p = c.slice(0);
        for (n = 0; n < o; ++n) m[s = p[h = i[n]]] = e[n], a[s] = t[n], p[h] = p[h] + 1;
        return [c, m, a]
    }, numeric.ccsGather = function(r) {
        var n, e, i, t, u = r[0],
            o = r[1],
            c = r[2],
            m = u.length - 1,
            a = o.length,
            f = Array(a),
            s = Array(a),
            h = Array(a);
        for (t = 0, n = 0; n < m; ++n)
            for (i = u[n + 1], e = u[n]; e !== i; ++e) s[t] = n, f[t] = o[e], h[t] = c[e], ++t;
        return [f, s, h]
    }, numeric.sdim = function r(n, e, i) {
        if (void 0 === e && (e = []), "object" != typeof n) return e;
        var t;
        for (t in void 0 === i && (i = 0), i in e || (e[i] = 0), n.length > e[i] && (e[i] = n.length), n) n.hasOwnProperty(t) && r(n[t], e, i + 1);
        return e
    }, numeric.sclone = function r(n, e, i) {
        void 0 === e && (e = 0), void 0 === i && (i = numeric.sdim(n).length);
        var t, u = Array(n.length);
        if (e === i - 1) {
            for (t in n) n.hasOwnProperty(t) && (u[t] = n[t]);
            return u
        }
        for (t in n) n.hasOwnProperty(t) && (u[t] = r(n[t], e + 1, i));
        return u
    }, numeric.sdiag = function(r) {
        var n, e, i = r.length,
            t = Array(i);
        for (n = i - 1; n >= 1; n -= 2) e = n - 1, t[n] = [], t[n][n] = r[n], t[e] = [], t[e][e] = r[e];
        return 0 === n && (t[0] = [], t[0][0] = r[n]), t
    }, numeric.sidentity = function(r) {
        return numeric.sdiag(numeric.rep([r], 1))
    }, numeric.stranspose = function(r) {
        var n, e, i, t = [];
        for (n in r)
            if (r.hasOwnProperty(n))
                for (e in i = r[n]) i.hasOwnProperty(e) && ("object" != typeof t[e] && (t[e] = []), t[e][n] = i[e]);
        return t
    }, numeric.sLUP = function(r, n) {
        throw new Error("The function numeric.sLUP had a bug in it and has been removed. Please use the new numeric.ccsLUP function instead.")
    }, numeric.sdotMM = function(r, n) {
        var e, i, t, u, o, c, m, a = r.length,
            f = numeric.stranspose(n),
            s = f.length,
            h = Array(a);
        for (t = a - 1; t >= 0; t--) {
            for (m = [], e = r[t], o = s - 1; o >= 0; o--) {
                for (u in c = 0, i = f[o], e) e.hasOwnProperty(u) && u in i && (c += e[u] * i[u]);
                c && (m[o] = c)
            }
            h[t] = m
        }
        return h
    }, numeric.sdotMV = function(r, n) {
        var e, i, t, u, o = r.length,
            c = Array(o);
        for (i = o - 1; i >= 0; i--) {
            for (t in u = 0, e = r[i]) e.hasOwnProperty(t) && n[t] && (u += e[t] * n[t]);
            u && (c[i] = u)
        }
        return c
    }, numeric.sdotVM = function(r, n) {
        var e, i, t, u, o = [];
        for (e in r)
            if (r.hasOwnProperty(e))
                for (i in u = r[e], t = n[e]) t.hasOwnProperty(i) && (o[i] || (o[i] = 0), o[i] += u * t[i]);
        return o
    }, numeric.sdotVV = function(r, n) {
        var e, i = 0;
        for (e in r) r[e] && n[e] && (i += r[e] * n[e]);
        return i
    }, numeric.sdot = function(r, n) {
        var e = numeric.sdim(r).length,
            i = numeric.sdim(n).length;
        switch (1e3 * e + i) {
            case 0:
                return r * n;
            case 1001:
                return numeric.sdotVV(r, n);
            case 2001:
                return numeric.sdotMV(r, n);
            case 1002:
                return numeric.sdotVM(r, n);
            case 2002:
                return numeric.sdotMM(r, n);
            default:
                throw new Error("numeric.sdot not implemented for tensors of order " + e + " and " + i)
        }
    }, numeric.sscatter = function(r) {
        var n, e, i, t, u = r.length,
            o = [];
        for (e = r[0].length - 1; e >= 0; --e)
            if (r[u - 1][e]) {
                for (t = o, i = 0; i < u - 2; i++) t[n = r[i][e]] || (t[n] = []), t = t[n];
                t[r[i][e]] = r[i + 1][e]
            } return o
    }, numeric.sgather = function r(n, e, i) {
        var t, u, o;
        for (u in void 0 === e && (e = []), void 0 === i && (i = []), t = i.length, n)
            if (n.hasOwnProperty(u))
                if (i[t] = parseInt(u), "number" == typeof(o = n[u])) {
                    if (o) {
                        if (0 === e.length)
                            for (u = t + 1; u >= 0; --u) e[u] = [];
                        for (u = t; u >= 0; --u) e[u].push(i[u]);
                        e[t + 1].push(o)
                    }
                } else r(o, e, i);
        return i.length > t && i.pop(), e
    }, numeric.cLU = function(r) {
        var n, e, i, t, u, o, c = r[0],
            m = r[1],
            a = r[2],
            f = c.length,
            s = 0;
        for (n = 0; n < f; n++) c[n] > s && (s = c[n]);
        s++;
        var h, p = Array(s),
            l = Array(s),
            y = numeric.rep([s], Infinity),
            g = numeric.rep([s], -Infinity);
        for (i = 0; i < f; i++)(e = m[i]) < y[n = c[i]] && (y[n] = e), e > g[n] && (g[n] = e);
        for (n = 0; n < s - 1; n++) g[n] > g[n + 1] && (g[n + 1] = g[n]);
        for (n = s - 1; n >= 1; n--) y[n] < y[n - 1] && (y[n - 1] = y[n]);
        for (n = 0; n < s; n++) l[n] = numeric.rep([g[n] - y[n] + 1], 0), p[n] = numeric.rep([n - y[n]], 0);
        for (i = 0; i < f; i++) l[n = c[i]][m[i] - y[n]] = a[i];
        for (n = 0; n < s - 1; n++)
            for (t = n - y[n], x = l[n], e = n + 1; y[e] <= n && e < s; e++)
                if (o = g[n] - n, h = (w = l[e])[u = n - y[e]] / x[t]) {
                    for (i = 1; i <= o; i++) w[i + u] -= h * x[i + t];
                    p[e][n - y[e]] = h
                } var v, d, x = [],
            w = [],
            b = [],
            k = [],
            T = [],
            A = [];
        for (f = 0, v = 0, n = 0; n < s; n++) {
            for (t = y[n], u = g[n], d = l[n], e = n; e <= u; e++) d[e - t] && (x[f] = n, w[f] = e, b[f] = d[e - t], f++);
            for (d = p[n], e = t; e < n; e++) d[e - t] && (k[v] = n, T[v] = e, A[v] = d[e - t], v++);
            k[v] = n, T[v] = n, A[v] = 1, v++
        }
        return {
            U: [x, w, b],
            L: [k, T, A]
        }
    }, numeric.cLUsolve = function(r, n) {
        var e, i, t = r.L,
            u = r.U,
            o = numeric.clone(n),
            c = t[1],
            m = t[2],
            a = u[1],
            f = u[2],
            s = u[0].length,
            h = o.length;
        for (i = 0, e = 0; e < h; e++) {
            for (; c[i] < e;) o[e] -= m[i] * o[c[i]], i++;
            i++
        }
        for (i = s - 1, e = h - 1; e >= 0; e--) {
            for (; a[i] > e;) o[e] -= f[i] * o[a[i]], i--;
            o[e] /= f[i], i--
        }
        return o
    }, numeric.cgrid = function(r, n) {
        "number" == typeof r && (r = [r, r]);
        var e, i, t, u = numeric.rep(r, -1);
        if ("function" != typeof n) switch (n) {
            case "L":
                n = function(n, e) {
                    return n >= r[0] / 2 || e < r[1] / 2
                };
                break;
            default:
                n = function(r, n) {
                    return !0
                }
        }
        for (t = 0, e = 1; e < r[0] - 1; e++)
            for (i = 1; i < r[1] - 1; i++) n(e, i) && (u[e][i] = t, t++);
        return u
    }, numeric.cdelsq = function(r) {
        var n, e, i, t, u, o = [
                [-1, 0],
                [0, -1],
                [0, 1],
                [1, 0]
            ],
            c = numeric.dim(r),
            m = c[0],
            a = c[1],
            f = [],
            s = [],
            h = [];
        for (n = 1; n < m - 1; n++)
            for (e = 1; e < a - 1; e++)
                if (!(r[n][e] < 0)) {
                    for (i = 0; i < 4; i++) r[t = n + o[i][0]][u = e + o[i][1]] < 0 || (f.push(r[n][e]), s.push(r[t][u]), h.push(-1));
                    f.push(r[n][e]), s.push(r[n][e]), h.push(4)
                } return [f, s, h]
    }, numeric.cdotMV = function(r, n) {
        var e, i, t, u = r[0],
            o = r[1],
            c = r[2],
            m = u.length;
        for (t = 0, i = 0; i < m; i++) u[i] > t && (t = u[i]);
        for (e = numeric.rep([++t], 0), i = 0; i < m; i++) e[u[i]] += c[i] * n[o[i]];
        return e
    }, numeric.Spline = function(r, n, e, i, t) {
        this.x = r, this.yl = n, this.yr = e, this.kl = i, this.kr = t
    }, numeric.Spline.prototype._at = function(r, n) {
        var e, i, t, u = this.x,
            o = this.yl,
            c = this.yr,
            m = this.kr,
            a = numeric.add,
            f = numeric.sub,
            s = numeric.mul;
        e = f(s(this.kl[n], u[n + 1] - u[n]), f(c[n + 1], o[n])), i = a(s(m[n + 1], u[n] - u[n + 1]), f(c[n + 1], o[n]));
        var h = (t = (r - u[n]) / (u[n + 1] - u[n])) * (1 - t);
        return a(a(a(s(1 - t, o[n]), s(t, c[n + 1])), s(e, h * (1 - t))), s(i, h * t))
    }, numeric.Spline.prototype.at = function(r) {
        if ("number" == typeof r) {
            var n, e, i, t = this.x,
                u = t.length,
                o = Math.floor;
            for (n = 0, e = u - 1; e - n > 1;) t[i = o((n + e) / 2)] <= r ? n = i : e = i;
            return this._at(r, n)
        }
        u = r.length;
        var c, m = Array(u);
        for (c = u - 1; - 1 !== c; --c) m[c] = this.at(r[c]);
        return m
    }, numeric.Spline.prototype.diff = function() {
        var r, n, e, i = this.x,
            t = this.yl,
            u = this.yr,
            o = this.kl,
            c = this.kr,
            m = t.length,
            a = o,
            f = c,
            s = Array(m),
            h = Array(m),
            p = numeric.add,
            l = numeric.mul,
            y = numeric.div,
            g = numeric.sub;
        for (r = m - 1; - 1 !== r; --r) n = i[r + 1] - i[r], e = g(u[r + 1], t[r]), s[r] = y(p(l(e, 6), l(o[r], -4 * n), l(c[r + 1], -2 * n)), n * n), h[r + 1] = y(p(l(e, -6), l(o[r], 2 * n), l(c[r + 1], 4 * n)), n * n);
        return new numeric.Spline(i, a, f, s, h)
    }, numeric.Spline.prototype.roots = function() {
        function r(r) {
            return r * r
        }
        var n = [],
            e = this.x,
            i = this.yl,
            t = this.yr,
            u = this.kl,
            o = this.kr;
        "number" == typeof i[0] && (i = [i], t = [t], u = [u], o = [o]);
        var c, m, a, f, s, h, p, l, y, g, v, d, x, w, b, k, T, A, j, M, _, S, V, P = i.length,
            F = e.length - 1,
            q = (n = Array(P), Math.sqrt);
        for (c = 0; c !== P; ++c) {
            for (f = i[c], s = t[c], h = u[c], p = o[c], l = [], m = 0; m !== F; m++) {
                for (m > 0 && s[m] * f[m] < 0 && l.push(e[m]), x = (g = p[m + 1] / (k = e[m + 1] - e[m])) + 3 * (v = f[m]) + 2 * (y = h[m] / k) - 3 * (d = s[m + 1]), w = 3 * (g + y + 2 * (v - d)), (b = r(y - g + 3 * (v - d)) + 12 * g * v) <= 0 ? T = (A = x / w) > e[m] && A < e[m + 1] ? [e[m], A, e[m + 1]] : [e[m], e[m + 1]] : (A = (x - q(b)) / w, j = (x + q(b)) / w, T = [e[m]], A > e[m] && A < e[m + 1] && T.push(A), j > e[m] && j < e[m + 1] && T.push(j), T.push(e[m + 1])), A = this._at(_ = T[0], m), a = 0; a < T.length - 1; a++)
                    if (j = this._at(S = T[a + 1], m), 0 !== A)
                        if (0 === j || A * j > 0) _ = S, A = j;
                        else {
                            for (var L = 0; !((V = (A * S - j * _) / (A - j)) <= _ || V >= S);)
                                if ((M = this._at(V, m)) * j > 0) S = V, j = M, -1 === L && (A *= .5), L = -1;
                                else {
                                    if (!(M * A > 0)) break;
                                    _ = V, A = M, 1 === L && (j *= .5), L = 1
                                } l.push(V), A = this._at(_ = T[a + 1], m)
                        }
                else l.push(_), _ = S, A = j;
                0 === j && l.push(S)
            }
            n[c] = l
        }
        return "number" == typeof this.yl[0] ? n[0] : n
    }, numeric.spline = function(r, n, e, i) {
        var t, u = r.length,
            o = [],
            c = [],
            m = [],
            a = numeric.sub,
            f = numeric.mul,
            s = numeric.add;
        for (t = u - 2; t >= 0; t--) c[t] = r[t + 1] - r[t], m[t] = a(n[t + 1], n[t]);
        "string" != typeof e && "string" != typeof i || (e = i = "periodic");
        var h = [
            [],
            [],
            []
        ];
        switch (typeof e) {
            case "undefined":
                o[0] = f(3 / (c[0] * c[0]), m[0]), h[0].push(0, 0), h[1].push(0, 1), h[2].push(2 / c[0], 1 / c[0]);
                break;
            case "string":
                o[0] = s(f(3 / (c[u - 2] * c[u - 2]), m[u - 2]), f(3 / (c[0] * c[0]), m[0])), h[0].push(0, 0, 0), h[1].push(u - 2, 0, 1), h[2].push(1 / c[u - 2], 2 / c[u - 2] + 2 / c[0], 1 / c[0]);
                break;
            default:
                o[0] = e, h[0].push(0), h[1].push(0), h[2].push(1)
        }
        for (t = 1; t < u - 1; t++) o[t] = s(f(3 / (c[t - 1] * c[t - 1]), m[t - 1]), f(3 / (c[t] * c[t]), m[t])), h[0].push(t, t, t), h[1].push(t - 1, t, t + 1), h[2].push(1 / c[t - 1], 2 / c[t - 1] + 2 / c[t], 1 / c[t]);
        switch (typeof i) {
            case "undefined":
                o[u - 1] = f(3 / (c[u - 2] * c[u - 2]), m[u - 2]), h[0].push(u - 1, u - 1), h[1].push(u - 2, u - 1), h[2].push(1 / c[u - 2], 2 / c[u - 2]);
                break;
            case "string":
                h[1][h[1].length - 1] = 0;
                break;
            default:
                o[u - 1] = i, h[0].push(u - 1), h[1].push(u - 1), h[2].push(1)
        }
        o = "number" != typeof o[0] ? numeric.transpose(o) : [o];
        var p = Array(o.length);
        if ("string" == typeof e)
            for (t = p.length - 1; - 1 !== t; --t) p[t] = numeric.ccsLUPSolve(numeric.ccsLUP(numeric.ccsScatter(h)), o[t]), p[t][u - 1] = p[t][0];
        else
            for (t = p.length - 1; - 1 !== t; --t) p[t] = numeric.cLUsolve(numeric.cLU(h), o[t]);
        return p = "number" == typeof n[0] ? p[0] : numeric.transpose(p), new numeric.Spline(r, n, n, p, p)
    }, numeric.fftpow2 = function r(n, e) {
        var i = n.length;
        if (1 !== i) {
            var t, u, o = Math.cos,
                c = Math.sin,
                m = Array(i / 2),
                a = Array(i / 2),
                f = Array(i / 2),
                s = Array(i / 2);
            for (u = i / 2, t = i - 1; - 1 !== t; --t) f[--u] = n[t], s[u] = e[t], m[u] = n[--t], a[u] = e[t];
            r(m, a), r(f, s), u = i / 2;
            var h, p, l, y = -6.283185307179586 / i;
            for (t = i - 1; - 1 !== t; --t) - 1 == --u && (u = i / 2 - 1), p = o(h = y * t), l = c(h), n[t] = m[u] + p * f[u] - l * s[u], e[t] = a[u] + p * s[u] + l * f[u]
        }
    }, numeric._ifftpow2 = function r(n, e) {
        var i = n.length;
        if (1 !== i) {
            var t, u, o = Math.cos,
                c = Math.sin,
                m = Array(i / 2),
                a = Array(i / 2),
                f = Array(i / 2),
                s = Array(i / 2);
            for (u = i / 2, t = i - 1; - 1 !== t; --t) f[--u] = n[t], s[u] = e[t], m[u] = n[--t], a[u] = e[t];
            r(m, a), r(f, s), u = i / 2;
            var h, p, l, y = 6.283185307179586 / i;
            for (t = i - 1; - 1 !== t; --t) - 1 == --u && (u = i / 2 - 1), p = o(h = y * t), l = c(h), n[t] = m[u] + p * f[u] - l * s[u], e[t] = a[u] + p * s[u] + l * f[u]
        }
    }, numeric.ifftpow2 = function(r, n) {
        numeric._ifftpow2(r, n), numeric.diveq(r, r.length), numeric.diveq(n, n.length)
    }, numeric.convpow2 = function(r, n, e, i) {
        var t, u, o, c, m;
        for (numeric.fftpow2(r, n), numeric.fftpow2(e, i), t = r.length - 1; - 1 !== t; --t) r[t] = (u = r[t]) * (o = e[t]) - (c = n[t]) * (m = i[t]), n[t] = u * m + c * o;
        numeric.ifftpow2(r, n)
    }, numeric.T.prototype.fft = function() {
        var r, n, e = this.x,
            i = this.y,
            t = e.length,
            u = Math.log,
            o = u(2),
            c = Math.ceil(u(2 * t - 1) / o),
            m = Math.pow(2, c),
            a = numeric.rep([m], 0),
            f = numeric.rep([m], 0),
            s = Math.cos,
            h = Math.sin,
            p = -3.141592653589793 / t,
            l = numeric.rep([m], 0),
            y = numeric.rep([m], 0);
        for (r = 0; r < t; r++) l[r] = e[r];
        if (void 0 !== i)
            for (r = 0; r < t; r++) y[r] = i[r];
        for (a[0] = 1, r = 1; r <= m / 2; r++) a[r] = s(n = p * r * r), f[r] = h(n), a[m - r] = s(n), f[m - r] = h(n);
        var g = new numeric.T(l, y),
            v = new numeric.T(a, f);
        return g = g.mul(v), numeric.convpow2(g.x, g.y, numeric.clone(v.x), numeric.neg(v.y)), (g = g.mul(v)).x.length = t, g.y.length = t, g
    }, numeric.T.prototype.ifft = function() {
        var r, n, e = this.x,
            i = this.y,
            t = e.length,
            u = Math.log,
            o = u(2),
            c = Math.ceil(u(2 * t - 1) / o),
            m = Math.pow(2, c),
            a = numeric.rep([m], 0),
            f = numeric.rep([m], 0),
            s = Math.cos,
            h = Math.sin,
            p = 3.141592653589793 / t,
            l = numeric.rep([m], 0),
            y = numeric.rep([m], 0);
        for (r = 0; r < t; r++) l[r] = e[r];
        if (void 0 !== i)
            for (r = 0; r < t; r++) y[r] = i[r];
        for (a[0] = 1, r = 1; r <= m / 2; r++) a[r] = s(n = p * r * r), f[r] = h(n), a[m - r] = s(n), f[m - r] = h(n);
        var g = new numeric.T(l, y),
            v = new numeric.T(a, f);
        return g = g.mul(v), numeric.convpow2(g.x, g.y, numeric.clone(v.x), numeric.neg(v.y)), (g = g.mul(v)).x.length = t, g.y.length = t, g.div(t)
    }, numeric.gradient = function(r, n) {
        var e = n.length,
            i = r(n);
        if (isNaN(i)) throw new Error("gradient: f(x) is a NaN!");
        var t, u, o, c, m, a, f, s, h, p = Math.max,
            l = numeric.clone(n),
            y = Array(e),
            g = (p = Math.max, Math.abs),
            v = Math.min,
            d = 0;
        for (t = 0; t < e; t++)
            for (var x = p(1e-6 * i, 1e-8);;) {
                if (++d > 20) throw new Error("Numerical gradient fails");
                if (l[t] = n[t] + x, u = r(l), l[t] = n[t] - x, o = r(l), l[t] = n[t], isNaN(u) || isNaN(o)) x /= 16;
                else {
                    if (y[t] = (u - o) / (2 * x), c = n[t] - x, m = n[t], a = n[t] + x, f = (u - i) / x, s = (i - o) / x, h = p(g(y[t]), g(i), g(u), g(o), g(c), g(m), g(a), 1e-8), !(v(p(g(f - y[t]), g(s - y[t]), g(f - s)) / h, x / h) > .001)) break;
                    x /= 16
                }
            }
        return y
    }, numeric.uncmin = function(r, n, e, i, t, u, o) {
        var c = numeric.gradient;
        void 0 === o && (o = {}), void 0 === e && (e = 1e-8), void 0 === i && (i = function(n) {
            return c(r, n)
        }), void 0 === t && (t = 1e3);
        var m, a, f = (n = numeric.clone(n)).length,
            s = r(n);
        if (isNaN(s)) throw new Error("uncmin: f(x0) is a NaN!");
        var h = Math.max,
            p = numeric.norm2;
        e = h(e, numeric.epsilon);
        var l, y, g, v, d, x, w, b, k, T, A = o.Hinv || numeric.identity(f),
            j = numeric.dot,
            M = numeric.sub,
            _ = numeric.add,
            S = numeric.tensor,
            V = numeric.div,
            P = numeric.mul,
            F = numeric.all,
            q = numeric.isFinite,
            L = numeric.neg,
            N = 0,
            O = "";
        for (y = i(n); N < t;) {
            if ("function" == typeof u && u(N, n, s, y, A)) {
                O = "Callback returned true";
                break
            }
            if (!F(q(y))) {
                O = "Gradient has Infinity or NaN";
                break
            }
            if (!F(q(l = L(j(A, y))))) {
                O = "Search direction has Infinity or NaN";
                break
            }
            if ((T = p(l)) < e) {
                O = "Newton step smaller than tol";
                break
            }
            for (k = 1, a = j(y, l), d = n; N < t && !(k * T < e) && (d = _(n, v = P(l, k)), (m = r(d)) - s >= .1 * k * a || isNaN(m));) k *= .5, ++N;
            if (k * T < e) {
                O = "Line search step size smaller than tol";
                break
            }
            if (N === t) {
                O = "maxit reached during line search";
                break
            }
            b = j(x = M(g = i(d), y), v), w = j(A, x), A = M(_(A, P((b + j(x, w)) / (b * b), S(v, v))), V(_(S(w, v), S(v, w)), b)), n = d, s = m, y = g, ++N
        }
        return {
            solution: n,
            f: s,
            gradient: y,
            invHessian: A,
            iterations: N,
            message: O
        }
    }, numeric.Dopri = function(r, n, e, i, t, u, o) {
        this.x = r, this.y = n, this.f = e, this.ymid = i, this.iterations = t, this.events = o, this.message = u
    }, numeric.Dopri.prototype._at = function(r, n) {
        function e(r) {
            return r * r
        }
        var i, t, u, o, c, m, a, f, s, h = this.x,
            p = this.y,
            l = this.f,
            y = numeric.add,
            g = numeric.mul,
            v = numeric.sub;
        return c = p[n + 1], m = this.ymid[n], a = v(l[n], g(o = p[n], 1 / ((i = h[n]) - (u = i + .5 * ((t = h[n + 1]) - i))) + 2 / (i - t))), f = v(l[n + 1], g(c, 1 / (t - u) + 2 / (t - i))), y(y(y(y(g(o, (s = [e(r - t) * (r - u) / e(i - t) / (i - u), e(r - i) * e(r - t) / e(i - u) / e(t - u), e(r - i) * (r - u) / e(t - i) / (t - u), (r - i) * e(r - t) * (r - u) / e(i - t) / (i - u), (r - t) * e(r - i) * (r - u) / e(i - t) / (t - u)])[0]), g(m, s[1])), g(c, s[2])), g(a, s[3])), g(f, s[4]))
    }, numeric.Dopri.prototype.at = function(r) {
        var n, e, i, t = Math.floor;
        if ("number" != typeof r) {
            var u = r.length,
                o = Array(u);
            for (n = u - 1; - 1 !== n; --n) o[n] = this.at(r[n]);
            return o
        }
        var c = this.x;
        for (n = 0, e = c.length - 1; e - n > 1;) c[i = t(.5 * (n + e))] <= r ? n = i : e = i;
        return this._at(r, n)
    }, numeric.dopri = function(r, n, e, i, t, u, o) {
        void 0 === t && (t = 1e-6), void 0 === u && (u = 1e3);
        var c, m, a, f, s, h, p, l, y, g, v, d, x, w = [r],
            b = [e],
            k = [i(r, e)],
            T = [],
            A = [.075, .225],
            j = [44 / 45, -56 / 15, 32 / 9],
            M = [19372 / 6561, -25360 / 2187, 64448 / 6561, -212 / 729],
            _ = [9017 / 3168, -355 / 33, 46732 / 5247, 49 / 176, -5103 / 18656],
            S = [35 / 384, 0, 500 / 1113, 125 / 192, -2187 / 6784, 11 / 84],
            V = [.10013431883002395, 0, .3918321794184259, -.02982460176594817, .05893268337240795, -.04497888809104361, .023904308236133973],
            P = [.2, .3, .8, 8 / 9, 1, 1],
            F = [-71 / 57600, 0, 71 / 16695, -71 / 1920, 17253 / 339200, -22 / 525, .025],
            q = 0,
            L = (n - r) / 10,
            N = 0,
            O = numeric.add,
            U = numeric.mul,
            B = Math.min,
            D = Math.abs,
            R = numeric.norminf,
            X = Math.pow,
            E = numeric.any,
            Y = numeric.lt,
            I = numeric.and,
            Q = new numeric.Dopri(w, b, k, T, -1, "");
        for ("function" == typeof o && (v = o(r, e)); r < n && N < u;)
            if (++N, r + L > n && (L = n - r), c = i(r + P[0] * L, O(e, U(.2 * L, k[q]))), m = i(r + P[1] * L, O(O(e, U(A[0] * L, k[q])), U(A[1] * L, c))), a = i(r + P[2] * L, O(O(O(e, U(j[0] * L, k[q])), U(j[1] * L, c)), U(j[2] * L, m))), f = i(r + P[3] * L, O(O(O(O(e, U(M[0] * L, k[q])), U(M[1] * L, c)), U(M[2] * L, m)), U(M[3] * L, a))), s = i(r + P[4] * L, O(O(O(O(O(e, U(_[0] * L, k[q])), U(_[1] * L, c)), U(_[2] * L, m)), U(_[3] * L, a)), U(_[4] * L, f))), h = i(r + L, y = O(O(O(O(O(e, U(k[q], L * S[0])), U(m, L * S[2])), U(a, L * S[3])), U(f, L * S[4])), U(s, L * S[5]))), (g = "number" == typeof(p = O(O(O(O(O(U(k[q], L * F[0]), U(m, L * F[2])), U(a, L * F[3])), U(f, L * F[4])), U(s, L * F[5])), U(h, L * F[6]))) ? D(p) : R(p)) > t) {
                if (r + (L = .2 * L * X(t / g, .25)) === r) {
                    Q.msg = "Step size became too small";
                    break
                }
            } else {
                if (T[q] = O(O(O(O(O(O(e, U(k[q], L * V[0])), U(m, L * V[2])), U(a, L * V[3])), U(f, L * V[4])), U(s, L * V[5])), U(h, L * V[6])), w[++q] = r + L, b[q] = y, k[q] = h, "function" == typeof o) {
                    var Z, C, H = r,
                        z = r + .5 * L;
                    if (d = o(z, T[q - 1]), E(x = I(Y(v, 0), Y(0, d))) || (H = z, v = d, d = o(z = r + L, y), x = I(Y(v, 0), Y(0, d))), E(x)) {
                        for (var G, $, W = 0, J = 1, K = 1;;) {
                            if ("number" == typeof v) C = (K * d * H - J * v * z) / (K * d - J * v);
                            else
                                for (C = z, l = v.length - 1; - 1 !== l; --l) v[l] < 0 && d[l] > 0 && (C = B(C, (K * d[l] * H - J * v[l] * z) / (K * d[l] - J * v[l])));
                            if (C <= H || C >= z) break;
                            $ = o(C, Z = Q._at(C, q - 1)), E(G = I(Y(v, 0), Y(0, $))) ? (z = C, d = $, x = G, K = 1, -1 === W ? J *= .5 : J = 1, W = -1) : (H = C, v = $, J = 1, 1 === W ? K *= .5 : K = 1, W = 1)
                        }
                        return y = Q._at(.5 * (r + C), q - 1), Q.f[q] = i(C, Z), Q.x[q] = C, Q.y[q] = Z, Q.ymid[q - 1] = y, Q.events = x, Q.iterations = N, Q
                    }
                }
                r += L, e = y, v = d, L = B(.8 * L * X(t / g, .25), 4 * L)
            } return Q.iterations = N, Q
    }, numeric.LU = function(r, n) {
        n = n || !1;
        var e, i, t, u, o, c, m, a, f, s = Math.abs,
            h = r.length,
            p = h - 1,
            l = new Array(h);
        for (n || (r = numeric.clone(r)), t = 0; t < h; ++t) {
            for (m = t, f = s((c = r[t])[t]), i = t + 1; i < h; ++i) f < (u = s(r[i][t])) && (f = u, m = i);
            for (l[t] = m, m != t && (r[t] = r[m], r[m] = c, c = r[t]), o = c[t], e = t + 1; e < h; ++e) r[e][t] /= o;
            for (e = t + 1; e < h; ++e) {
                for (a = r[e], i = t + 1; i < p; ++i) a[i] -= a[t] * c[i], a[++i] -= a[t] * c[i];
                i === p && (a[i] -= a[t] * c[i])
            }
        }
        return {
            LU: r,
            P: l
        }
    }, numeric.LUsolve = function(r, n) {
        var e, i, t, u, o, c = r.LU,
            m = c.length,
            a = numeric.clone(n),
            f = r.P;
        for (e = m - 1; - 1 !== e; --e) a[e] = n[e];
        for (e = 0; e < m; ++e)
            for (t = f[e], f[e] !== e && (o = a[e], a[e] = a[t], a[t] = o), u = c[e], i = 0; i < e; ++i) a[e] -= a[i] * u[i];
        for (e = m - 1; e >= 0; --e) {
            for (u = c[e], i = e + 1; i < m; ++i) a[e] -= a[i] * u[i];
            a[e] /= u[e]
        }
        return a
    }, numeric.solve = function(r, n, e) {
        return numeric.LUsolve(numeric.LU(r, e), n)
    }, numeric.echelonize = function(r) {
        var n, e, i, t, u, o, c, m, a = numeric.dim(r),
            f = a[0],
            s = a[1],
            h = numeric.identity(f),
            p = Array(f),
            l = Math.abs,
            y = numeric.diveq;
        for (r = numeric.clone(r), n = 0; n < f; ++n) {
            for (i = 0, u = r[n], o = h[n], e = 1; e < s; ++e) l(u[i]) < l(u[e]) && (i = e);
            for (p[n] = i, y(o, u[i]), y(u, u[i]), e = 0; e < f; ++e)
                if (e !== n) {
                    for (m = (c = r[e])[i], t = s - 1; - 1 !== t; --t) c[t] -= u[t] * m;
                    for (c = h[e], t = f - 1; - 1 !== t; --t) c[t] -= o[t] * m
                }
        }
        return {
            I: h,
            A: r,
            P: p
        }
    }, numeric.__solveLP = function(r, n, e, i, t, u, o) {
        var c, m, a, f, s = numeric.sum,
            h = numeric.mul,
            p = numeric.sub,
            l = numeric.dot,
            y = numeric.div,
            g = numeric.add,
            v = r.length,
            d = e.length,
            x = !1,
            w = 1,
            b = (numeric.transpose(n), numeric.transpose),
            k = Math.sqrt,
            T = Math.abs,
            A = Math.min,
            j = numeric.all,
            M = numeric.gt,
            _ = Array(v),
            S = Array(d),
            V = (numeric.rep([d], 1), numeric.solve),
            P = p(e, l(n, u)),
            F = l(r, r);
        for (a = 0; a < t; ++a) {
            var q, L;
            for (q = d - 1; - 1 !== q; --q) S[q] = y(n[q], P[q]);
            var N = b(S);
            for (q = v - 1; - 1 !== q; --q) _[q] = s(N[q]);
            w = .25 * T(F / l(r, _));
            var O = 100 * k(F / l(_, _));
            for ((!isFinite(w) || w > O) && (w = O), f = g(r, h(w, _)), m = l(N, S), q = v - 1; - 1 !== q; --q) m[q][q] += 1;
            L = V(m, y(f, w), !0);
            var U = y(P, l(n, L)),
                B = 1;
            for (q = d - 1; - 1 !== q; --q) U[q] < 0 && (B = A(B, -.999 * U[q]));
            if (c = p(u, h(L, B)), !j(M(P = p(e, l(n, c)), 0))) return {
                solution: u,
                message: "",
                iterations: a
            };
            if (u = c, w < i) return {
                solution: c,
                message: "",
                iterations: a
            };
            if (o) {
                var D = l(r, f),
                    R = l(n, f);
                for (x = !0, q = d - 1; - 1 !== q; --q)
                    if (D * R[q] < 0) {
                        x = !1;
                        break
                    }
            } else x = !(u[v - 1] >= 0);
            if (x) return {
                solution: c,
                message: "Unbounded",
                iterations: a
            }
        }
        return {
            solution: u,
            message: "maximum iteration count exceeded",
            iterations: a
        }
    }, numeric._solveLP = function(r, n, e, i, t) {
        var u = r.length,
            o = e.length,
            c = numeric.sub,
            m = numeric.dot,
            a = numeric.rep([u], 0).concat([1]),
            f = numeric.rep([o, 1], -1),
            s = numeric.blockMatrix([
                [n, f]
            ]),
            h = e,
            p = numeric.rep([u], 0).concat(Math.max(0, numeric.sup(numeric.neg(e))) + 1),
            l = numeric.__solveLP(a, s, h, i, t, p, !1),
            y = numeric.clone(l.solution);
        if (y.length = u, numeric.inf(c(e, m(n, y))) < 0) return {
            solution: NaN,
            message: "Infeasible",
            iterations: l.iterations
        };
        var g = numeric.__solveLP(r, n, e, i, t - l.iterations, y, !0);
        return g.iterations += l.iterations, g
    }, numeric.solveLP = function(r, n, e, i, t, u, o) {
        if (void 0 === o && (o = 1e3), void 0 === u && (u = numeric.epsilon), void 0 === i) return numeric._solveLP(r, n, e, u, o);
        var c, m = i.length,
            a = i[0].length,
            f = n.length,
            s = numeric.echelonize(i),
            h = numeric.rep([a], 0),
            p = s.P,
            l = [];
        for (c = p.length - 1; - 1 !== c; --c) h[p[c]] = 1;
        for (c = a - 1; - 1 !== c; --c) 0 === h[c] && l.push(c);
        var y = numeric.getRange,
            g = numeric.linspace(0, m - 1),
            v = numeric.linspace(0, f - 1),
            d = y(i, g, l),
            x = y(n, v, p),
            w = y(n, v, l),
            b = numeric.dot,
            k = numeric.sub,
            T = b(x, s.I),
            A = k(w, b(T, d)),
            j = k(e, b(T, t)),
            M = Array(p.length),
            _ = Array(l.length);
        for (c = p.length - 1; - 1 !== c; --c) M[c] = r[p[c]];
        for (c = l.length - 1; - 1 !== c; --c) _[c] = r[l[c]];
        var S = k(_, b(M, b(s.I, d))),
            V = numeric._solveLP(S, A, j, u, o),
            P = V.solution;
        if (P != P) return V;
        var F = b(s.I, k(t, b(d, P))),
            q = Array(r.length);
        for (c = p.length - 1; - 1 !== c; --c) q[p[c]] = F[c];
        for (c = l.length - 1; - 1 !== c; --c) q[l[c]] = P[c];
        return {
            solution: q,
            message: V.message,
            iterations: V.iterations
        }
    }, numeric.MPStoLP = function(r) {
        r instanceof String && r.split("\n");
        var n, e, i, t, u = 0,
            o = ["Initial state", "NAME", "ROWS", "COLUMNS", "RHS", "BOUNDS", "ENDATA"],
            c = r.length,
            m = 0,
            a = {},
            f = [],
            s = 0,
            h = {},
            p = 0,
            l = [],
            y = [],
            g = [];

        function v(e) {
            throw new Error("MPStoLP: " + e + "\nLine " + n + ": " + r[n] + "\nCurrent state: " + o[u] + "\n")
        }
        for (n = 0; n < c; ++n) {
            var d = (i = r[n]).match(/\S*/g),
                x = [];
            for (e = 0; e < d.length; ++e) "" !== d[e] && x.push(d[e]);
            if (0 !== x.length) {
                for (e = 0; e < o.length && i.substr(0, o[e].length) !== o[e]; ++e);
                if (e < o.length) {
                    if (u = e, 1 === e && (t = x[1]), 6 === e) return {
                        name: t,
                        c: l,
                        A: numeric.transpose(y),
                        b: g,
                        rows: a,
                        vars: h
                    }
                } else switch (u) {
                    case 0:
                    case 1:
                        v("Unexpected line");
                    case 2:
                        switch (x[0]) {
                            case "N":
                                0 === m ? m = x[1] : v("Two or more N rows");
                                break;
                            case "L":
                                a[x[1]] = s, f[s] = 1, g[s] = 0, ++s;
                                break;
                            case "G":
                                a[x[1]] = s, f[s] = -1, g[s] = 0, ++s;
                                break;
                            case "E":
                                a[x[1]] = s, f[s] = 0, g[s] = 0, ++s;
                                break;
                            default:
                                v("Parse error " + numeric.prettyPrint(x))
                        }
                        break;
                    case 3:
                        h.hasOwnProperty(x[0]) || (h[x[0]] = p, l[p] = 0, y[p] = numeric.rep([s], 0), ++p);
                        var w = h[x[0]];
                        for (e = 1; e < x.length; e += 2)
                            if (x[e] !== m) {
                                var b = a[x[e]];
                                y[w][b] = (f[b] < 0 ? -1 : 1) * parseFloat(x[e + 1])
                            } else l[w] = parseFloat(x[e + 1]);
                        break;
                    case 4:
                        for (e = 1; e < x.length; e += 2) g[a[x[e]]] = (f[a[x[e]]] < 0 ? -1 : 1) * parseFloat(x[e + 1]);
                        break;
                    case 5:
                        break;
                    case 6:
                        v("Internal error")
                }
            }
        }
        v("Reached end of file without ENDATA")
    }, module.exports = numeric;
//# sourceMappingURL=numeric.js.map
