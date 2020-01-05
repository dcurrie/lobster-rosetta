import std

def lcs(a, b) -> string:
    var out = ""
    let lengths = map(a.length * b.length): 0
    var greatestLength = 0
    for(a) x, i:
        for(b) y, j:
            if x == y:
                if i == 0 or j == 0:
                    lengths[i * b.length + j] = 1
                else:
                    lengths[i * b.length + j] = lengths[(i-1) * b.length + j - 1] + 1
                if lengths[i * b.length + j] > greatestLength:
                    greatestLength = lengths[i * b.length + j]
                    out = a.substring(i - greatestLength + 1, greatestLength)
    return out

def time (cnt, f):
    let s = seconds_elapsed()
    var r = ""
    for(cnt): r = f()
    let e = seconds_elapsed()
    print(r + " took " + (e-s) + " s")

//print(lcs("thisisatest", "testing123testing"))
//time(100000): lcs("thisisatest", "testing123testing")

def lcs2(a, b) -> string:
    var out = ""
    let lengths = map(b.length): map(a.length): 0
    var greatestLength = 0
    for(a) x, i:
        for(b) y, j:
            if x == y:
                if i == 0 or j == 0:
                    lengths[j][i] = 1
                else:
                    lengths[j][i] = lengths[j-1][i-1] + 1
                if lengths[j][i] > greatestLength:
                    greatestLength = lengths[j][i]
                    out = a.substring(i - greatestLength + 1, greatestLength)
    return out

//print(lcs2("thisisatest", "testing123testing"))
time(100000): lcs2("thisisatest", "testing123testing")
time(100000): lcs("thisisatest", "testing123testing")
