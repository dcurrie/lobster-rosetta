
// Generate a collection filled with 1000 normally distributed random (or pseudo-random) numbers
// with a mean of   1.0   and a   standard deviation   of   0.5

let mean = 1.0
let stdv = 0.5
let count = 1000

// stats computes a running mean and variance 
// See Knuth TAOCP vol 2, 3rd edition, page 232

def stats(xs: [float]) -> float, float: // variance, mean
    var oldM = xs[0]
    var oldS = 0.0
    var n = 1.0
    for(xs.length - 1) i:
        let x = xs[i + 1]
        n = n + 1.0
        let newM = oldM + (x - oldM) / n
        let newS = oldS + (x - oldM) * (x - newM)
        // set up for next iteration
        oldM = newM
        oldS = newS
    //return (if n > 1.0: oldS / (n - 1.0) else: 0.0), oldM
    return (if n > 0.0: oldS / n else: 0.0), oldM
 
def test_random_normal() -> [float]:
    rnd_seed(floor(seconds_elapsed() * 1000000))
    let r = vector_reserve(typeof return, count)
    for (count):
        r.push(rnd_gaussian() * stdv + mean)
    let cvar, cmean = stats(r)
    let cstdv = sqrt(cvar)
    print concat_string(["Mean: ", string(cmean), ", Std.Deviation: ", string(cstdv)], "")

test_random_normal()
