
import std

class DaryHeap<T>:
    // A d-ary heap implementing a priority queue.
    d: int // the fanout of the heap
    data: [T] // the type T must support functions: cmpHeap, getHeapIndex, setHeapIndex

def initDaryHeap<T>(d: int, v: T) -> DaryHeap<T>:
    // Create a new heap with one item.
    return DaryHeap { d: d, data: [v] }

def len<T>(heap: DaryHeap<T>) -> int:
    // Return the number of elements of `heap`.
    return length(heap.data)

def get<T>(heap: DaryHeap<T>, i: int) -> T:
    // Access the i-th element of `heap`
    return heap.data[i]

/*
class DaryHeapVal<P,V>:
    value: V
    priority: P // P must support P < P, e.g., int, float
    index: int

def cmpHeap(x, y: DaryHeapVal) -> int:
    return x.priority < y.priority

proc `[]`*[T](heap: DaryHeap[T], i: Natural): T {.inline.} =
  ## Access the i-th element of `heap`.
  heap.data[i]
*/

def siftdown<T>(heap: DaryHeap<T>, startpos, p: int):
    // 'heap' is a heap at all indices >= startpos, except possibly for pos.  pos
    // is the index of a leaf with a possibly out-of-order value.  Restore the
    // heap invariant.
    //echo("siftdown ", startpos, " ", p)
    var pos = p
    let newitem = heap.data[pos]
    //newitem.index = pos # taken care of below
    // Follow the path to the root, moving parents down until finding a place newitem fits.
    do():
        while pos > startpos:
            let parentpos = (pos - 1) / heap.d // floor
            let parent = heap.data[parentpos]
            if cmpHeap(newitem, parent):
                heap.data[pos] = parent
                setHeapIndex(parent, pos)
                pos = parentpos
            else:
                return false from do
    heap.data[pos] = newitem
    setHeapIndex(newitem, pos)

def siftup<T>(heap: DaryHeap<T>, p: int):
    //echo("siftup   ", p)
    let endpos = len(heap)
    var pos = p
    let startpos = pos
    let newitem = heap.data[pos]
    // Bubble up the smaller child until hitting a leaf.
    var childpos = heap.d * pos + 1 // leftmost child position
    while childpos < endpos:
        // Set childpos to index of smallest child.
        let dpos = childpos + heap.d
        var rightpos = childpos + 1
        while rightpos < dpos and rightpos < endpos:
            if not cmpHeap(heap.data[childpos], heap.data[rightpos]):
                childpos = rightpos
            rightpos = rightpos + 1
        // Move the smallest child up.
        heap.data[pos] = heap.data[childpos]
        setHeapIndex(heap.data[pos], pos)
        pos = childpos
        childpos = heap.d * pos + 1
    // The leaf at pos is empty now.  Put newitem there, and bubble it up
    // to its final resting place (by sifting its parents down).
    heap.data[pos] = newitem
    //setHeapIndex(newItem, pos) // done by siftdown
    siftdown(heap, startpos, pos)

def pushit<T>(heap: DaryHeap<T>, item: T):
    // Push `item` onto heap, maintaining the heap invariant.
    heap.data.push(item)
    siftdown(heap, 0, len(heap)-1)

def decr<T>(heap: DaryHeap<T>, item: T):
    // Decrease-priority of `item`, or push it if it's not in the heap.
    // Caller lowered the `priority` field of `item`.
    let pos = getHeapIndex(item)
    // use unsigned compare for 0 <= pos and pos < heap.len == pos <% heap.len
    if 0 <= pos and pos < heap.len and heap.data[pos] == item:
        siftdown(heap, 0, pos)
    else:
        pushit(heap, item)

def popit<T>(heap: DaryHeap<T>) -> T:
    // Pop and return the smallest item from `heap`,
    // maintaining the heap invariant.
    let lastelt = heap.data.pop()
    var result = lastelt
    if heap.len > 0:
        result = heap.data[0]
        heap.data[0] = lastelt
        //setHeapIndex(lastelt, 0)
        siftup(heap, 0)
    return result

def del<T>(heap: DaryHeap<T>, index: int):
    //echo("** del ", index)
    // Removes the element at `index` from `heap`, maintaining the heap invariant.
    let lastelt = heap.data.pop()
    if heap.len > index:
        heap.data[index] = lastelt
        setHeapIndex(lastelt, index)
        siftup(heap, index)

def replace<T>(heap: DaryHeap<T>, item: T) -> T:
    // Pop and return the current smallest value, and add the new item.
    // This is more efficient than popit() followed by pushit(), and can be
    // more appropriate when using a fixed-size heap. Note that the value
    // returned may be larger than item! That constrains reasonable uses of
    // this routine unless written as part of a conditional replacement:
    //
    // .. code-block:: nim
    //    if item > heap.data[0]:
    //        item = replace(heap, item)
    let result = heap.data[0]
    heap.data[0] = item
    setHeapIndex(item, 0)
    siftup(heap, 0)
    return result

def pushpop<T>(heap: DaryHeap<T>, item: T) -> T:
    // Fast version of a push followed by a pop.
    var result = item
    if heap.len > 0 and cmpHeap(heap.data[0], item):
      result = heap.data[0]
      heap.data[0] = item
      setHeapIndex(item, 0)
      siftup(heap, 0)
    return result

/*
def clear*[T](heap: var DaryHeap[T]) =
    // Remove all elements from `heap`, making it empty.
    //runnableExamples:
    //  type Job = ref object
    //    priority*: int
    //    index*: int
    //  proc `<`*(a, b: Job): bool = a.priority < b.priority
    //  proc `index=`*(x: var Job, value: int) {.inline.} = x.index = value
    //  var jobs = initDaryHeap[Job](4)
    //  jobs.pushit(Job(priority: 1))
    //  jobs.clear()
    //  assert jobs.len == 0
    heap.data.setLen(0)
*/

/*
proc `$`*[T](heap: DaryHeap[T]): string =
  ## Turn a heap into its string representation.
  runnableExamples:
    type Job = ref object
      priority*: int
      index*: int
    proc `<`*(a, b: Job): bool = a.priority < b.priority
    proc `index=`*(x: var Job, value: int) {.inline.} = x.index = value
    proc `$`*(x: Job): string = $x.priority
    var jobs = initDaryHeap[Job](4)
    jobs.pushit(Job(priority: 1))
    jobs.pushit(Job(priority: 2))
    assert $jobs == "[1, 2]"
  result = "["
  for x in heap.data:
    if result.len > 1: result.add(", ")
    result.addQuoted(x)
  result.add("]")
*/

class Job:
    priority: int
    index: int
    value: string

def getHeapIndex(x: Job) -> int:
    return x.index

def setHeapIndex(x: Job, i: int):
    x.index = i

def cmpHeap(x: Job, y: Job) -> int:
    return x.priority < y.priority

def test1():
    class Jobs = DaryHeap<Job>
    //let jobs = initDaryHeap<Job>(4, Job { priority: 1, index: -1, value: "one" })
    //let jobs = initDaryHeap(4, Job { priority: 1, index: -1, value: "one" })
    //let jobs: DaryHeap<Job> = DaryHeap { d: 4, data: []::Job }
    let jobs: DaryHeap<Job> = DaryHeap { d: 4, data: [Job { priority: 1, index: -1, value: "one" }] }
    jobs.pushit(Job { priority: 2, index: -1, value: "two" })
    print popit(jobs).value
    print popit(jobs).value
    print jobs.len

test1()
