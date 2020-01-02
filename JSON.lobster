
// Lobster has Google Flatbuffers built-in
// Flatbuffers uses IDL syntax for schema defintion, and static typing

let fbschema = """
table tt {
    name: string;
    age:  long;
    wage: float;
}
root_type tt;
"""

let jsonstr = """
{ name: "Joe", age: 91, wage: 1.98 }
"""

let blob, err1 = flatbuffers_json_to_binary(fbschema, jsonstr, [])

assert err1 == nil

let str, err2 = flatbuffers_binary_to_json(fbschema, blob, [])

assert err2 == nil

print str

//print flatbuffers_field_string(blob, 20, 1)

//assert flatbuffers_field_string(blob, 1, 1) == "Joe"
