declare const any: any;
const never: never = any; // Error
declare const never2: never
const any2: any = never // Ok
declare const unknown: unknown;
const unknown1: any = unknown; // Ok
const unknown2: unknown = any; // Ok
const unknown3: unknown = never; // Ok

type x = undefined extends void ? 1 : 2; // 1

type y = null extends void ? 1 : 2; // 2
type z = null extends undefined ? 1 : 2; // 2

declare const a: boolean | number;
const a2: boolean | number | undefined = a; // Ok
const a3: boolean & number = a; // Error

declare const b: boolean & number;
const b2: boolean = b; // Ok

const tuple: [number, string] = [1, "hi"];
const array: (number | string)[] = tuple;

const x = { a: 1, b: "hi" };
const y: object = x;
const y2: { a: number } = x; // Ok
const y3: {} = x; // Ok
const y4: { c: 2 } = x; // Error

const z: { a: number } = x;
const z2: { a: number, b: string, c: number } = x; // Error

const arr_to_object: { a: number } = [1]; // Error

const func1 = (x: number, y: string, z: undefined): number | string => x;
const func2 = (x: number, y2: number | string): number => 1;

type test = typeof func2 extends typeof func1 ? 1 : 2; // 1

declare const union: boolean | number;
const union2: boolean | number | undefined = union; // Ok

enum Role {
    Admin = "ADMIN",
    User = "USER",
    Guest = "GUEST"
}

enum Role2 {
    Admin = "ADMIN",
    User = "USER",
}

enum Role3 {
    Admin = "ADMIN",
    User = "USER",
    Guest = "GUEST"
}

const role: Role = Role2; // Error
const role2: Role = Role3; // Error


const person = {
    name: "John",
    age: 30
}

const person2: {
    name: string
} = person; // Ok

const str: string = Role.Admin; // Ok

const q: unknown = Role;

const assign: {} | null | undefined = unknown; // Ok

const check1: number = union; // Error
const check2: unknown = union; // Ok


// rough work

const arr2 = [1, "hi", 3];
const index1 = arr2[1]; // number | string

const tuple2: [number, string] = [1, "hi"];
const num = 1 + 1

const index2 = tuple2[any2]; // string | number
const index3 = tuple2[2];
const index4 = tuple2[1]; // string

const xIndex = x[0]; // any

const newObj = {};

const newObjIndex = newObj[0]; // any

const obj = {
    a: 1,
    b: "hi"
}

const rough2 = num.c;

const objOfObj = {
    a: obj
}

const rough = num[0];

// typeCheckVar

const userObj = {
    name: "John",
    age: 30
}

const n = userObj.name; // string
const objectIndex = {}.name;
const n2 = userObj.bad;

const arr = [1, "hi", 3];
const arrIndex = arr["hi"]; // any



const nonObj = arr.hi;

const arrIndex1 = arr[1];

const arrIndex2 = arr["hi"]; // number | string

const tupleee: [number, string] = [1, "hi"];
const tupleIndex = tupleee[1]; // string
const tupleIndex2 = 1["hi"]; // number | string

let arrOut = [1, 2, 3];
arrOut["hi"] = 4; // Ok???

let check = -arrOut

var something: string | number = 1

function testFunc() {
    return something = "hi";
}

1 == { a: 1 } // Error
1 == 1 + 1

{ a: 1 } == { a: 1 } // Error TODO: why

const obj1 = { a: 1 };
const obj2 = { a: 1 };
const obj3 = { a: 1, b: 2 };
obj1 == obj2
obj2 == obj3

const whattt: {} = {};
whattt == {}

const whatttttt = 1
whatttttt == {}

1 == {}

whatttttt == whattt

const wut = undefined;
const wut2 = null;

wut == wut2

1 == [1; 2; 3]

1 == "hi"

1 == 1

const objecttt = {}

const objectttt: object = objecttt

const varrrr = 1
const varrrrr = [1, 2, 3]
varrrr == varrrrr


// if () {
//     console.log("hi")
// }

// let x = if (true) { return 1 } else { return 2 };

try {
    console.log("hi")
} catch (e) {
    console.log("hi")
}

switch (1) {
    case 1:
        console.log("hi")
        break;
    default:
        console.log("hi")
}

if (1) {
    let x = 3;
}