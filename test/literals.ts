const num = 42;
const str = "literal-string";
const boolTrue: false = true;
const boolFalse: boolean = false;
const constObj: [number, boolean] = { key: "value" };
const obj = { key: "value", key2: 42 };
const arrOfNum = [1, 2, 3];
const arrOfStr = ["a", "b", "c"];
const arrOfBool = [true, false, true];
const arrOfObj = [{ key: "value" }, { key: "value" }];
const arrOfArr = [
    [1, 2, 3],
    ["a", "b", "c"],
];
const arrOfMixed = [1, "a", true, { key: "value" }, [1, 2, 3]];
const nullLiteral = null;
// numbers
const decimal: number = 42; // Standard integer
const decimalFloat = -42.42; // Floating-point

const binary = 0b101010; // 42 in binary
const octal: string = -0o52; // 42 in octal
const hexadecimal = 0x2a; // 42 in hexadecimal

// NOT SUPPORTING BIGINT: (not available when targeting lower than ES2020)
// const bigInt = 42n; // BigInt literal
// const bigIntHex = 0x2an; // BigInt in hexadecimal
// const bigIntBinary = 0b101010n; // BigInt in binary
// const bigIntOctal = 0o52n; // BigInt in octal

const scientific = (((-4.2e1 * 100 ** 2) << 4) / 4.2) | 0b1011101;
const scientificNegative = 4.2e-1; // 0.42

const infinity = Infinity; // Positive infinity
const negativeInfinity = -Infinity; // Negative infinity
const nan = NaN; // Not a Number

// console.log(
//     decimal,
//     decimalFloat,
//     binary,
//     octal,
//     hexadecimal,
//     scientific,
//     scientificNegative,
//     infinity,
//     negativeInfinity,
//     nan
// );

// console.log(typeof nan);

// console.log(NaN === NaN); // false? but it's an error

// console.log(5 === 5.0);

type Test1 = {
    test: [5.0, 3] & ["test", 4.0[], string];
};

interface Test2 {
    test: 5;
}

const test1: Test2 = { test: 5 };

// console.log(typeof test1.test); // number

// console.log(Infinity === Infinity);

// console.log(5 / 0);

for (let x = 0; x < 10; x++) i = x;

if (true) {
    let val = "5 == 6";
} else if (5 - 7 < 21) val2 = true;

else {
    test1 = true;
}

if (false) {
    let val = "5 != 6";
} else if (5 - 7 > 21) val3 = false;

if (5 == 6) {
    let val = "5 != 6";
} else {
    let val = "5 == 6";
}

while (true) {
    let val = "5 == 6";
}

try {
    5 + 6;
} catch (a: any) {
    const names = ["a", "b", "c"];
}

try {
    5 + 6;
} catch {
    const names = ["a", "b", "c"];
} finally {
    let val = 7;
}

try {
    5 + 6;
} finally {
    let val = 7;
}

// if (true) while (false) for (;;) console.log(":(");

// const fn = () => {
//     console.log("test");
// };

// for (fn(); fn(); ) {}

// const test123: {
//     [key: string]: string;
// } = {
//     test: "test",
// };

type MyType = number | string | boolean;

const test: MyType = 5;

type MyType2 = {
    "test field": 5
    field1: number;
    field2?: string;
    field3: boolean | MyType,
    field4: (number | (MyType2 | MyType))[];
};

interface MyType3 {
    field1: number;
    field2: string,
    field3?: boolean | MyType
    field4?: number | (MyType2 | MyType)[];
}
