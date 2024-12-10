const num = 42;
const str = "literal-string";
const boolTrue = true;
const boolFalse = false;
const constObj = { key: "value" } as const;
const obj = { key: "value" };
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

const decimal = 42;
const decimalFloat = 42.42;

const binary = 0b101010;
const octal = 0o52;
const hexadecimal = 0x2a;

const scientific = 4.2e1;
const scientificNegative = 4.2e-1;

const infinity = Infinity;
const negativeInfinity = -Infinity;
const nan = NaN;

console.log(
    decimal,
    decimalFloat,
    binary,
    octal,
    hexadecimal,
    scientific,
    scientificNegative,
    infinity,
    negativeInfinity,
    nan
);

console.log(typeof nan);

console.log(5 === 5.0);

type Test1 = {
    test: [5.0, 3] & ["test", 4.0[], string];
};

interface Test2 {
    test: 5;
}

const test1: Test2 = { test: 5 };

console.log(typeof test1.test);

console.log(Infinity === Infinity);

for (5 - 4; ; ) {
    console.log(test1);
}
