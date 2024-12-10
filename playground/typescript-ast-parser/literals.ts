const num = 42
const str = "literal-string"
const boolTrue = true
const boolFalse = false
const constObj = { key: "value" } as const
const obj = { key: "value" }
const arrOfNum = [1, 2, 3]
const arrOfStr = ["a", "b", "c"]
const arrOfBool = [true, false, true]
const arrOfObj = [{ key: "value" }, { key: "value" }]
const arrOfArr = [
	[1, 2, 3],
	["a", "b", "c"],
]
const arrOfMixed = [1, "a", true, { key: "value" }, [1, 2, 3]]
const nullLiteral = null
// numbers
const decimal = 42 // Standard integer
const decimalFloat = 42.42 // Floating-point

const binary = 0b101010 // 42 in binary
const octal = 0o52 // 42 in octal
const hexadecimal = 0x2a // 42 in hexadecimal

// NOT SUPPORTING BIGINT: (not available when targeting lower than ES2020)
// const bigInt = 42n; // BigInt literal
// const bigIntHex = 0x2an; // BigInt in hexadecimal
// const bigIntBinary = 0b101010n; // BigInt in binary
// const bigIntOctal = 0o52n; // BigInt in octal

const scientific = 4.2e1 // 42 in scientific notation
const scientificNegative = 4.2e-1 // 0.42

const infinity = Infinity // Positive infinity
const negativeInfinity = -Infinity // Negative infinity
const nan = NaN // Not a Number

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
)

console.log(typeof nan)

// console.log(NaN === NaN); // false? but it's an error

console.log(5 === 5.0)

interface Test1 {
	test: 5
}

interface Test2 {
	test: 5.0
}

const test1: Test2 = { test: 5 }

console.log(typeof test1.test) // number

console.log(Infinity === Infinity)
