const a = 1
const b = 2
const c = 3
let x = 1,
	y = 2,
	z

z = x + y

const sum = (a: number, b: number) => a + b

function foo() {
	return sum(x, y)
}

console.log([a, b, c, x, y, z, foo()])
console.log([a, b, c, x, y, z, foo()].reduce((acc, val) => acc + val, 0))

interface Animal {
	name: string
	age: number
}

interface Dog extends Animal {
	bark(): void
}

interface Cat extends Animal {
	meow(): void
}
