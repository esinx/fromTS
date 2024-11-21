const variable = 0
let uncertain: string | number = Math.random() > 0.5 ? "string" : 0

uncertain
// ^??

if (variable == 0) {
	const a = 1
}
if (variable == 0) {
	const a = 2
} else {
	const b = 3
}
