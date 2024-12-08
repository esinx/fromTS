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

const z: { a: number } = x;
const z2: { a: number, b: string, c: number } = x; // Error

const func1 = (x: number): number | string => x;
const func2 = (x: number | string): number => 1;

type test = typeof func2 extends typeof func1 ? 1 : 2; // 1

declare const union: boolean | number;
const union2: boolean | number | undefined = union; // Error






