## Variables, Literals

`fromTS` supports the following variable types:

### `as const` Keyword

`fromTS` does not support the `as const` keyword.

## Binary Operators

### Arithmetic (`+`, `-`, `*`, `/`, `%`)

`fromTS` supports the following arithmetic operators:

```typescript
const a = 10;
const b = 20;
const c = a + b;
const d = a - b;
const e = a * b;
const f = a / b;
const g = a % b;
```

Along with assignment operators:

```typescript
let a = 10;
a += 10;
a -= 10;
a *= 10;
a /= 10;
a %= 10;
```

Arithmetic operators on strings are supported, and illegal operations will result in a type error as expected.

### Boolean (`&&`, `||`, `!`)

`fromTS` supports the following boolean operators:

```typescript
const a = true;
const b = false;
const c = a && b;
const d = a || b;
const e = !a;
```

Along with assignment operators:

```typescript
let a = true;
a &&= false;
a ||= true;
```

`fromTS` will evaluate the truthiness of the operands and return the result of the operation (short-circuit) as expected.

### Equality

#### Comparisons

`fromTS` will consider equality between two variables of different types to be false. This is because TypeScript does not allow comparisons between variables of different types.

#### `==` or `===` with `{}`

`fromTS` does not support the `==` and `===` operators with `{}`. This is because `fromTS` considers `{}` as the type `Object`, which is the superclass of all objects in TypeScript. TypeScript does not allow the `==` or `===` operators to be used with other variables, such as numbers or strings. However, `fromTS` allows this behavior.

```typescript
const obj = {};
const num = 10;
const str = "Hello, World!";
const b1 = obj == num; // type error, but fromTS allows this
const b2 = obj == str; // type error, but fromTS allows this
```

## Generics

`fromTS` does not support generics outside of standard array notation:

```typescript
const arr: number[] = [1, 2, 3];
const arr2: string[] = ["Hello", "World"];
```

## Functions

`fromTS` does not support functions.

## Other Non-Supported Features

-   Classes
-   Type extensions
-   Comma binary operator
-   "as const"
-   BinInt (not supported lower than ES2020 anyway)
-   Template strings and template literals
-   Support nested strings (in templates)
-   Escape characters in strings
-   Destructuring
-   Async/await and promises
-   Support for "throw"
-   Parametric types
-   Ternary operator
-   Default function parameters (and ... rest parameters)
-   Unnamed call signatures in user types and interfaces
-   Finish "TODOs"
-   Limited built-in type support (only has `Array.prototype.length` for now)
-   Types with arbitrary string keys. E.g:

```typescript
const test123: {
    [key: string]: string;
} = {
    test: "test",
};
```
