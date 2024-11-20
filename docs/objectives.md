# fromTS Objectives

> `fromTS` is a TypeScript transpiler that supports a subset of the TypeScript language.


## Variables

### Var
 
```typescript
var x: number = 10;
var str: string = "Hello, World!";
```

### Let and Const

```typescript
let x: number = 10;
const y: number = 20;
let str1: string = "Hello, World!";
const str2: string = "Goodbye, World!";
```

## Literals

> Literals are fixed values that you provide, and does not require type annotations.

### Null and Undefined

```typescript
let x = null;
let y = undefined;
```

### Number, String, and Boolean Literals

```typescript
let x = 10;
let str = "Hello, World!";
let bool = true;
```

### Object Literals

```typescript
let obj = {
    x: 10,
    y: 20
    nested: {
        a: 30,
        b: 40
    }
};
```

### Array Literals

```typescript
let arr = [10, 20, 30, 40];
let nestedArr = [[10, 20], [30, 40]];
```

## Accessors

### Dot Notation

```typescript
let obj = {
    x: 10,
    y: 20
};
console.log(obj.x); // 10
```

### Projection (Bracket) Notation
    
```typescript
let obj = {
    x: 10,
    y: 20
};
let arr = ['x', 'y'];
console.log(obj['x']); // 10
console.log(obj[arr[0]]); // 10
```

## Functions

### Functions

```typescript
function add(x: number, y: number): number {
    return x + y;
}
```

### Arrow Functions

```typescript
let add = (x: number, y: number): number => {
    return x + y;
};
```

## Loops

### For Loops

```typescript
for (let i = 0; i < 10; i++) {
    console.log(i);
}
```

### While Loops

```typescript
let i = 0;
while (i < 10) {
    console.log(i);
    i++;
}
```

## Unwrapping Optionals

### Optional Chaining

```typescript
let obj = {
    x: 10,
    y: 20
};
console.log(obj?.x); // 10
let nestedObj = {
    x: 10,
    y: {
        z: 20
    }
};
console.log(nestedObj.y?.z); // 20
console.log(nestedObj.nonExistent?.value); // undefined
```

### Forced Unwrapping

```typescript
type Obj = {
    x: number,
    y?: number
};
let obj: Obj = {
    x: 10,
    y: 20
};
console.log(obj!.x); // 10
console.log(obj.y.toFixed(2)); // Type check fails, y is optional
console.log(obj.y!.toFixed(2)); // 20.00
```

## Type Casting

### Type Assertion

```typescript
let x: any = "Hello, World!";
let str: string = x as string;
```


