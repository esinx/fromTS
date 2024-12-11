
## Equality

### `==` or `===` with `{}`

`fromTS` does not support the `==` and `===` operators with `{}`. This is because `fromTS` considers `{}` as the type `Object`, which is the superclass of all objects in TypeScript. TypeScript does not allow the `==` or `===` operators to be used with other variables, such as numbers or strings. However, `fromTS` allows this behavior.

```typescript
const obj = {};
const num = 10;
const str = "Hello, World!";
const b1 = obj == num; // type error, but fromTS allows this
const b2 = obj == str; // type error, but fromTS allows this
```