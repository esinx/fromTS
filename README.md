# fromTS

> A TypeScript typechecker & transpiler written in Haskell

## Team

- [Eunsoo Shin](https://github.com/esinx) (esinx)
- [Jordan Hochman](https://github.com/JHawk0224) (jhawkman)
- [Katrina Liu](https://github.com/liukatkat) (katltn)

## Structure

```
├── README.md
├── app
│   └── Main.hs -- Main entry point for the program
├── docs
│   └── specs.md -- Specifications for the project, definitions of supported features
├── fromTS.cabal
├── hie.yaml
├── model -- TypeScript AST model for model based testing
│   ├── index.ts
|   ...
├── src
│   ├── Model.hs -- Model based testing utilities
│   ├── TSError.hs -- Error types
│   ├── TSNumber.hs -- Number type utilities
│   ├── TSParser.hs -- Parser for TypeScript
│   ├── TSSyntax.hs -- TypeScript AST types
│   ├── TSType.hs -- Type data types
│   ├── TSTypeChecker.hs -- Type checker
│   └── fromTS.hs -- Main functions for the program
├── stack.yaml
├── stack.yaml.lock
└── test
    ├── Spec.hs -- Test suite
    ...
```

## Usage

```bash
stack run <file.ts>
```

To run tests, use:
```bash
cd model; yarn install; yarn build; cd ..
stack test
```

## Features

See [docs/specs.md](docs/specs.md) for a list of supported features.

## Libraries used

- `pretty` for pretty printing
- `megaparsec` for parser combinators,
- `aeson` for JSON parsing
- `safe` for safe indexing