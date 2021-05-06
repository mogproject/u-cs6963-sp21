

## How to create a new Haskell project

- Install Stack

With Homebrew on Mac.

```
brew install stack
stack upgrade
```

- Create project directory

```
stack new cluster-editing
cd cluster-editing
```

- Update files: see https://github.com/mogproject/santorini-bitboard-example
  - `.gitignore`
  - `package.yaml`

- Install dependencies

```
stack setup
```

- Build and run the executable

```
stack build
stack exec exact
```
