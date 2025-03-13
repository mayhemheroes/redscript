<img align="left" width="0px" height="18px"/>
<img src="https://user-images.githubusercontent.com/11986158/145484796-9bf1f77f-e706-4e15-b46b-c9b949f0086c.png" align="left" width="100px" height="100px"/>

<h3>REDscript</h3>

REDscript is a statically-typed, object-oriented programming language designed for developing scripts for the REDengine, CD Projekt RED’s game engine used in Cyberpunk 2077. If you want to learn more about the language, visit the [official wiki](https://wiki.redmodding.org/redscript).

## Installation

To compile your scripts automatically on game startup, follow these steps:

- Download the `redscript-mod-{version}.zip` archive from the [latest release](https://github.com/jac3km4/redscript/releases).
- Extract it into the main `Cyberpunk 2077` directory.
- When you start the game, successful setup can be confirmed with logs appearing in `Cyberpunk 2077/r6/logs/redscript_rCURRENT.log`.

## Editor Support

REDscript provides extensive editor tooling, including a language server and debugger that integrate with many popular editors such as VSCode, neovim, IntelliJ IDEA, Zed, and Helix. For detailed setup instructions, check out the [REDscript IDE repository](https://github.com/jac3km4/redscript-ide).

## Project Structure

This project includes various tools for working with REDscript, such as a compiler, a decompiler, and a formatter. These are organized as modular Rust crates located in the `crates` directory:

- **`cli`** - A command-line interface serving as the entry point for all tools in this project.
- **`compiler/frontend`** - Parses REDscript source code and performs static analysis, lowering the syntax to an intermediate representation.
- **`compiler/backend`** - Generates bytecode from the intermediate representation and handles monomorphization for generic functions and types.
- **`compiler/api`** - Provides a high-level API for interacting seamlessly with the compiler.
- **`io`** - A module dedicated to reading and writing the binary files following the REDengine format.
- **`scc/cli`** - A CLI that acts as a drop-in replacement for the original CD Projekt RED's REDscript compiler.
- **`scc/capi`** - A C API with an interface similar to the `scc` CLI, but with some additional capabilities like source mapping.
- **`scc/shared`** - Common code used across the scc C API and CLI.
- **`syntax/ast`** - Defines the Abstract Syntax Tree (AST) for REDscript.
- **`syntax/formatter`** - A code formatter for REDscript.
- **`syntax/parser`** - A parser dedicated to processing REDscript code.
