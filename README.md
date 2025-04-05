<img align="left" width="0px" height="18px"/>
<img src="https://user-images.githubusercontent.com/11986158/145484796-9bf1f77f-e706-4e15-b46b-c9b949f0086c.png" align="left" width="100px" height="100px"/>

<h3>REDscript</h3>

REDscript is a statically-typed, object-oriented programming language designed for developing scripts for the REDengine, CD Projekt's game engine used in Cyberpunk 2077. If you want to learn more about the language, visit the [official wiki](https://wiki.redmodding.org/redscript).

## Installation

To compile your scripts automatically on game startup, follow these steps:

- Download the `redscript-mod-{version}.zip` archive from the [latest release](https://github.com/jac3km4/redscript/releases/latest).
- Extract it into the main `Cyberpunk 2077` directory.
- When you start the game, successful setup can be confirmed with logs appearing in `Cyberpunk 2077/r6/logs/redscript_rCURRENT.log`.

## Editor Support

REDscript provides extensive editor tooling, including a language server and a debugger that integrate with many popular editors such as VSCode, neovim, IntelliJ IDEA, Zed, and Helix. For detailed setup instructions, check out the [REDscript IDE repository](https://github.com/jac3km4/redscript-ide).

## Project Structure

This project includes various tools for working with REDscript, such as a compiler, a decompiler, and a formatter. These are organized as modular Rust crates located in the `crates` directory:

- **`cli`** - Exposes a command-line interface serving as the entry point for all tools in this project.
- **`compiler/frontend`** - Performs static analysis on REDscript syntax, lowering it to an intermediate representation.
- **`compiler/backend`** - Generates bytecode from the intermediate representation and handles monomorphization for generic functions and types.
- **`compiler/api`** - Provides a high-level API for interacting seamlessly with the compiler.
- **`decompiler`** - Decompiles bytecode back into REDscript source code.
- **`io`** - Reads and writes binary files in the native REDengine format.
- **`scc/cli`** - Exposes a CLI that acts as a drop-in replacement for the original compiler made by CD Projekt.
- **`scc/capi`** - Exposes a C API with an interface similar to the `scc` CLI, but with some additional capabilities like source mapping.
- **`scc/shared`** - Contains common code used across the scc C API and CLI.
- **`syntax/ast`** - Defines the Abstract Syntax Tree (AST) for REDscript.
- **`syntax/formatter`** - Formats REDscript source code according to configuration.
- **`syntax/parser`** - Parses REDscript source code into the AST.
- **`dotfile`** - Parses the configuration `.redscript` file.

### Component Diagram
<img width="100%" height="750px" src="https://mermaid.ink/svg/pako:eNqtlFGPmkAQx78Kmb5wicciIgpNmiiI-tCk6fl04MMKq1KBJbtLojV-9y5LtXfN0TZeH0iWYf7_38yyO2dIaErAgx3D1V5bBXGpaZMoBl-GNX1FjuIhhrX2-PhJm-rRF8w4YeuHj03aVEX9aPK0WquArwKBruRFleWEaSGjpSBlKk1aVaCSZtHyayuaqffwlWiKk8NLTahy5tH0JEhTb6ucq-hCjwIZbJU_8xdtZS-KWupRSFmBhbglLdWHiVo3T5JnyUE2FcNeiIp7CO0ysa83hjRH33AyOBQ2YiTlCcsqgQQjBPUN0ziihGFBOOKnUuAjqtQexSCNnmjNEhJDY8_FKSfSPqE5Zd4H03Td7fYX1383F3PRAfW7oMH90OuOo-3tB7_JDrrYs39lb3K6-QsbcZagjBmMd1Qx66oi_A87sLme1jfRYRd6fv8GZLTtuOTiD03Pu8iL-5tOb1etA7rogi7ffcC31wvcgV7-joaeHGtZCt4W55z0oCDSoHmHs7r0IPakkBaeXKaYHRqzixRVuHymtABPsFrKGK13-5tJXaWyqCDDcmIWtyiTB4Awn9alAG8wspQJeGc4gmePjb5jjhzHMftDxxr24ASeMzYcyxpZjm2btuU67qUH3xXUNMa2PRi5tpT0zfHQGvSApJmg7HM7qtXEvvwAQCLa_g"/>
