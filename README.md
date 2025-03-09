# Flora

Flora is a minimal dynamically-typed stack-based concatenative programming language.

## Installation

To install Flora, clone the repository and build the interpreter. You'll need a Rust compiler.

```bash
git clone https://github.com/japi012/flora.git
cd flora
cargo build --release
```

Start the REPL with:

```
cargo run
```

And execute a file by doing:

```
cargo run -- <filename>
```

## Examples

### Comments

```
(This is a comment.)
(Comments are surrounded in parentheses.)
```

### `Hello world!`

```
"Hello, world!" puts
```

### Arithmetic

```
3 4 + . (=> 7)
5 3 - . (=> 2)
10 2 * . (=> 20)
10 2 / . (=> 5)
```

### Quotations

```
"std/prelude.flora" load (`load` can be used to load a file.)

(A quotation is a block of code surrounded in square brackets.)
(They can be executed with `call`:)

[1 2 +] call . (=> 3)

(They can be used for conditionals with `if`:)

5 10 > ["Greater"] ["Smaller"] if puts (=> Smaller)

(They can be used for regular lists.)

[1 2 3] [4 5 6] compose . (=> [1 2 3 4 5 6])

(They work well with a lot of standard library functions.)

[1 2 3 4] [dup *] map . (=> [1 4 9 16])
1 100 range . (=> [1 2 3 4 ... 98 99 100])
1 100 range [2 % 0 =] filter . (=> [2 4 6 8 ... 96 98 100])
```
