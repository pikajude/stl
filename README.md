# Basic concepts

### Language design

Everything without an exclamation mark is a future. By default, this is everything. Add a suffix exclamation mark `!` to expressions to force their results.

`main` has type `void!`. `void` is a builtin, and `!` indicates that the expression result is required. This just means that the "main" function must return (or loop forever). This is required by the RTS.

# Syntax

### Strings

Strings are enclosed with double quotes.

```
mystr = "She said, 'Oh, that's handy.'"
```

Supported escape sequences are `\n`, `\r`, `\t`, and `\e`. stl doesn't have single-quoted strings.

### Numbers

`0`, `100_000`, `0xdeadbeef`, and `0o777` are `integer`s. Underscores within numeric literals are optional and just there to make reading literals easier.

`1.0f`, `0x3200.16f` are `float`s. In stl, floats are doubles. You can use some primitives from the `stl.prim` library if you care about sizes.

`3n`, `0x200n` and `0o177n` are `natural`s. `-1n` is a syntax error.

### Function application

Function application looks like this: `f(a, b, c)`. Functions in stl are first-class citizens.

Within braces `{}`, you can use semicolons or newlines to separate expressions.

```
foo(x) = {
  print("Hello, world!")
  x
}
```

```
bar(x) = { print("Hello, world!"); x }
```

### Function definition

Function definition looks like function application, but with a `=`.

```
f(a, b) = "Hello, #{a}! Welcome to #{b}!"

f("dear user", "stl")
```

Literals and data constructors on the LHS of a function definition will be matched on.

```
f(1) = print("Finished!")
f(n) = { print("Still going to 1…"); f(n - 1) }
```

### Datatypes

stl has algebraic data types. These are introduced with the `type` keyword.

```
type Option(x) = Some(x) | None

type List(a) = Nil | Cons(a, List(a))
```

### Pattern matching

stl supports pattern matching because not doing that is super lame. An example is shown in the last code block. You can also match on parts of constructors:

```
# :: is the cons operator
map(_, []) = []
map(f, x::xs) = f(x) :: map(f, xs)
```

including user-defined ones:

```
fromOption Option(x) = x
fromOption None = panic! "fromOption on None"
```

### Lambdas

```
power2s : [integer]
power2s = map (x -> 2 ** x, [1..10])
```

# Control flow

### Loops

Referential transparency makes futures a lot easier to deal with. Thus we don't have mutable state in this language. Use recursion instead.

```
foo(1) = print "got to 1!"
foo(n) = {
  print "still not at 1..."
  foo(n - 1)
}
```

Runtime should implement TCO so this won't be much of a performance hit.

### Conditionals

```
if true "foo" else "bar"
```

```
if true {
  foo
  bar
  "baz"
} else "bar"
```

### Pointfree pattern matching

```
foo : integer -> string
foo = choose {
  1 -> "number 1!"
  x if x % 2 == 0 -> "even number!"
  _ -> "i don't know!"
}
```
