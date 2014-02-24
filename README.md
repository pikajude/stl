# Basic concepts

Everything of type `a` is a future.

Add a suffix exclamation mark `!` to expressions to force their results.

`main` has type `void!`. `void` is a builtin.

# Syntax

Strings are enclosed with double quotes.

```
mystr = "She said, 'Oh, that's handy.'"
```

Supported escape sequences are `\n`, `\r`, `\t`, and `\e`.

# Control flow

### Recursion

Referential transparency makes futures a lot easier to deal with. Thus we don't have mutable state in this language.

```
foo 1 = print "got to 1!"
foo n = {
  print "still not at 1..."
  foo (n - 1)
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

### Pattern matching

```
foo : integer -> string
foo = choose {
  1 -> "number 1!"
  x if x % 2 == 0 -> "even number!"
  _ -> "i don't know!"
}
```
