# Orilla

Orilla is a toy programming language using LLVM as the backend I am developing to learn!

## Current features:
- Variables
- Printing to the console (through printf from libc)
- If and else statements
- Scopes (with variable shadowing)

## Requirements:
- LLVM
- CMake (for the build system)
- A C++ compiler (for the project)

## Usage:
- Build the project with CMake
- Compile a file as follows:
```
orilla-compiler <filename> [-o output]
```
- The output will be an executable file (for the host's architecture) in the current directory or the one specified with the -o flag.

## Current syntax and grammar:

### File extension:
The file extension for Orilla files is `.ola`.

### Variables:
Variables can be declared with the let keyword. They are typed strongly.
Type annotations are done with a colon after the variable name and the type keyword before the value.
If no type annotation is provided, the type will be inferred from the value.
```
let int: Int = 10;
let float: Float = 10.5;
let double: Double = 10.5;
let string: String = "Hello, World!";
let bool: Bool = true;
```

### Printing:
Printing to the console (printf from libc under the hood):
```
let x: Int = 10;
print("The value of x is: %d\n", x);
```

### If statements:
```
if (100 > 0) {
    print("100 is indeed greater than 0\n");
} else {
    print("100 is not greater than 0, somehow\n");
}
```

If statements as expressions:
```
if (10 > 10.5) print("10 is greater than 10.5\n") else print("10 is less than 10.5\n");
```

### Exit statement:
Exits the program with a return value. If it is not declared, the program will return 0 when it finishes.
```
exit(10);
```

### Scopes and variable shadowing:
Variables declared inside a scope are not visible outside of it and they can shadow variables with the same name declared outside of it.
```
let x: Int = 10;
{
    let x: Int = 50;
    print("The value of x is: %d\n", x); // This will print 50
}
print("The value of x is: %d\n", x); // This will print 10
```

### Comments:
Single line comments are supported and they are ignored by the compiler.
```
// This is a single line comment
```

