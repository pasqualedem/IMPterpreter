# IMPterpreter

**In the executable there are some bugs in the shell**



## Introduction

An interpreter is a program that directly analyse and execute instructions written in a certain programming language. In this project was built a simple interpreter in Haskell based on modified strategy given in the book *Programming in Haskell – Second Edition" by Graham Hutton*. IMPterpreter works on a simple imperative programming language called IMP.

The basic constructs of IMP are:

 - Skip: does nothing
 - Assignment: evaluate an expression and assign it to a variable
 - If then else: Performs the conditional selection between two paths
 - While: Loop a set of instructions according to a boolean condition

IMPterpreter uses an eager evaluation (call-by-value), so in functions calls like g(f(x)) is evaluated first g and subsequently f. 

## Usage

Load the **Main.hs** module from an haskell shell,  and then type `main` to launch IMPterpreter, this should be shown:

![image-20210103202322514](docs\imgs\main.png)

Instructions can be directly typed into the shell or can be loaded from file with the **:l** command. All commands available are:

- **:l filename** loads and executes instructions contained in the given file 
- **:env** prints all the variables in the environment
- **:p v** search the variable "v" in the environment: if is found prints its value, otherwise prints *VariableNotDefined "v"*
- **:cl**  empties the environment
- **:h** shows the help message
- **:q** quits from the interactive shell

### Code examples

Assigning variables

```pascal
x = 6.7;
y = False;
v = [1, False, 4.5];
```

Using selection and loop

```pascal
b = True;
if b then
	x = 1;
else
	x = -1;
end
```

```pascal
n = 0;
i = 0;
while i<n do
	i = i + 1;
end
```

Factorial of 6

```pascal
f = 1;
n = 6;
i = 1;
while i < n + 1 do
    f = f * i;
    i = i + 1;
end
```

Average value of an array

```pascal
v = [2, 3, 6, 2, 3, 4];
n = 6;
i = 0;
s = 0;
while  i < n do
    s = s + v[i];
    i = i + 1;
end
m = s / n;
```

Other examples can be found in the *examples* folder.

```
:l ../examples/matrixProd.imp
:env
```

```
:l ../examples/pow.imp
:env
```

