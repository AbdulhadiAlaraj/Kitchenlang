# KitchenLang

## Introduction

**KitchenLang** is a domain-specific language (DSL) developed in Racket, designed to simulate kitchen operations such as chopping, mixing, and whisking in a programming context. This language allows users to perform arithmetic and logical operations through a culinary-themed metaphor, handling variables and expressions in a fun and intuitive way.

## Installation

To get started with KitchenLang, you must first install Racket, which can be downloaded from the Racket website:

- Download Racket from [https://racket-lang.org](https://racket-lang.org).

After installing Racket, you can run KitchenLang scripts either through the DrRacket IDE or VSCode.

## Writing KitchenLang Scripts

KitchenLang scripts are composed of commands that resemble kitchen operations. Here are the basic constructs and syntax rules you need to know:

### Variables and Assignments

- **Variables:** Use the `pour` command to assign values to variables.
  - Example: `pour x 10` assigns the value `10` to the variable `x`.
  - You can also assign the result of an expression: `pour y mix x x` which assigns the result of `mix x x` to `y`.

### Operations

- **Chop:** Halves the value of a variable or a number.
  - Syntax: `chop x` where `x` can be a number or a variable.
- **Mix:** Adds two values.
  - Syntax: `mix x y` adds the values of `x` and `y`.
- **Peel:** Decreases the value by 1.
  - Syntax: `peel x` decrements `x` by 1.
- **Whisk:** Multiplies two values.
  - Syntax: `whisk x y` multiplies `x` by `y`.

### Conditional Statements

- **Taste Conditions:** Compare values and perform operations based on comparisons.
  - `taste-eq x y do-this else-do-this`: Executes `do-this` if `x` equals `y`, otherwise executes `else-do-this`.
  - `taste-gt` and `taste-lt` work similarly for greater than and less than comparisons.

### Loops

- **Bake:** Repeats an operation a specified number of times.
  - Syntax: `bake mix x y 3` which will execute `mix x y` three times.

## Running KitchenLang Scripts

To run a KitchenLang script, you need to use the `kitchen-code-evaluate` function provided within your Racket script. Here's how to execute a file named `example.kcl`:

```racket
(kitchen-code-evaluate "example.kcl")
```
This function will parse and execute the commands in the file, displaying the results of each operation.

### Example Script

Here is a sample script that demonstrates various features of KitchenLang (the semicolons are there for this readme and are not part of the language yet):
```racket
pour x 20
pour y 5

; Perform operations
chop x
mix x y
peel y
whisk x y

; Conditional operation
taste-eq x y chop x peel y

; Looping operation
bake mix x y 2
```
## Debugging and Output

Each operation in the script prints the outcome to the console, helping you trace and debug the flow of operations and their results.

## License

This project is licensed under the MIT License.
