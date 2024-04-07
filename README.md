# KitchenCode Programming Language

KitchenCode is a unique, esoteric programming language inspired by the art of cooking. It turns culinary actions and ingredients into programming constructs, creating a flavorful blend of coding and cooking. In KitchenCode, traditional programming concepts are expressed through cooking-related terms, allowing for a delightful and intuitive programming experience.

## Concept

KitchenCode marries the process of cooking with programming. Ingredients represent data, while cooking processes translate into commands that manipulate this data. For example, `chop` is used to divide data, `mix` combines variables, and `bake` executes loops, introducing a playful yet practical approach to coding.

## Implementation

The language is implemented using Racket. It leverages Racket's `parser-tools/lex` and `parser-tools/yacc` for lexical analysis and parsing, respectively. KitchenCode's lexer recognizes cooking-related terms as tokens, and its parser assigns functional meanings to these tokens, effectively translating "recipes" into executable programs.

### Lexer and Tokens

KitchenCode defines two sets of tokens: ingredients and actions. Ingredients are numeric values, while actions include `CHOP`, `MIX`, `BAKE`, `PEEL`, `WHISK`, `TASTE-EQ`, and `TASTE-GT`, each corresponding to a specific operation in the language.

### Parser and Grammar

The parser defines the grammar of KitchenCode, mapping sequences of tokens to expressions. These expressions represent the operations to be performed on the ingredients, supporting basic arithmetic and conditional execution based on comparisons.

### Evaluation Function

An evaluation function interprets the parsed expressions, executing the defined operations. This function supports arithmetic operations, conditional execution, and looping constructs, all within the culinary-themed syntax of KitchenCode.

## Usage

To run a KitchenCode program, write your code as a string that follows the language's syntax and pass it to the `kitchen-code-evaluate` function. This function parses and evaluates the code, executing the culinary-themed operations defined in your program.

### Example Programs

Below are some example KitchenCode programs and their outputs:

- `chop 10` divides the ingredient (10) by 2, resulting in 5.
- `mix 5 3` combines the ingredients (5 and 3), resulting in 8.
- `peel 5` subtracts 1 from the ingredient (5), resulting in 4.
- `whisk 3 4` multiplies the ingredients (3 and 4), resulting in 12.
- `bake chop 10 1` performs the `chop` operation on the ingredient (10) one time, resulting in 5.
- `bake peel 10 3` performs the `peel` operation on the ingredient (10) three times, resulting in 7.
- `bake mix 10 3 1` combines the ingredients (10 and 3) one time, resulting in 13.
- `bake whisk 10 2 1` multiplies the ingredients (10 and 2) one time, resulting in 20.
- `taste-eq chop 10 mix 5 5 whisk 3 4 mix 5 5` evaluates the equality of `chop 10` and `mix 5 5`, executing `whisk 3 4` if true, else `mix 5 5`.
- `taste-gt chop 20 5 peel 2 mix 1 1` evaluates if `chop 20` is greater than 5, executing `peel 2` if true, else `mix 1 1`.

KitchenCode offers a fresh and engaging way to learn programming concepts, bringing the creativity and experimentation of cooking into the world of code. Bon appétit!
