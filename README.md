# Spyglys
Spyglys is a simple interpreted string manipulation language originally designed for [sakinyje](github.com/brewingweasel/sakinyje).
In general, Spyglys is meant to be embedded in larger programs.

## Basic tutorial

### General information
Spyglys includes an interactive shell which can be reached through the `spyglys shell` command. 
This command takes an extra file argument which can be used to import all of the contents of the file into the scope.

Strings are inside of double quotes ("")
Regex patterns are inside of single quotes ('')

In Spyglys, functions are used to handle all string manipulation.
In order to maintain simplicity, functions always only have one argument, and one expression to modify that argument.
The argument always has the type of regex, though this can be built with expressions inside the definition.
The expression that handles the modification always has the type of string.
Functions are defined with the `def` keyword, and the regex pattern they match on is placed between parenthesis.
```ruby
def trim ('^\s*(?<main>\S(.*\S)?)\s*$'):
    main
end
```

Functions are called with the syntax you would expect.
```ruby
> trim("   hi   ")
=> "hi"
```

Variables can be used to share definitions across multiple functions.
```ruby
let whitespace = '\s*';
let main = '(?<main>\S(.*\S)?)';

def trim ('^' + whitespace + main + whitespace + '$'):
    main
end

def trim_left ('^' + whitespace + main):
    main
end

def trim_right (main + whitespace + '$'):
    main
end
```

They can also be strings used inside of the functions
```ruby
let response = " sounds delicious";
def food_response ('I want some (?<food>.*)'):
    food + response
end
```

When functions do not match a pattern, they will return `EMPTY`, which is represented as ().
This usually serves as the equivalent of an empty string.
```ruby
> let response = food_response("I want to eat pasta");
> response
=> ()
> response + " right now."
=> " right now."
```

Empty function responses can be handled with builtin functions (which use the $function() syntax), which can take multiple parameters.
For example, the previous function could be edited to include a default food.
```ruby
let response = " sounds delicious";
def food_response ('I want some (food|(?<food>.+))'):
    $unwrap_empty(food, "cake") + response
end
```

```ruby
> food_response("I want some pizza")
=> "pizza sounds delicious"
> food_response("I want some food")
=> "cake sounds delicious"
```

Functions can also be recursive. This function will naively recursively transform xml/html into its equivelant s-expression.
```ruby
def parse_html ('<(?<tag>\w+)>(?<inner>(?<inner_html><.+)|.+)?</(?<secondtag>\w+)>'):
    "(" + tag + $map(inner, " ") + $map(inner, $if_else(inner_html, parse_html(inner), inner)) + ")"
end
```

## TODO:
* docs
* more tests
* new language features
