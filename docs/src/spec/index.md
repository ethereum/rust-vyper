# Fe Language Specification

<div class="warning">
  Warning: This is a work in progress document. It is incomplete and specifications aren't stable yet.
</div>

# 1. Notation

## 1.1 Grammar

The following notations are used by the *Lexer* and *Syntax* grammar snippets:

| Notation          | Examples                      | Meaning                                   |
|-------------------|-------------------------------|-------------------------------------------|
| CAPITAL           | KW_IF                         | A token produced by the lexer             |
| _ItalicCamelCase_ | _Item_                        | A syntactical production                  |
| `string`          | `x`, `while`, `*`             | The exact character(s)                    |
| \\x               | \\n, \\r, \\t, \\0            | The character represented by this escape  |
| x<sup>?</sup>     | `pub`<sup>?</sup>             | An optional item                          |
| x<sup>\*</sup>    | _OuterAttribute_<sup>\*</sup> | 0 or more of x                            |
| x<sup>+</sup>     |  _MacroMatch_<sup>+</sup>     | 1 or more of x                            |
| x<sup>a..b</sup>  | HEX_DIGIT<sup>1..6</sup>      | a to b repetitions of x                   |
| \|                | `u8` \| `u16`, Block \| Item  | Either one or another                     |
| [ ]               | [`b` `B`]                     | Any of the characters listed              |
| [ - ]             | [`a`-`z`]                     | Any of the characters in the range        |
| ~[ ]              | ~[`b` `B`]                    | Any characters, except those listed       |
| ~`string`         | ~`\n`, ~`*/`                  | Any characters, except this sequence      |
| ( )               | (`,` _Parameter_)             | Groups items                              |



## 2. Lexical structure

### 2.1. Keywords

Fe divides keywords into two categories:

* [strict](#strict-keywords)
* [reserved](#reserved-keywords)

#### 2.1.1. Strict keywords

These keywords can only be used in their correct contexts. They cannot
be used as the names of:

* [Items]
* [Variables] and function parameters
* Fields and [variants]

> **<sup>Lexer:<sup>**\
> KW_AS             : `as`\
> KW_BREAK          : `break`\
> KW_CONST          : `const`\
> KW_CONTINUE       : `continue`\
> KW_CONST          : `contract`\
> KW_DEF            : `def`\
> KW_ELIF           : `elif`\
> KW_ELSE           : `else`\
> KW_EMIT           : `emit`\
> KW_ENUM           : `enum`\
> KW_EVENT          : `event`\
> KW_FALSE          : `false`\
> KW_FOR            : `for`\
> KW_IDX             : `idx`\
> KW_IF             : `if`\
> KW_IN             : `in`\
> KW_LET            : `let`\
> KW_NONPAYABLE     : `nonpayable`\
> KW_PASS           : `pass`\
> KW_PAYABLE        : `payable`\
> KW_PUB            : `pub`\
> KW_RETURN         : `return`\
> KW_REVERT         : `revert`\
> KW_SELFVALUE      : `self`\
> KW_STRUCT         : `struct`\
> KW_TRUE           : `true`\
> KW_WHILE          : `while` \
> KW_ADDRESS        : `address`


#### 2.1.2. Reserved keywords

These keywords aren't used yet, but they are reserved for future use. They have
the same restrictions as strict keywords. The reasoning behind this is to make
current programs forward compatible with future versions of Fe by forbidding
them to use these keywords.

> **<sup>Lexer</sup>**\
> KW_ABSTRACT       : `abstract`\
> KW_ABSTRACT       : `async`\
> KW_ABSTRACT       : `await`\
> KW_DO             : `do`\
> KW_EXTERNAL       : `external`\
> KW_FINAL          : `final`\
> KW_IMPL           : `impl`\
> KW_MACRO          : `macro`\
> KW_MATCH          : `match`\
> KW_MUT            : `mut`\
> KW_OVERRIDE       : `override`\
> KW_PURE           : `pure`\
> KW_SELFTYPE       : `Self`\
> KW_STATIC         : `static`\
> KW_SUPER          : `super`\
> KW_TRAIT          : `trait`\
> KW_TYPE           : `type`\
> KW_TYPEOF         : `typeof`\
> KW_USE            : `use`\
> KW_VIEW           : `view`\
> KW_VIRTUAL        : `virtual`\
> KW_WHERE          : `where`\
> KW_YIELD          : `yield`


### 2.2. Identifiers

> **<sup>Lexer:<sup>**\
> IDENTIFIER_OR_KEYWORD :\
> &nbsp;&nbsp; &nbsp;&nbsp; [`a`-`z` `A`-`Z`]&nbsp;[`a`-`z` `A`-`Z` `0`-`9` `_`]<sup>\*</sup>\
> &nbsp;&nbsp; | `_` [`a`-`z` `A`-`Z` `0`-`9` `_`]<sup>+</sup>
><sub>*Except a [strict] or [reserved] keyword*</sub>
>

An identifier is any nonempty ASCII string of the following form:

Either

* The first character is a letter.
* The remaining characters are alphanumeric or `_`.

Or

* The first character is `_`.
* The identifier is more than one character. `_` alone is not an identifier.
* The remaining characters are alphanumeric or `_`.

[strict]: index.md#strict-keywords
[reserved]: index.md#reserved-keywords


### 2.3. Comments

MISSING


### 2.4. End of header

> **<sup>Lexer:<sup>**\
> _EndOfHeader_ :\
> &nbsp;&nbsp; `:`

### 2.5. Block Expression

> **<sup>Lexer:<sup>**\
> _BlockExpression_ :\

The contents of a block expression are indented from the parent context following [Python indentation rules](https://docs.python.org/2.0/ref/indentation.html).

## 3. Items

### 3.1. Functions

> **<sup>Syntax</sup>**\
> _Function_ :\
> &nbsp;&nbsp; _FunctionDecorators_\
> &nbsp;&nbsp; _FunctionQualifiers_ `def` [IDENTIFIER]\
> &nbsp;&nbsp; &nbsp;&nbsp; `(` _FunctionParameters_<sup>?</sup> `)`\
> &nbsp;&nbsp; &nbsp;&nbsp; _FunctionReturnType_<sup>?</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; [_EndOfHeader_]\
> &nbsp;&nbsp; &nbsp;&nbsp; [_BlockExpression_]
>
> _FunctionQualifiers_ :\
> &nbsp;&nbsp; `pub`<sup>?</sup>
>
> _FunctionDecorators_ :\
> &nbsp;&nbsp; _FunctionDecorator_<sup>\*</sup>
>
> _FunctionDecorator_ :\
> &nbsp;&nbsp; `@`[IDENTIFIER]
>
> _FunctionParameters_ :\
> &nbsp;&nbsp; _FunctionParam_ (`,` _FunctionParam_)<sup>\*</sup> `,`<sup>?</sup>
>
> _FunctionParam_ :\
> &nbsp;&nbsp; [IDENTIFIER] `:` [_Type_]
>
> _FunctionReturnType_ :\
> &nbsp;&nbsp; `->` [_Type_]


A _function_ consists of a [block], along with a name and a set of parameters.
Other than a name, all these are optional. Functions are declared with the
keyword `def`. Functions may declare a set of *input* [*variables*][variables]
as parameters, through which the caller passes arguments into the function, and
the *output* [*type*][type] of the value the function will return to its caller
on completion.

When referred to, a _function_ yields a first-class *value* of the
corresponding zero-sized [*function item type*], which
when called evaluates to a direct call to the function.

A function header ends with a colon (`:`) after which the function body begins.

For example, this is a simple function:

```python
def answer_to_life_the_universe_and_everything() -> u256:
    return 42;
```

#### 3.1.1 Visibility and Privacy

These two terms are often used interchangeably, and what they are attempting to convey is the answer to the question "Can this item be used at this location?"

Fe knows two different types of visibility for functions and state variables: `public` and `private`. Visibility of `private` is the default and is used if no other visiblity is specified.

**Public:** External functions are part of the contract interface, which means they can be called from other contracts and via transactions.

**Private:** Those functions and state variables can only be accessed internally from within the same contract. This is the default visibility.

For example, this is a function that can be called externally from a transaction:

```python
pub def answer_to_life_the_universe_and_everything() -> u256:
    return 42;
```

### 3.2. Structs

> **<sup>Syntax</sup>**\
> _Struct_ :\
> &nbsp;&nbsp; `struct`
>   [IDENTIFIER]&nbsp;
> &nbsp;&nbsp; [_EndOfHeader_]\
> &nbsp;&nbsp; _StructField_<sup>\*</sup>
>
> _StructField_ :\
> &nbsp;&nbsp; [IDENTIFIER] `:` [_Type_]


A _struct_ is a nominal [struct type] defined with the keyword `struct`.

An example of a `struct` item and its use:

```
struct Point:
    x: u256
    y: u256

p = Point {x: 10, y: 11}
px: u256 = p.x;
```


Builtin functions:

- `abi_encode()` encodes the struct as an ABI tuple and returns the encoded data as a fixed-size byte array that is equal in size to the encoding.

### 3.3. Events

> **<sup>Syntax</sup>**\
> _Event_ :\
> &nbsp;&nbsp; `event`
>   [IDENTIFIER]&nbsp;
> &nbsp;&nbsp; [_EndOfHeader_]\
> &nbsp;&nbsp; _EventField_<sup>\*</sup>
>
> _EventField_ :\
> &nbsp;&nbsp; _EventIndexability_ [IDENTIFIER] `:` [_Type_]

> _EventIndexability_ :\
> &nbsp;&nbsp; `idx`<sup>?</sup>

An _event_ is a nominal [event type] defined with the keyword `event`. It is emitted with the keyword `emit`.

An example of a `event` item and its use:

```
event Transfer:
    idx sender: address
    idx receiver: address
    value: u256

def transfer(to : address, value : u256):
   # Heavy logic here
   # All done, log the event for listeners
   emit Transfer(msg.sender, _to, _value)
```

### 3.4. Enumeration

> **<sup>Syntax</sup>**\
> _Enumeration_ :\
> &nbsp;&nbsp; `enum`
>   [IDENTIFIER]&nbsp;
> &nbsp;&nbsp; [_EndOfHeader_]\
> &nbsp;&nbsp; _EnumField_<sup>\*</sup>
>
> _EnumField_ :\
> &nbsp;&nbsp; [IDENTIFIER]`,`

An *enumeration*, also referred to as *enum* is a simultaneous definition of a
nominal [enumerated type], that can be used to create or pattern-match values of the corresponding enumerated type.

Enumerations are declared with the keyword `enum`.

An example of an `enum` item and its use:

```
enum Animal:
    Dog,
    Cat,

barker = Animal.Dog
```

### 3.5. Type aliases

> **<sup>Syntax</sup>**\
> _TypeAlias_ :\
> &nbsp;&nbsp; `type` [IDENTIFIER]&nbsp;`=` [_Type_]

A _type alias_ defines a new name for an existing [type]. Type aliases are
declared with the keyword `type`.

For example, the following defines the type `BookMsg` as a synonym for the type
`bytes[100]`, a sequence of `100` bytes:

```
type BookMsg = bytes[100]
```

### 3.6. Contracts

> **<sup>Syntax</sup>**\
> _Contract_ :\
> &nbsp;&nbsp; `contract`
>   [IDENTIFIER]&nbsp;
> &nbsp;&nbsp; [_EndOfHeader_]\
> &nbsp;&nbsp; _ContractMember_<sup>\*</sup>
>
> _ContractMember_:\
> &nbsp;&nbsp; [_Visibility_]<sup>?</sup>\
> &nbsp;&nbsp; (\
> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;  [_ContractField_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Function_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Struct_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Event_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Enumeration_]\
> &nbsp;&nbsp; )
>
> _Visibility_ :\
> &nbsp;&nbsp; `pub`<sup>?</sup>
>
> _ContractField_ :\
> &nbsp;&nbsp; [IDENTIFIER] `:` [_Type_]


A _contract_ in Fe is a collection of code that resides at a specific address on the Ethereum blockchain. It is defined with the keyword `contract`.

An example of a `contract`:

```
contract GuestBook:
    pub guest_book: Map<address, bytes[100]>

    event Signed:
        idx book_msg: bytes[100]

    pub def sign(book_msg: bytes[100]):
        self.guest_book[msg.sender] = book_msg

        emit Signed(book_msg=book_msg)

    pub def get_msg(addr: address) -> bytes[100]:
        return self.guest_book[addr]
```


## 4. Statements and Expressions

### 4.1. Statements

### 4.1.1 `pragma` statement


> **<sup>Syntax</sup>**\
> _PragmaStatement_ :\
> &nbsp;&nbsp; `pragma` [_VersionRequirementExpression_]

The pragma statement is denoted with the keyword `pragma`. Evaluating a `pragma`
statement will cause the compiler to reject compilation if the version of the compiler does not conform to the given version requirement.

An example of a `pragma` statement:

```
pragma ^0.1.0
```

The version requirement syntax is identical to the one that is used by cargo ([more info](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html)).

### 4.1.2 `revert` statement


> **<sup>Syntax</sup>**\
> _RevertStatement_ :\
> &nbsp;&nbsp; `revert`<sup>?</sup>

The revert statement is denoted with the keyword `revert`. Evaluating a `revert`
statement will cause to revert all state changes made by the call and return with an revert error to the caller.

An example of a `revert` statement:

```
def transfer(to : address, value : u256):
    if not self.in_whitelist(to):
        revert
    # more logic here

### 4.2 Expressions

### 4.2.1 Arithmetic Operators

> **<sup>Syntax</sup>**\
> _ArithmeticExpression_ :\
> &nbsp;&nbsp;&nbsp;&nbsp; [_Expression_] `+` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `-` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `*` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `/` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `%` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `**` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `&` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `|` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `^` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `<<` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `>>` [_Expression_]

Binary operators expressions are all written with [infix notation](https://en.wikipedia.org/wiki/Infix_notation).
This table summarizes the behavior of arithmetic and logical binary operators on
primitive types.

| Symbol | Integer                 | Status      | Discussions    |
|--------|-------------------------|-------------|----------------|
| `+`    | Addition                | IMPLEMENTED |                |
| `-`    | Subtraction             | IMPLEMENTED |                |
| `*`    | Multiplication          | IMPLEMENTED |                |
| `/`    | Division*               | IMPLEMENTED |                |
| `%`    | Remainder               | IMPLEMENTED |                |
| `**`   | Exponentiation          | IMPLEMENTED |                |
| `&`    | Bitwise AND             | IMPLEMENTED |                |
| <code>&#124;</code> | Bitwise OR | IMPLEMENTED |                |
| `^`    | Bitwise XOR             | IMPLEMENTED |                |
| `<<`   | Left Shift              | IMPLEMENTED |                |
| `>>`   | Right Shift             | IMPLEMENTED |                |

\* Integer division rounds towards zero.


Here are examples of these operators being used.

```
3 + 6 == 9
6 - 3 == 3
2 * 3 == 6
6 / 3 == 2 TODO: Rest
5 % 4 == 1
2 ** 4 == 16
12 & 25 == 8
12 | 25 == 29
12 ^ 25 == 21
212 << 1 == 424
212 >> 1 == 106
```

### 4.2.2 Comparision Operators

> **<sup>Syntax</sup>**\
> _ComparisonExpression_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Expression_] `==` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `!=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `>` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `<` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `>=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `<=` [_Expression_]


| Symbol | Meaning                  |     Status                 |
|--------|--------------------------|----------------------------|
| `==`   | Equal                    |         IMPLEMENTED        |
| `!=`   | Not equal                |         IMPLEMENTED        |
| `>`    | Greater than             |         IMPLEMENTED        |
| `<`    | Less than                |         IMPLEMENTED        |
| `>=`   | Greater than or equal to |         IMPLEMENTED        |
| `<=`   | Less than or equal to    |         IMPLEMENTED        |

Here are examples of the comparison operators being used.

```
123 == 123
23 != -12
12 > 11
11 >= 11
11 < 12
11 <= 11
```

## 5. Types system

### 5.1 Types

#### 5.1.1 Types

Every variable, item, and value in a Fe program has a type. The _type_ of a
*value* defines the interpretation of the memory holding it and the operations
that may be performed on the value.

Built-in types are tightly integrated into the language, in nontrivial ways
that are not possible to emulate in user-defined types. User-defined types have
limited capabilities.

The list of types is:

* Data types
    * Base types:
        * [Boolean] — `true` or `false`
        * [Address] - Ethereum address
        * [Numeric] — integer
    * Reference types:
        * Sequence types
            * [Tuple]
            * [Array]
            * [Bytes]
            * [String]
            * [Struct]
            * [Enum]
        * [HashMap]
* Other types:
    * [Event]
    * [Contract]
    * [Function]

### 5.1.1.1 Boolean type

The `bool` type is a data type which can be either `true` or `false`.

Example:

```rust
x = true
```

### 5.1.1.2. Contract type

An *contract type* is the type denoted by the name of an [`contract` item].

A value of a given contract type carries the contract's public interface as 
attribute functions. A new contract value can be created by either casting
an address to a contract type or by creating a new contract using the type 
attribute functions `create` or `create2`.

example:

```python 
 contract Foo:
    pub def get_my_num() -> u256:
        return 42

contract FooFactory:
    pub def create2_foo() -> address:
        # `0` is the value being sent and `52` is the address salt
        foo: Foo = Foo.create2(0, 52)
        return address(foo)
```

### 5.1.1.3. Numeric types

The unsigned integer types consist of:

Type   | Minimum | Maximum
-------|---------|-------------------
`u8`   | 0       | 2<sup>8</sup>-1
`u16`  | 0       | 2<sup>16</sup>-1
`u32`  | 0       | 2<sup>32</sup>-1
`u64`  | 0       | 2<sup>64</sup>-1
`u128` | 0       | 2<sup>128</sup>-1
`u256` | 0       | 2<sup>256</sup>-1

The signed two's complement integer types consist of:

Type   | Minimum            | Maximum
-------|--------------------|-------------------
`i8`   | -(2<sup>7</sup>)   | 2<sup>7</sup>-1
`i16`  | -(2<sup>15</sup>)  | 2<sup>15</sup>-1
`i32`  | -(2<sup>31</sup>)  | 2<sup>31</sup>-1
`i64`  | -(2<sup>63</sup>)  | 2<sup>63</sup>-1
`i128` | -(2<sup>127</sup>) | 2<sup>127</sup>-1
`i256` | -(2<sup>255</sup>) | 2<sup>255</sup>-1


### 5.1.1.4. Textual types

MISSING

### 5.1.1.5. Tuple types

MISSING

### 5.1.1.6. Array types

MISSING

### 5.1.1.7. Struct types

An *struct type* is the type denoted by the name of an [`struct` item].

### 5.1.1.8. Enumerated types

An *enum type* is the type denoted by the name of an [`enum` item].

### 5.1.1.9. Function item types

MISSING

### 5.1.1.10. Address type

MISSING

### 5.1.1.11. HashMap type

Maps a key to a value.

Example:

```
Map<TKey,TValue>
```

Where TKey is a base type and TValue is any data type.

### 5.1.1.12. Bytes types

MISSING

### 5.1.1.13. String types

A String type.

Example:

```
content: String<100>
```

### 5.1.1.14. Event types

An *event type* is the type denoted by the name of an [`event` item].

## 6. Data Layout

There are three places where data can be stored on the EVM:

- **stack**: 256-bit values placed on the stack that are loaded using `DUP` operations.
- **storage**: 256-bit address space where 256-bit values can be stored. Accessing higher
storage slots does not increase gas cost.
- **memory**: 256-bit address space where 256-bit values can be stored. Accessing higher
memory slots increases gas cost.

Each data type described in section 5 can be stored in these locations. How data is stored is
described in this section.

### 6.1. Stack

The following can be stored on the stack:

- base type values
- pointers to sequence type values

The size of each value stored on the stack must not exceed 256 bits. Since all base types are less
than or equal to 256 bits in size, we store them on the stack. Pointers to values stored in memory
may also be stored on the stack.

Example:

```python
# function scope
foo: u256 = 42 # foo is stored on the stack
bar: u256[100] # bar is a memory pointer stored on the stack
```

### 6.2. Storage

All data types can be stored in storage.

#### 6.2.1. Constant size values in storage

Storage pointers for constant size values are determined at compile time.

Example:

```python
# contract scope
foo: u256 # foo is assigned a static pointer by the compiler
```

The value of a base type in storage is found by simply loading the value from storage at the
given pointer.

To find an element inside of a sequence type, the relative location of the element is added to the
given pointer.

#### 6.2.2. Maps in storage

Maps are not assigned pointers, because they do not have a location in storage. They are instead
assigned a nonce that is used to derive the location of keyed values during runtime.

Example:

``` python
# contract scope
bar: Map<address, u256> # bar is assigned a static nonce by the compiler
baz: Map<address, Map<address, u256>> # baz is assigned a static nonce by the compiler
```

The expression `bar[0x00]` would resolve to the hash of both bar's nonce and the key value
.i.e. `keccak256(<bar nonce>, 0x00)`. Similarly, the expression `baz[0x00][0x01]` would resolve to
a nested hash i.e. `keccak256(keccak256(<baz nonce>, 0x00), 0x01)`.

#### 6.2.3. The `to_mem` function

Reference type values can be copied from storage and into memory using the `to_mem` function.

Example:

```python
my_array_var: u256[10] = self.my_array_field.to_mem()
```

### 6.3. Memory

Only sequence types can be stored in memory.

The first memory slot (`0x00`) is used to keep track of the lowest available memory slot. Newly
allocated segments begin at the value given by this slot. When more memory has been allocated,
the value stored in `0x00` is increased.

We do not free memory after it is allocated.

#### 6.3.1. Sequence types in memory

Sequence type values may exceed the 256-bit stack slot size, so we store them in memory and
reference them using pointers kept on the stack.

Example:

```python
# function scope
foo: u256[100] # foo is a pointer that references 100 * 256 bits in memory.
```

To find an element inside of a sequence type, the relative location of the element is added to the
given pointer.

#### 6.2.3. The `clone` function

Reference type values in memory can be cloned using the `clone` function.

Example:

```python
# with clone
foo: u256[10] = bar.clone() # `foo` points to a new segment of memory
assert foo[1] == bar[1] 
foo[1] = 42
assert foo[1] != bar[1] # modifying `foo` does not modify bar

# without clone
foo: u256[10] = bar # `foo` and `bar` point to the same segment of memory
assert foo[1] == bar[1]
foo[1] = 42
assert foo[1] == bar[1] # modifying `foo` also modifies `bar`
```

### 6.4. Function calls

Constant size values stored on the stack or in memory can be passed into and returned by functions.

[IDENTIFIER]: #22-identifiers
[_EndOfHeader_]: #24-end-of-header
[_BlockExpression_]: #25-block-expression
[Items]: #3-items
[Structs]: #32-structs
  [`struct` item]: #32-structs
[Events]: #33-events
  [`event` item]: #33-events
[Enumeration]: #34-enumeration
  [variants]: #34-enumeration
  [`enum` item]: #34-enumeration
[Contracts]: #36-contracts
  [`contract` item]: #36-contracts
[Expressions]: #42-expressions
  [expression]: #42-expressions
  [_Expression_]: #42-expressions
[Types]: #51-types
  [_Type_]: #51-types
  [type]: #51-types
[Boolean]: #5111-boolean-types
[Contract]: #5112-contract-types
[Numeric]: #5113-numeric-type
[Textual]: #5114-textual-types
[Tuple]: #5115-tuple-types
[Array]: #5116-array-types
[Struct]: #5117-struct-types
  [struct type]: #5117-struct-types
[Enum]: #5118-enum-types
  [enumerated type]: #5118-enum-types
[Function]: #5119-function-types
[Address]: #51110-address-types
[HashMap]: #51111-hashmap-types
[Bytes]: #51112-bytes-types
[String]: #51113-string-types
[Event]: #51114-event-types
  [event type]: #51114-event-types
