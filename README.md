# lol-lang
A programming language based on C syntax and League of Legends terms.


## BNF syntax
```
<program> ::= <declaration>*

<comment> ::= "/all" [^\n]*

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

<declaration> ::= <ability-decl> | <item-decl> | <nexus-decl>

<statement> ::= <block>
              | <item-decl>
              | <assignment>
              | <coinflip>
              | <go next>
              | <ping>
              | <recall-stmt>
              | <expression-stmt>

<nexus-decl> ::= "nexus" "(" ")" "{" <statement>* "}" // Main function

<ability-decl> ::= "ability" <identifier> "(" <param-list>? ")" "->" <type> "{" <statement>* "}" // Function declaration

<param-list> ::= <param> | <param> "," <param-list>
<param> ::= <identifier> ":" <type>

<item-decl> ::= "buy" <identifier> ":" <type> "=" <expression> ";" // Variable assignment/declaration

<block> ::= "{" <statement>* "}"

<assignment> ::= <identifier> "=" <expression> ";" // Variable reassignment

<coinflip> ::= "coinflip" "(" <expression> ")" <block> ("ff15" <block>)? // If / else

<go next> ::= "go next" "(" <expression> ")" <block> // While loop

<ping> ::= "ping" "(" <expression> ")" ";" // Print function

<recall-stmt> ::= "recall" <expression> ";" // Return statement

<expression-stmt> ::= <expression> ";"

<expression> ::= <literal>
               | <identifier>
               | <binary-expr>
               | <unary-expr>
               | <call-expr>
               | <group-expr>
               | <inventory-expr>
               | <duo-expr>
               | <index-expr>

<binary-expr> ::= <expression> <binary-op> <expression>
<binary-op> ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||"

<unary-expr> ::= <unary-op> <expression>
<unary-op> ::= "-" | "!"

<call-expr> ::= <identifier> "(" <argument-list>? ")" // Function call
<argument-list> ::= <expression> | <expression> "," <argument-list>

<group-expr> ::= "(" <expression> ")" // Plain parenthesis

<duo-expr> ::= "(" <expression> "," <expression> ")" // Tuple

<inventory-expr> ::= "[" <inventory-items>? "]" // List initialization
<inventory-items> ::= <expression> | <expression> "," <inventory-items>

<shop-expr> ::= "{" <shop-items>? "}" // Map initialization
<shop-items> ::= <duo-expr> | <duo-expr> "," <shop-items>

<index-expr> ::= <expression> "[" <expression> "]" // Indexing into list

<literal> ::= <integer> | <boolean> | <string> | <unit>
<integer> ::= [0-9]+
<boolean> ::= "true" | "false"
<string> ::= "\"" <any-char>* "\""
<unit> ::= "()"

<type> ::= "Gold"                           // integer type
         | "Status"                         // boolean type
         | "Chat"                           // string type
         | "Void"                           // unit type
         | "Duo" "<" <type> "," <type> ">"  // tuple type
         | "Inventory" "<" <type> ">"       // list type
         | "Shop" "<" <type> "," <type> ">" // map type
```
