# lol-lang
A programming language based on C syntax and League of Legends terms.


## BNF syntax
```
<program> ::= <declaration>*

<declaration> ::= <champion-decl> | <item-decl> | <nexus-decl>

<nexus-decl> ::= "nexus" "main" "(" ")" "{" <statement>* "}"

<champion-decl> ::= "champion" <identifier> "(" <param-list>? ")" "->" <type> "{" <statement>* "}"

<param-list> ::= <param> | <param> "," <param-list>
<param> ::= <identifier> ":" <type>

<item-decl> ::= "shop" <identifier> ":" <type> "=" <expression> ";"

<statement> ::= <block>
              | <item-decl>
              | <assignment>
              | <coinflip>
              | <respawn>
              | <ping>
              | <return-stmt>
              | <expression-stmt>

<block> ::= "{" <statement>* "}"

<assignment> ::= <identifier> "=" <expression> ";"

<coinflip> ::= "coinflip" "(" <expression> ")" <block> ("else" <block>)?

<respawn> ::= "respawn" "(" <expression> ")" <block>

<ping> ::= "ping" "(" <expression> ")" ";"

<return-stmt> ::= "return" <expression> ";"

<expression-stmt> ::= <expression> ";"

<expression> ::= <literal>
               | <identifier>
               | <binary-expr>
               | <unary-expr>
               | <call-expr>
               | <group-expr>
               | <team-expr>
               | <duo-expr>
               | <index-expr>

<binary-expr> ::= <expression> <binary-op> <expression>
<binary-op> ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||"

<unary-expr> ::= <unary-op> <expression>
<unary-op> ::= "-" | "!"

<call-expr> ::= <identifier> "(" <argument-list>? ")"
<argument-list> ::= <expression> | <expression> "," <argument-list>

<group-expr> ::= "(" <expression> ")"

<team-expr> ::= "[" <team-items>? "]"
<team-items> ::= <expression> | <expression> "," <team-items>

<duo-expr> ::= "(" <expression> "," <expression> ")"

<index-expr> ::= <expression> "[" <expression> "]"

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
         | "Team" "<" <type> ">"            // list type
         | <identifier>                     // user-defined type

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*
```