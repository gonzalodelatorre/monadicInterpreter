# monadicInterpreter


Monadic interpreter for a simple imperative language.
Here's the concrete syntax for this language:
 
 
digit ::= '0' | '1' | ... | '9'
letter ::= 'a' | .. | 'Z'
nat ::= digit | digit nat
var ::= letter | letter var
intexp ::= nat
		| var
		| '-' intexp
		| intexp '+' intexp
		| intexp '-' intexp
		| intexp '*' intexp
		| intexp '/' intexp
		| '(' intexp ')'
		| var '=' intexp
		| intexp ';' intexp
boolexp ::= 'true' | 'false'
		| intexp '==' intexp
		| intexp '!=' intexp
		| intexp '<' intexp
		| intexp '>' intexp
		| boolexp '&&' boolexp
		| boolexp '||' boolexp
		| '!' boolexp
		| '(' boolexp ')'
comm ::= skip
		| var '=' intexp
		| comm ';' comm
		| 'if' boolexp '{' comm '}'
		| 'if' boolexp '{' comm '}' 'else' '{' comm '}'
		| 'while' boolexp '{' comm '}'
