# monadicInterpreter


Monadic interpreter for a simple imperative language that contains integer variables, sequences, and if/while statements.

Within this language, you can write programs like this:



x = 0;
while (x < 700) {
  x = x + 1
}


or:


n = 25;
i = 0;
t = 0; 
while (t != n) {i = i + 1; 
                t = i * i} 


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
	| intexp '* ' intexp
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
