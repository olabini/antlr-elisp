lexer grammar calc_java;
options { language = Java; }

LPAREN: '(';
RPAREN: ')';
PI: 'PI';
E: 'E';
INTEGER: DIGIT+;
DECIMAL: DIGIT+ '.' DIGIT+;

fragment
DIGIT: '0'..'9';
WS: (' ' | '\n' | '\t')+ { channel = 99 };

