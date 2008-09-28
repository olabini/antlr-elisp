grammar full_calc_elisp;
options { language = ELisp; }

evaluate : expression ;

expression : 
    mult (
    '+' mult
  | '-' mult
  )*
    ;

mult : 
    log (
    '*' log
  | '/' log 
  | '%' log 
  )* 
    ;

log : 
    'ln' exp
  | exp 
  ;

exp : atom ('^' atom )? ;

atom :
    INTEGER
  | DECIMAL
  | '(' expression ')'
  | 'PI'
  | 'E'
  ;

INTEGER: DIGIT+;
DECIMAL: DIGIT+ '.' DIGIT+;

fragment
DIGIT: '0'..'9';
WS: (' ' | '\n' | '\t')+ { (lexer-set-channel 99) };
