%token <int> INT
%token PLUS MINUS TIMES DIV TWICEPLUS POWER SQUARE CUBE REVERSEMINUS RANDOMPLUS REMAINDERFIVE BIGBANG BOMB
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS
%left REMAINDERFIVE TIMES DIV TWICEPLUS
%right SQUARE CUBE REVERSEMINUS RANDOMPLUS BOMB
%nonassoc UMINUS POWER BIGBANG
%start main
%type <int> main
%%
main:
    expr EOL                { $1 }
;
expr:
    INT                     { $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 + $3 }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
  | expr TWICEPLUS expr     { $1 + 2 * $3 }
  | expr POWER expr         { Float.pow (float $1) (float $3) |> int_of_float }
  | expr SQUARE             { $1 * $1 }
  | expr CUBE               { $1 * $1 * $1 }
  | expr REVERSEMINUS expr  { - $1 + $3 }
  | expr REMAINDERFIVE expr    { ($1 + $3) mod 5 }
  | expr BIGBANG expr       { $1 * $1 + $3 * $3 }
  | expr BOMB expr          { $1 * 10 + $3 }
//   | expr RANDOMPLUS expr    { (Random.int_range 0 (1 + $3)) }
  | MINUS expr %prec UMINUS { - $2 }