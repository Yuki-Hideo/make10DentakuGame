{
open Parser
exception Eof
}
rule token = parse
    [' ' '\t']     { token lexbuf }
  | ['\n' ]        { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  (* | '+'            { PLUS } *)
  (* | '-'            { MINUS } *)
  (* | '-'            { TIMES } *)
  (* | '@'            { DIV } *)
  (* | '('            { LPAREN } *)
  (* | ')'            { RPAREN } *)
  | '!'            { TWICEPLUS }
  | '?'            { POWER }
  (* | '&'            { SQUARE } *)
  | '%'            { CUBE }
  | '#'            { REVERSEMINUS }
  | '/'            { REMAINDERFIVE }
  | '^'            { BIGBANG }
  | '='            { BOMB }
  (* | '&'            { RANDOMPLUS } *)
  | eof            { raise Eof }