%token MOVER
%token MOVEL
%token INCR
%token DECR
%token LOOPIN
%token LOOPOUT
%token PRINT
%token EOF

%start <Ast.exprlist> prog

%%

prog:
  | e = expr*; EOF { e }
  ;

expr:
  | MOVER { Move_Right }
  | MOVEL { Move_Left }
  | INCR { Increment }
  | DECR { Decrement }
  | LOOPIN { Enter_Loop }
  | LOOPOUT { Exit_Loop }
  | PRINT { Print }
  ;
