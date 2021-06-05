# list-func-solver

atoms       :: { Exp }
atoms       : '[' lista ']' ':'':' NUM               { List ($2, $6, DEFAULT) }
            | VAR ':'':' NUM                         { Var ($1, $4, DEFAULT) }
            | '[' lista ']' '<' TYPE '>' ':'':' NUM  { List ($2, $9, $5) }
            | VAR '<' TYPE '>' ':'':' NUM            { Var ($1, $7, $3) }
