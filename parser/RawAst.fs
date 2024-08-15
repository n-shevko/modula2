module bnf.RawAst


type Var_symbol = string // 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' '_'


and Var =
  Var0 of var_symbol  : Var_symbol *
          var         : Var         

| Var1 of var_symbol  : Var_symbol  


and Terminal_symbol = string // '`' '-' '=' '[' ']' '\' ';' ',' '.' '/' '~' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '_' '+' '{' '}' '|' ':' '"' '<' '>' '?' '\'' ' ' '\n' '	' '' '' 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' '0' '1' '2' '3' '4' '5' '6' '7' '8' '9'


and Terminal_symbols =
  Terminal_symbols0 of terminal_symbol   : Terminal_symbol * 
                       terminal_symbols  : Terminal_symbols  

| Terminal_symbols1 of term  : string   // ''


and Term =
  Term0 of term              : string *           // '''
           terminal_symbols  : Terminal_symbols *       
           term_2            : string             // '''


and Quantifier_or_nothing = string // '?' '+' '*' ''


and Separator_symbol = string // ' ' '\n' '	' '' ''


and Separator =
  Separator0 of separator_symbol  : Separator_symbol *
                separator         : Separator         

| Separator1 of term  : string   // ''


and Group_name =
  Group_name0 of term         : string *    // '{'
                 separator    : Separator *       
                 var          : Var *             
                 separator_2  : Separator *       
                 term_2       : string      // '}'

| Group_name1 of term  : string   // ''


and Expr =
  Expr0 of group      : Group *    
           separator  : Separator *
           expr       : Expr       

| Expr1 of group  : Group  


and Group =
  Group0 of term                   : string *                // '('
            separator              : Separator *                   
            expr                   : Expr *                        
            separator_2            : Separator *                   
            term_2                 : string *                // ')'
            group_name             : Group_name *                  
            quantifier_or_nothing  : Quantifier_or_nothing         

| Group1 of var                    : Var *                  
            quantifier_or_nothing  : Quantifier_or_nothing  

| Group2 of term                   : Term *                 
            group_name             : Group_name *           
            quantifier_or_nothing  : Quantifier_or_nothing  


and Branch =
  Branch0 of group      : Group *    
             separator  : Separator *
             branch     : Branch     

| Branch1 of group  : Group  


and Branches =
  Branches0 of branch       : Branch *          
               separator    : Separator *       
               term         : string *    // '|'
               separator_2  : Separator *       
               branches     : Branches          

| Branches1 of branch  : Branch  


and Left_side =
  Left_side0 of var  : Var  

| Left_side1 of term    : string * // '{'
                var     : Var *          
                term_2  : string   // '}'


and Equation =
  Equation0 of left_side    : Left_side *       
               separator    : Separator *       
               term         : string *    // '='
               separator_2  : Separator *       
               branches     : Branches *        
               separator_3  : Separator *       
               term_2       : string      // ';'


and Bnf =
  Bnf0 of separator  : Separator *
          equation   : Equation * 
          bnf        : Bnf        

| Bnf1 of separator    : Separator *
          equation     : Equation * 
          separator_2  : Separator  