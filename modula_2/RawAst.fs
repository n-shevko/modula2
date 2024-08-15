module modula_2.RawAst


type Digit_plus =
  Digit_plus0 of digit       : Digit *     
                 digit_plus  : Digit_plus  

| Digit_plus1 of digit  : Digit  


and Symbol_plus =
  Symbol_plus0 of symbol       : Symbol *     
                  symbol_plus  : Symbol_plus  

| Symbol_plus1 of symbol  : Symbol  


and Delimeter_star =
  Delimeter_star0 of delimeter       : Delimeter *     
                     delimeter_star  : Delimeter_star  

| Delimeter_star1 of term  : string   // ''


and Identifiers_tail =
  Identifiers_tail0 of delimeter_star    : Delimeter_star *         
                       term              : string *           // ','
                       delimeter_star_2  : Delimeter_star *         
                       identifier        : Identifier *             
                       identifiers_tail  : Identifiers_tail         

| Identifiers_tail1 of term  : string   // ''


and Qualidents_tail =
  Qualidents_tail0 of term             : string *          // '.'
                      identifier       : Identifier *            
                      qualidents_tail  : Qualidents_tail         

| Qualidents_tail1 of term  : string   // ''


and From_optional =
  From_optional0 of term              : string *         // 'from'
                    delimeter_plus    : Delimeter_plus *          
                    identifier        : Identifier *              
                    delimeter_plus_2  : Delimeter_plus            

| From_optional1 of term  : string   // ''


and Delimeter_plus =
  Delimeter_plus0 of delimeter       : Delimeter *     
                     delimeter_plus  : Delimeter_plus  

| Delimeter_plus1 of delimeter  : Delimeter  


and Exprs_tail =
  Exprs_tail0 of delimeter_star    : Delimeter_star *       
                 term              : string *         // ','
                 delimeter_star_2  : Delimeter_star *       
                 expr              : Expr *                 
                 exprs_tail        : Exprs_tail             

| Exprs_tail1 of term  : string   // ''


and Exp_list_optional =
  Exp_list_optional0 of exp_list  : Exp_list  
| Exp_list_optional1 of term  : string   // ''


and Optional_var = string // 'var' ''


and Formal_parameters_tail =
  Formal_parameters_tail0 of delimeter_star          : Delimeter_star *               
                             term                    : string *                 // ';'
                             delimeter_star_2        : Delimeter_star *               
                             formal_parameter        : Formal_parameter *             
                             formal_parameters_tail  : Formal_parameters_tail         

| Formal_parameters_tail1 of term  : string   // ''


and Formal_parameter_list_optional =
  Formal_parameter_list_optional0 of formal_parameter_list  : Formal_parameter_list  
| Formal_parameter_list_optional1 of term  : string   // ''


and Optional_return_type =
  Optional_return_type0 of delimeter_star    : Delimeter_star *       
                           term              : string *         // ':'
                           delimeter_star_2  : Delimeter_star *       
                           qualident         : Qualident              

| Optional_return_type1 of term  : string   // ''


and Constant_declarations =
  Constant_declarations0 of delimeter_star         : Delimeter_star *              
                            constant_declaration   : Constant_declaration *        
                            delimeter_star_2       : Delimeter_star *              
                            term                   : string *                // ';'
                            constant_declarations  : Constant_declarations         

| Constant_declarations1 of term  : string   // ''


and Variable_declarations =
  Variable_declarations0 of delimeter_star         : Delimeter_star *              
                            variable_declaration   : Variable_declaration *        
                            delimeter_star_2       : Delimeter_star *              
                            term                   : string *                // ';'
                            variable_declarations  : Variable_declarations         

| Variable_declarations1 of term  : string   // ''


and Typedefs =
  Typedefs0 of identifier        : Identifier *           
               delimeter_star    : Delimeter_star *       
               term              : string *         // '='
               delimeter_star_2  : Delimeter_star *       
               type_def          : Type_def *             
               delimeter_star_3  : Delimeter_star *       
               term_2            : string *         // ';'
               typedefs          : Typedefs               

| Typedefs1 of term  : string   // ''


and Optional_expr =
  Optional_expr0 of delimeter_star  : Delimeter_star *
                    expr            : Expr            

| Optional_expr1 of term  : string   // ''


and Statements =
  Statements0 of statement         : Statement *            
                 delimeter_star    : Delimeter_star *       
                 term              : string *         // ';'
                 delimeter_star_2  : Delimeter_star *       
                 statements        : Statements             

| Statements1 of statement         : Statement *            
                 delimeter_star    : Delimeter_star *       
                 term              : string *         // ';'
                 delimeter_star_2  : Delimeter_star         


and Else_branch =
  Else_branch0 of term                : string *             // 'else'
                  delimeter_plus      : Delimeter_plus *              
                  statement_sequence  : Statement_sequence            

| Else_branch1 of term  : string   // ''


and Declarations =
  Declarations0 of declaration     : Declaration *   
                   delimeter_star  : Delimeter_star *
                   declarations    : Declarations    

| Declarations1 of term  : string   // ''


and Statements_in_block =
  Statements_in_block0 of term                : string *             // 'begin'
                          delimeter_plus      : Delimeter_plus *               
                          statement_sequence  : Statement_sequence             

| Statements_in_block1 of term  : string   // ''


and Formal_parameters_optional =
  Formal_parameters_optional0 of formal_parameters  : Formal_parameters  
| Formal_parameters_optional1 of term  : string   // ''


and Imports =
  Imports0 of import            : Import *               
              delimeter_star    : Delimeter_star *       
              term              : string *         // ';'
              delimeter_star_2  : Delimeter_star *       
              imports           : Imports                

| Imports1 of term  : string   // ''


and Letter = string // 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z'


and Digit = string // '0' '1' '2' '3' '4' '5' '6' '7' '8' '9'


and Number =
  Number0 of digit_plus  : Digit_plus  


and Symbol =
  Symbol0 of letter  : Letter  
| Symbol1 of digit  : Digit  
| Symbol2 of term  : string   // '_'


and Identifier =
  Identifier0 of symbol_plus  : Symbol_plus  


and Delimeter = string // '\n' ' '


and Ident_list =
  Ident_list0 of identifier        : Identifier *      
                 delimeter_star    : Delimeter_star *  
                 identifiers_tail  : Identifiers_tail  


and Qualident =
  Qualident0 of identifier       : Identifier *     
                qualidents_tail  : Qualidents_tail  


and Import =
  Import0 of from_optional   : From_optional *             
             term            : string *         // 'import'
             delimeter_plus  : Delimeter_plus *            
             ident_list      : Ident_list                  


and Operation = string // '*' '/' '+' '-' 'div' 'mod' 'or' 'and' '=' '#' '<' '<=' '>' '>='


and Expr =
  Expr0 of expr_no_lrec      : Expr_no_lrec *  
           delimeter_star    : Delimeter_star *
           operation         : Operation *     
           delimeter_star_2  : Delimeter_star *
           expr              : Expr            

| Expr1 of expr_no_lrec  : Expr_no_lrec  


and Expr_no_lrec =
  Expr_no_lrec0 of term              : string *         // '('
                   delimeter_star    : Delimeter_star *       
                   expr              : Expr *                 
                   delimeter_star_2  : Delimeter_star *       
                   term_2            : string           // ')'

| Expr_no_lrec1 of term            : string *         // 'not'
                   delimeter_star  : Delimeter_star *         
                   expr            : Expr                     

| Expr_no_lrec2 of fn_call  : Fn_call  

| Expr_no_lrec3 of number  : Number  

| Expr_no_lrec4 of var_or_array_elem  : Var_or_array_elem  

| Expr_no_lrec5 of term              : string *         // '['
                   delimeter_star    : Delimeter_star *       
                   exp_list          : Exp_list *             
                   delimeter_star_2  : Delimeter_star *       
                   term_2            : string           // ']'


and Exp_list =
  Exp_list0 of expr        : Expr *      
               exprs_tail  : Exprs_tail  


and Fn_call =
  Fn_call0 of qualident          : Qualident *               
              term               : string *            // '('
              delimeter_star     : Delimeter_star *          
              exp_list_optional  : Exp_list_optional *       
              delimeter_star_2   : Delimeter_star *          
              term_2             : string              // ')'


and Constant_declaration =
  Constant_declaration0 of identifier        : Identifier *           
                           delimeter_star    : Delimeter_star *       
                           term              : string *         // '='
                           delimeter_star_2  : Delimeter_star *       
                           expr              : Expr                   


and Variable_declaration =
  Variable_declaration0 of ident_list        : Ident_list *           
                           delimeter_star    : Delimeter_star *       
                           term              : string *         // ':'
                           delimeter_star_2  : Delimeter_star *       
                           qualident         : Qualident              


and Formal_parameter =
  Formal_parameter0 of optional_var      : Optional_var *         
                       delimeter_star    : Delimeter_star *       
                       ident_list        : Ident_list *           
                       delimeter_star_2  : Delimeter_star *       
                       term              : string *         // ':'
                       delimeter_star_3  : Delimeter_star *       
                       qualident         : Qualident              


and Formal_parameter_list =
  Formal_parameter_list0 of formal_parameter        : Formal_parameter *      
                            formal_parameters_tail  : Formal_parameters_tail  


and Formal_parameters =
  Formal_parameters0 of term                            : string *                         // '('
                        delimeter_star                  : Delimeter_star *                       
                        formal_parameter_list_optional  : Formal_parameter_list_optional *       
                        delimeter_star_2                : Delimeter_star *                       
                        term_2                          : string *                         // ')'
                        optional_return_type            : Optional_return_type                   


and Type_def =
  Type_def0 of term            : string *         // 'array['
               number          : Number *                    
               term_2          : string *         // ']'     
               delimeter_star  : Delimeter_star *            
               term_3          : string *         // 'of'    
               delimeter_plus  : Delimeter_plus *            
               identifier      : Identifier                  

| Type_def1 of term                   : string *                // 'record'
               variable_declarations  : Variable_declarations *            
               delimeter_plus         : Delimeter_plus *                   
               term_2                 : string                  // 'end'   


and Declaration =
  Declaration0 of term                   : string *                // 'const'
                  constant_declarations  : Constant_declarations             

| Declaration1 of term                   : string *                // 'var'
                  variable_declarations  : Variable_declarations           

| Declaration2 of procedure_declaration  : Procedure_declaration *       
                  delimeter_star         : Delimeter_star *              
                  term                   : string                  // ';'

| Declaration3 of term            : string *         // 'type'
                  delimeter_plus  : Delimeter_plus *          
                  typedefs        : Typedefs                  


and Var_or_array_elem =
  Var_or_array_elem0 of identifier  : Identifier *       
                        term        : string *     // '['
                        expr        : Expr *             
                        term_2      : string       // ']'

| Var_or_array_elem1 of qualident  : Qualident  


and Statement =
  Statement0 of var_or_array_elem  : Var_or_array_elem *        
                delimeter_star     : Delimeter_star *           
                term               : string *            // ':='
                delimeter_star_2   : Delimeter_star *           
                expr               : Expr                       

| Statement1 of if_statement  : If_statement  

| Statement2 of while_statement  : While_statement  

| Statement3 of term           : string *        // 'return'
                optional_expr  : Optional_expr              

| Statement4 of fn_call  : Fn_call  


and Statement_sequence =
  Statement_sequence0 of statements  : Statements  


and If_statement =
  If_statement0 of term                : string *             // 'if'  
                   delimeter_plus      : Delimeter_plus *              
                   expr                : Expr *                        
                   delimeter_plus_2    : Delimeter_plus *              
                   term_2              : string *             // 'then'
                   delimeter_plus_3    : Delimeter_plus *              
                   statement_sequence  : Statement_sequence *          
                   else_branch         : Else_branch *                 
                   delimeter_star      : Delimeter_star *              
                   term_3              : string               // 'end' 


and While_statement =
  While_statement0 of term                : string *             // 'while'
                      delimeter_plus      : Delimeter_plus *               
                      expr                : Expr *                         
                      delimeter_plus_2    : Delimeter_plus *               
                      term_2              : string *             // 'do'   
                      delimeter_plus_3    : Delimeter_plus *               
                      statement_sequence  : Statement_sequence *           
                      term_3              : string               // 'end'  


and Block =
  Block0 of declarations         : Declarations *                
            statements_in_block  : Statements_in_block *         
            delimeter_star       : Delimeter_star *              
            term                 : string                // 'end'


and Procedure_declaration =
  Procedure_declaration0 of term                        : string *                     // 'procedure'
                            delimeter_plus              : Delimeter_plus *                           
                            identifier                  : Identifier *                               
                            delimeter_star              : Delimeter_star *                           
                            formal_parameters_optional  : Formal_parameters_optional *               
                            term_2                      : string *                     // ';'        
                            delimeter_star_2            : Delimeter_star *                           
                            block                       : Block *                                    
                            delimeter_star_3            : Delimeter_star *                           
                            identifier_2                : Identifier                                 


and Modula_2 =
  Modula_20 of term              : string *         // 'module'
               delimeter_plus    : Delimeter_plus *            
               identifier        : Identifier *                
               delimeter_star    : Delimeter_star *            
               term_2            : string *         // ';'     
               delimeter_star_2  : Delimeter_star *            
               imports           : Imports *                   
               block             : Block *                     
               delimeter_star_3  : Delimeter_star *            
               identifier_2      : Identifier *                
               term_3            : string *         // '.'     
               delimeter_star_4  : Delimeter_star              