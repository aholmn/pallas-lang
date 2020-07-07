# pallas

```
  GRAMMAR
  
  STATEMENTS

  Stmt     -> Print | Decl | Assign
  Decl     -> var Id "=" Comparison;
  Print    -> "print" Comparison ";"
  Assign   -> Id = Comparison;
 
  EXPRESSIONS
  
  Comparison -> Add ("!="|"=="|">"|">="|"<"|"<=") Add | Add

  Add        -> Mul ("+"|"-") Add | Mul
  Mul        -> Prim ("*"|"/") Mul | Prim
 

  Prim  -> Num | Id | Boolean
  Num   -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  Id    -> [A-Za-z]
  Boolean -> true | false
```