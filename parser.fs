//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a tuple containing 
// 3 values:  
//
//   (result, msg, program)
//
// where result is true or false (legal or not legal), 
// msg is a success or syntax error message, and program
// is a list of instructions if parsing was successful.
//
// <<Harsh Devprakash Gupta>>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #05
//

#light

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // These are debug routines that output the tokens, or
  // program, respectively.  They are written so you can
  // inject these into a pipeline to output the current 
  // state of the tokens or program.
  //
  let private __outputTokens (tokens, program) =
    printfn "Tokens: %A" tokens
    (tokens, program)

  let private __outputProgram (tokens, program) =
    printfn "Program: %A" program
    (tokens, program)


  //
  // matchToken
  //
  let private matchToken expected_token (tokens, program) =
    let (token, _) = List.head tokens
    //
    // if the token matches the expected token, keep parsing by
    // returning the rest of the tokens.  Otherwise throw an
    // exception because there's a syntax error, effectively 
    // stopping compilation:
    //
    if expected_token = token then  
      (List.tail tokens, program)
    else
      failwith ("expecting " + (string expected_token) + ", but found " + (string token))
      
  // <empty>    
  let private empty (tokens, program) = 
    let (T1, P1) = matchToken lexer.Tokens.Semicolon (tokens, program)
    (T1, ["$EMPTY"]::P1)
  
  //<vardecl>
  let private vardecl (tokens, program) =
    let (T1, P1) = matchToken lexer.Tokens.Int (tokens, program)
    let (t1, p1) = List.head T1
    let (T2, P2) = matchToken lexer.Tokens.ID (T1, P1)
    let (T3, P3) = matchToken lexer.Tokens.Semicolon (T2, P2)
    (T3, ["$DECL"; p1]::P3)
    
  //<input>
  let private input (tokens, program) = 
    let (T1, P1) = matchToken lexer.Tokens.Cin (tokens, program)
    let (T2, P2) = matchToken lexer.Tokens.Input (T1, P1)
    let (t1, p1) = List.head T2
    let (T3, P3) = matchToken lexer.Tokens.ID (T2, P2)
    let (T4, P4) = matchToken lexer.Tokens.Semicolon (T3, P3)
    (T4, ["$INPUT"; p1]::P4)
  
  // <expr-value>
  let expr_value (tokens, program) =
    let (nextToken, _) = List.head tokens
    if nextToken = lexer.Tokens.ID then matchToken lexer.Tokens.ID (tokens, program)
    else if nextToken = lexer.Tokens.Int_Literal then matchToken lexer.Tokens.Int_Literal (tokens, program)
    else if nextToken = lexer.Tokens.Str_Literal then matchToken lexer.Tokens.Str_Literal (tokens, program)
    else if nextToken = lexer.Tokens.Bool_Literal then matchToken lexer.Tokens.Bool_Literal (tokens, program)
    else
      failwith ("expecting identifier or literal, but found " + (string tokens))
  
  // <output_value>
  let output_value (tokens, program) =
    let (nextToken, _) = List.head tokens
    if nextToken = lexer.Tokens.Endl then
      let (T1, P1) = matchToken lexer.Tokens.Endl (tokens, program)
      let (t1 , p1) = List.head tokens
      (T1, ["$OUTPUT"; (string t1); p1] :: P1)
    else
      let (T1, P1) = expr_value (tokens, program)
      let (t1 , p1) = List.head tokens
      (T1, ["$OUTPUT"; (string t1); p1] :: P1)
  
  
  //<output>
  let private output (tokens, program) = 
    (tokens, program)
    |> matchToken lexer.Tokens.Cout
    |> matchToken lexer.Tokens.Output
    |> output_value
    |> matchToken lexer.Tokens.Semicolon
    
  // <assignment>
  let private expr_op (tokens, program) =
    let (nextToken,_) = List.head tokens
    if nextToken = lexer.Tokens.Plus then matchToken lexer.Tokens.Plus (tokens, program)
    else if nextToken = lexer.Tokens.Minus then matchToken lexer.Tokens.Minus (tokens, program)
    else if nextToken = lexer.Tokens.Times then matchToken lexer.Tokens.Times (tokens, program)
    else if nextToken = lexer.Tokens.Divide then matchToken lexer.Tokens.Divide (tokens, program)
    else if nextToken = lexer.Tokens.Power then matchToken lexer.Tokens.Power (tokens, program)
    else if nextToken = lexer.Tokens.LT then matchToken lexer.Tokens.LT (tokens, program)
    else if nextToken = lexer.Tokens.LTE then matchToken lexer.Tokens.LTE (tokens, program)
    else if nextToken = lexer.Tokens.GT then matchToken lexer.Tokens.GT (tokens, program)
    else if nextToken = lexer.Tokens.GTE then matchToken lexer.Tokens.GTE (tokens, program)
    else if nextToken = lexer.Tokens.EQ then matchToken lexer.Tokens.EQ (tokens, program)
    else if nextToken = lexer.Tokens.NE then matchToken lexer.Tokens.NE (tokens, program)
    else matchToken lexer.Tokens.Unknown (tokens, program)
    
  
  // <_condition> 
  let private _condition (tokens, program) =
    let (T1, P1) = expr_value (tokens,program)
    let (cond, _) = List.head T1 
    if cond = lexer.Tokens.Semicolon || cond = lexer.Tokens.CloseParen then (T1,P1)
    else
       let (T2, P2) = expr_op (T1, P1)
       let (T3, P3) = expr_value (T2, P2)
       (T3,P3)
    
  // <expr>
  let private expr (tokens, program) =
    let (nextToken,_) = List.head tokens
    if nextToken = lexer.Tokens.Int_Literal then _condition (tokens, program)
    else if nextToken = lexer.Tokens.Bool_Literal then _condition (tokens, program)
    else if nextToken = lexer.Tokens.Str_Literal then _condition (tokens, program)  
    else _condition (tokens, program)

    
  // <condition> 
  let private condition (tokens, program) =
    (tokens, program)
    |> expr
    
    
  // <assignment>
  let private assignment (tokens, program) = 
    let (T1, P1) = matchToken lexer.Tokens.ID (tokens, program)
    let (t1, p1) = List.item 1 T1
    let (t2, p2) = List.item 2 T1
    let (T2, P2) = matchToken lexer.Tokens.Assign (T1, P1)
    let (T3, P3) = expr (T2, P2)
    let (t3, p3) = List.head tokens
    if t2 = lexer.Tokens.Semicolon then
       let print = ["$ASSIGN"; p3; (string t1); p1]
       (T3, print::P3)
    else
       let (t4, p4) = List.item 3 T1
       let print = ["$ASSIGN"; p3; (string t1); p1; p2; (string t4); p4]
       (T3, print::P3)

    
  // <stmt>
  let rec private stmt (tokens, program) = 
    let (nextToken,_) = List.head tokens
    if nextToken = lexer.Tokens.Semicolon then empty (tokens, program)
    else if nextToken = lexer.Tokens.Int then vardecl (tokens, program)
    else if nextToken = lexer.Tokens.Cin then input (tokens, program)
    else if nextToken = lexer.Tokens.Cout then output (tokens, program)
    else if nextToken = lexer.Tokens.ID then 
      let (T1, P1) = assignment (tokens, program)
      let (T2, P2) = matchToken lexer.Tokens.Semicolon (T1,P1)
      (T2, P2)
    else if nextToken = lexer.Tokens.If then
      let (T1, P1) = ifstmt (tokens, program)
      (T1, P1)
    else
      failwith ("expecting statement, but found " + (string nextToken))

  and private ifstmt (tokens,program) = 
    let (t1, p1) = List.item 2 tokens
    let (T1, P1) = matchToken lexer.Tokens.If (tokens, program)
    let (T2, P2) = matchToken lexer.Tokens.OpenParen (T1, P1)
    let (T3, P3) = condition (T2, P2)
    let (t3, p3) = List.item 3 tokens
    if p3 = ")" then
      let print = ["$IF"; (string t1); p1]
      let (T4, P4) = matchToken lexer.Tokens.CloseParen (T3, print::P3)
      let (T5, P5) = then_part (T4, P4)
      let (T6, P6) = else_part (T5, P5)
      (T6, P6)
    else
      let (t4, p4) = List.item 4 tokens
      let print = ["$IF"; (string t1); p1; p3; (string t4); p4]
      let (T4, P4) = matchToken lexer.Tokens.CloseParen (T3, print::P3)
      let (T5, P5) = then_part (T4, P4)
      let (T6, P6) = else_part (T5, P5)
      (T6, P6)
    
  and private then_part (tokens, program) = 
    stmt (tokens, program)
    
  and private else_part (tokens,program) =
    let (nextToken,_) = List.head tokens
    if nextToken = lexer.Tokens.Else then 
      let (T1, P1) = matchToken lexer.Tokens.Else (tokens, program)
      let (T2, P2) = stmt (T1, P1)
      (T2, P2)
    else (tokens,["$EMPTY"]::program)
    
    

    
  // <morestmts>
  let rec private morestmts (tokens, program) =
    let (nextToken,_) = List.head tokens
    if nextToken = lexer.Tokens.Cout || nextToken = lexer.Tokens.ID || nextToken = lexer.Tokens.If || nextToken = lexer.Tokens.Semicolon || nextToken = lexer.Tokens.Int || nextToken = lexer.Tokens.Cin then
      let (T1,P1) = stmt (tokens, program)
      let (T2,P2) = morestmts (T1, P1)
      (T2, P2)
    else
      (tokens, program)

  
  // <stmts>
  let private stmts (tokens, program) =
    (tokens,program)
    |> stmt
    |> morestmts 
    
  
    
  
  
  // 

          
  //
  // simpleC
  // 
  let private simpleC (tokens, program) = 
    //matchToken lexer.Tokens.EOF (tokens, program)
    //let(T1, P1) = matchToken lexer.Tokens.Void (tokens, program)
    //let(T2, P2) = matchToken lexer.Tokens.Main (T1, P1)
    //let(T3, P3) = matchToken lexer.Tokens.OpenParen (T2, P2)
    //let(T4, P4) = matchToken lexer.Tokens.CloseParen (T3, P3)
    //let(T5, P5) = matchToken lexer.Tokens.OpenBrace (T4, P4)
    ////let (T6, P6) = stmts (T5,P5)
    //let(T7, P7) = matchToken lexer.Tokens.CloseBrace (T5, P5)
    //let(T8, P8) = matchToken lexer.Tokens.EOF (T7, P7)
    //(T8, P8)
    //
    (tokens, program)
    |> matchToken lexer.Tokens.Void
    |> matchToken lexer.Tokens.Main
    |> matchToken lexer.Tokens.OpenParen
    |> matchToken lexer.Tokens.CloseParen
    |> matchToken lexer.Tokens.OpenBrace
    |> stmts
    |> matchToken lexer.Tokens.CloseBrace
    |> matchToken lexer.Tokens.EOF


  
  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // a tuple containing 3 values:  
  //
  //   (result, msg, program)
  //
  // where result is true or false (legal or not legal), 
  // msg is a success or syntax error message, and program
  // is a list of instructions if parsing was successful.
  //
  let parse tokens = 
    try
      let (_, program) = simpleC (tokens, [])
      (true, "success", List.rev program)
    with 
      | ex -> (false, ex.Message, [])
