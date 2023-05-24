(**************
 * Data Types *
 **************)

type expr =
    ExTrue
  | ExFalse
  | ExIf     of expr * expr * expr
  | ExZero 
  | ExSucc   of expr
  | ExPred   of expr
  | ExIsZero of expr

type token =
    TkTrue
  | TkFalse
  | TkZero
  | TkIf
  | TkThen
  | TkElse
  | TkIsZero
  | TkPred
  | TkSucc

(* Two item tuple *)
type ('a, 'b) pair
  = Pair of 'a * 'b

(* Rust-like option enum *)
type 'a option =
    Some of 'a
  | None

(* Data structure for function that iterate over lists. 
   - Present -> success
   - Invalid -> fail 
   - EOF -> The caller decides whether it's a success or a failure *)
type 'a res =
    Present of 'a
  | Invalid
  | EOF


(************************
 * Conversion to string *
 ************************)

let rec expr_to_string(e: expr): string =
    match e with
        ExTrue -> "ExTrue"
      | ExFalse -> "ExFalse"
      | ExZero -> "ExZero"
      | ExSucc(e1) -> String.concat "" [ "ExSucc("; (expr_to_string e1); ")" ] 
      | ExPred(e1) -> String.concat "" [ "ExPred("; (expr_to_string e1); ")" ] 
      | ExIsZero(e1) -> String.concat "" [ "ExIsZero("; (expr_to_string e1); ")" ] 
      | ExIf(e1, e2, e3) -> String.concat "" [ "ExIf("; expr_to_string e1; ", "; expr_to_string e2; ", "; expr_to_string e3; ")" ] 

let token_to_string(tk: token): string =
    match tk with
        TkTrue -> "TkTrue"
      | TkFalse -> "TkFalse"
      | TkZero -> "TkZero"
      | TkIf -> "TkIf"
      | TkThen -> "TkThen"
      | TkElse -> "TkElse"
      | TkIsZero -> "TkIsZero"
      | TkPred -> "TkPred"
      | TkSucc -> "TkSucc"

let expr_res_to_string (expr_result: (expr, int) pair res): string =
    match expr_result with
        Present(Pair(e, i)) -> String.concat "" [ expr_to_string e; " (next token at "; string_of_int i; ")" ]
      | Invalid -> "Invalid"
      | EOF -> "EOF"


(********************
 * Lexical analysis *
 ********************)

(* This function skips whitespace then collects all the characters until if finds a whitespace (or EOF) *)
let next_word(str: string) (index: int): (string, int) pair option =
  let str_len = String.length str in
  
  let rec skip_whitespace (i: int): int =
    if i < str_len then
      match String.get str i with
        (' ' | '\t' | '\r' | '\n' | '(' | ')') -> skip_whitespace (i + 1)
      | _ -> i
    else
      i in
  
  let rec collect_word (i: int): (string, int) pair option =
    if i < str_len then
      match String.get str i with
        (' ' | '\t' | '\r' | '\n' | '(' | ')') -> Some(Pair("", i+1))
      | c -> match collect_word (i + 1) with
          Some(Pair(s, _i)) -> Some(Pair(String.cat (String.make 1 c) s, _i))
        | None -> Some(Pair(String.make 1 c, i+1))
    else
      None in
  
  collect_word (skip_whitespace index) 

(* Gets the next token in the string *)
let rec next_token(input: string) (index: int): (token, int) pair res =
    let word = next_word input index in 
    match word with
        Some(Pair(word, i)) -> 
            (match word with
                "0" -> Present(Pair(TkZero, i))
              | "true" -> Present(Pair(TkTrue, i))
              | "false" -> Present(Pair(TkFalse, i))
              | "pred" -> Present(Pair(TkPred, i))
              | "succ" -> Present(Pair(TkSucc, i))
              | ("iszero" | "is_zero") -> Present(Pair(TkIsZero, i))
              | "if" -> Present(Pair(TkIf, i))
              | "then" -> Present(Pair(TkThen, i))
              | "else" -> Present(Pair(TkElse, i))
              | _ -> Invalid) 
      | None -> EOF 

(* Executes next_token until the end of the string *)
let rec tokenize(input: string) (index: int): token list = 
    match next_token input index with
        Present(Pair(token, i)) -> List.append [ token ] (tokenize input i)
      | Invalid -> []
      | EOF -> []


(**********************
 * Syntactic analysis *
 **********************)

let parse(tokens: token list) (index: int): (expr, int) pair res =
    let tokens_len = List.length tokens in 

    let rec aux (i: int): (expr, int) pair res =
        if i < tokens_len then 
            match List.nth tokens i with 
                TkTrue -> Present(Pair(ExTrue, i + 1))
              | TkFalse -> Present(Pair(ExFalse, i + 1))
              | TkZero -> Present(Pair(ExZero, i + 1))
              | TkPred -> (match aux (i + 1) with Present(Pair(e, _i)) -> Present(Pair(ExPred(e), _i)) | _ -> Invalid)
              | TkSucc -> (match aux (i + 1) with Present(Pair(e, _i)) -> Present(Pair(ExSucc(e), _i)) | _ -> Invalid)
              | TkIsZero -> (match aux (i + 1) with Present(Pair(e, _i)) -> Present(Pair(ExIsZero(e), _i)) | _ -> Invalid)
              | TkIf -> (match aux (i + 1) with 
                    Present(Pair(e1, i)) -> (
                        if i < tokens_len then match List.nth tokens i with 
                            TkThen -> (match aux (i + 1) with 
                                Present(Pair(e2, i)) -> (
                                    if i < tokens_len then match List.nth tokens i with 
                                        TkElse -> (match aux (i + 1) with 
                                            Present(Pair(e3, i)) -> Present(Pair(ExIf(e1, e2, e3), i))
                                          | _ -> Invalid) 
                                      | _ -> Invalid
                                    else 
                                        Invalid) 
                              | _ -> Invalid) 
                          | _ -> Invalid
                        else 
                            Invalid) 
                  | _ -> Invalid)
              | _ -> Invalid
                        else 
            EOF in 

    aux index


(*********************
 * Auxiliar function *
 *********************)

exception CouldNotParse

let easy_parse (str: string): expr =
    match parse (tokenize str 0) 0 with
        Present(Pair(e, i)) -> e
      | _ -> raise (CouldNotParse) 


(**************
 * Parameters *
 **************)

let ignore_prompt = false 

let on_repl_parse (ast: expr) =
    print_endline "asd asd "


(********
 * REPL *
 ********)

    (* prompt *)
let () = if ignore_prompt then () else (print_endline "";
    print_endline "### What the implemented language looks like #####################################";
    print_endline "#                                                                                #";
    print_endline "#   [ Examples ]                                                                 #";
    print_endline "#                                                                                #";
    print_endline "#   > true                                                                       #";
    print_endline "#   > false                                                                      #";
    print_endline "#   > 0                                                                          #";
    print_endline "#   > pred e                                                                     #";
    print_endline "#   > succ e                                                                     #";
    print_endline "#   > iszero e                                                                   #";
    print_endline "#   > is_zero e                                                                  #";
    print_endline "#   > if e1 then e2 else e3                                                      #";
    print_endline "#                                                                                #";
    print_endline "#   [ Parenthesis can be used, but are completely ignored by the lexer ]         #";
    print_endline "#                                                                                #";
    print_endline "#   > pred(e)                                                                    #";
    print_endline "#   > succ(e)                                                                    #";
    print_endline "#   > iszero(e)                                                                  #";
    print_endline "#   > is_zero(e)                                                                 #";
    print_endline "#   > if (e1) then (e2) else (e3)                                                #";
    print_endline "#                                                                                #";
    print_endline "##################################################################################";
    print_endline "";
    print_endline "";
    print_endline "### How to use the parse in your code ############################################";
    print_endline "#                                                                                #";
    print_endline "#   let ast = easy_parse \"<your expression here>\"                                #";
    print_endline "#                                                                                #";
    print_endline "#   let () = print_endline (expr_to_string (easy_parse \"is_zero 0\"))             #";
    print_endline "#                                                                                #";
    print_endline "##################################################################################";
    print_endline "";
    print_endline "";
    print_endline "### REPL Examples ################################################################";
    print_endline "#                                                                                #";
    print_endline "#   > is_zero 0                                                                  #";
    print_endline "#   tokens: [ TkIsZero, TkZero ]                                                 #";
    print_endline "#   AST: ExIsZero(ExZero) (next token at 2)                                      #";
    print_endline "#                                                                                #";
    print_endline "#   > pred succ 0                                                                #";
    print_endline "#   tokens: [ TkPred, TkSucc, TkZero ]                                           #";
    print_endline "#   AST: ExPred(ExSucc(ExZero)) (next token at 3)                                #";
    print_endline "#                                                                                #";
    print_endline "#   > if (iszero 0) then (0) else (succ 0)                                       #";
    print_endline "#   tokens: [ TkIf, TkIsZero, TkZero, TkThen, TkZero, TkElse, TkSucc, TkZero ]   #";
    print_endline "#   AST: ExIf(ExIsZero(ExZero), ExZero, ExSucc(ExZero)) (next token at 2)        #";
    print_endline "#                                                                                #";
    print_endline "##################################################################################";
    print_endline "";
    print_endline " [Note] The output of the lexical analysis, as well as the last token used to";
    print_endline "        build the AST, are displayed for debugging purposes";
    print_endline "";
    (flush stdout))

let () =
    try
        while true do 
            (* read *)
            print_endline "";
            print_string "> ";
            let line = read_line () in
            (* eval *)
            let tokens = tokenize line 0 in 
            let parsed = parse tokens 0 in
            (* print *)
            print_endline (String.concat " " [ "tokens: ["; (String.concat ", " (List.map token_to_string tokens)); "]"]);
            print_endline (String.cat "AST: " (expr_res_to_string parsed));
            (* yield control to user *)
            match parsed with 
                Present(Pair(ast, _)) -> on_repl_parse ast | _ -> ()
        done
            with End_of_file -> ()


