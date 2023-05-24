type expr =
    ExTrue
  | ExFalse
  | ExIf     of expr * expr * expr
  | ExZero 
  | ExSucc   of expr
  | ExPred   of expr
  | ExIsZero of expr

module Parser : sig 
    val parse : string -> expr
    val repl : (expr -> unit) -> unit
    exception ParseError of string
end 
=
struct
    (* Types *)

    exception ParseError of string

    type token =
        TkTrue of int * int
      | TkFalse of int * int
      | TkZero of int * int
      | TkIf of int * int
      | TkThen of int * int
      | TkElse of int * int
      | TkIsZero of int * int
      | TkPred of int * int
      | TkSucc of int * int

    (* Auxiliary functions *)

    let token_start (tk: token): int =
        match tk with
            TkTrue (start, _) -> start
          | TkFalse (start, _) -> start
          | TkZero (start, _) -> start
          | TkIf (start, _) -> start
          | TkThen (start, _) -> start
          | TkElse (start, _) -> start
          | TkIsZero (start, _) -> start
          | TkPred (start, _) -> start
          | TkSucc (start, _) -> start

    let token_size (tk: token): int =
        match tk with
            TkTrue (_, size) -> size 
          | TkFalse (_, size) -> size 
          | TkZero (_, size) -> size 
          | TkIf (_, size) -> size 
          | TkThen (_, size) -> size 
          | TkElse (_, size) -> size 
          | TkIsZero (_, size) -> size 
          | TkPred (_, size) -> size 
          | TkSucc (_, size) -> size 

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
            TkTrue _ -> "TkTrue"
          | TkFalse _ -> "TkFalse"
          | TkZero _ -> "TkZero"
          | TkIf _ -> "TkIf"
          | TkThen _ -> "TkThen"
          | TkElse _ -> "TkElse"
          | TkIsZero _ -> "TkIsZero"
          | TkPred _ -> "TkPred"
          | TkSucc _ -> "TkSucc"

    let debug_token_to_string(tk: token): string =
        match tk with
            TkTrue (st, si) -> "TkTrue" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"
          | TkFalse (st, si) -> "TkFalse" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"
          | TkZero (st, si) -> "TkZero" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"
          | TkIf (st, si) -> "TkIf" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"
          | TkThen (st, si) -> "TkThen" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"
          | TkElse (st, si) -> "TkElse" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"
          | TkIsZero (st, si) -> "TkIsZero" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"
          | TkPred (st, si) -> "TkPred" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"
          | TkSucc (st, si) -> "TkSucc" ^ "(" ^(string_of_int st) ^ ", " ^ (string_of_int si) ^ ")"

    let expr_res_to_string (expr_result: (expr * int)): string =
        let (e, _) = expr_result in expr_to_string e


    (* Lexical analysis *)

    (* This function skips whitespace then collects all the characters until if finds a whitespace (or EOF) *)
    let next_word(str: string) (index: int): (string * int) option =
      let str_len = String.length str in
      
      let rec skip_whitespace (i: int): int =
        if i < str_len then
          match String.get str i with
            (' ' | '\t' | '\r' | '\n' | '(' | ')') -> skip_whitespace (i + 1)
          | _ -> i
        else
          i in
      
      let rec collect_word (i: int): (string * int) option =
        if i < str_len then
          match String.get str i with
            (' ' | '\t' | '\r' | '\n' | '(' | ')') -> Some(("", i+1))
          | c -> match collect_word (i + 1) with
              Some((s, _i)) -> Some((String.cat (String.make 1 c) s, _i))
            | None -> Some((String.make 1 c, i+1))
        else
          None in
      
      collect_word (skip_whitespace index) 

    (* Gets the next token in the string *)
    let rec next_token(input: string) (index: int): (token * int) option =
        let word = next_word input index in 
        match word with
            Some((word, i)) -> 
                (match word with
                    "0" -> Some((TkZero (index, 1), i))
                  | "true" -> Some((TkTrue (index, 4), i))
                  | "false" -> Some((TkFalse (index, 5), i))
                  | "pred" -> Some((TkPred (index, 4), i))
                  | "succ" -> Some((TkSucc (index, 4), i))
                  | "iszero" -> Some((TkIsZero (index, 6), i))
                  | "is_zero" -> Some((TkIsZero (index, 7), i))
                  | "if" -> Some((TkIf (index, 2), i))
                  | "then" -> Some((TkThen (index, 4), i))
                  | "else" -> Some((TkElse (index, 4), i))
                  | _ -> raise (ParseError ("Invalid token ('" ^ word ^ "') at position " ^ (string_of_int (index+1)))))
          | None -> None 

    (* Executes next_token until the end of the string *)
    let rec tokenize(input: string) (index: int): token list = 
        match next_token input index with
            Some((token, i)) -> List.append [ token ] (tokenize input i)
          | None -> []


    (**********************
     * Syntactic analysis *
     **********************)

    let parse_internal(tokens: token list) (index: int): (expr * int) =
        let tokens_len = List.length tokens in 
        let rec aux (i: int): (expr * int) =
            if i < tokens_len then 
                match List.nth tokens i with 
                    TkTrue(st, si) -> (ExTrue, i + 1)
                  | TkFalse(st, si) -> (ExFalse, i + 1)
                  | TkZero(st, si) -> (ExZero, i + 1)
                  | TkPred(st, si) -> let (e, i) = aux (i+1) in (ExPred(e), i)
                  | TkSucc(st, si) -> let (e, i) = aux (i+1) in (ExSucc(e), i)
                  | TkIsZero(st, si) -> let (e, i) = aux (i+1) in (ExIsZero(e), i)
                  | TkIf(st, si) -> 
                        (* Parse e1 *)
                        let (e1, i) = aux (i+1) in 
                        (* Parse then *)
                        let () = if i < tokens_len then 
                            match List.nth tokens i with 
                                TkThen(st, si) -> ()
                              | tk -> raise (ParseError ("Expected to find 'then' at position " ^ (string_of_int ((token_start tk)+1)) ^ " (found " ^ (token_to_string tk) ^ " instead)"))
                        else 
                            raise (ParseError ("Expected to find 'then' at tokens[" ^ (string_of_int i) ^ "], instead found EOF")) in
                        (* Parse e2 *)
                        let (e2, i) = aux (i+1) in 
                        (* Parse else *)
                        let () = if i < tokens_len then 
                            match List.nth tokens i with 
                                TkElse(st, si) -> ()
                              | tk -> raise (ParseError ("Expected to find 'else' at position " ^ (string_of_int ((token_start tk)+1)) ^ " (found " ^ (token_to_string tk) ^ " instead)"))
                        else 
                            raise (ParseError ("Expected to find 'then' at tokens[" ^ (string_of_int i) ^ "], instead found EOF")) in
                        (* Parse e3 *)
                        let (e3, i) = aux (i+1) in 
                        (* Write result *)
                        (ExIf(e1, e2, e3), i) 
                  | token -> raise (ParseError ("Unexpected token (" ^ (token_to_string token) ^ ") at position " ^ (string_of_int ((token_start token) + 1)) ^ " (starting to construct an AST)"))
            else 
                raise (ParseError ("Unexpected EOF at tokens[" ^ (string_of_int i) ^ "]" )) in 
        aux index

    (* Interface functions *)

    let parse (str: string): expr =
        let (e, _) = parse_internal (tokenize str 0) 0 in e

    let repl (_on_repl_parse: expr -> unit): unit =
        try
            while true do 
                print_endline "";
                print_string "> ";
                let line = read_line () in

                try
                    let tokens = tokenize line 0 in 
                    print_endline (String.concat " " [ "tokens: ["; (String.concat ", " (List.map token_to_string tokens)); "]"]);

                    let parsed = parse_internal tokens 0 in
                    print_endline (String.cat "AST: " (expr_res_to_string parsed));

                    (* yield control to user *)
                    let (ast, _) = parsed in _on_repl_parse ast
                with 
                    ParseError msg -> print_endline ("ERROR: " ^ msg)
            done
                with End_of_file -> ()
end

let () = Parser.repl (fun _ -> ())
