open SimpleType
module Environment = Map.Make(String)

type t =
    Var of string
  | Fun of string * t
  | App of t * t
  | Let of string * t * t

exception UnboundValue of string

let infer env a = 
  let rec aux env tenv unifier = function
    | Var x -> 
      (try tenv, unifier, Environment.find x env
       with Not_found -> raise (UnboundValue x))

    | Fun (x, a) ->
      let tv1 = new_var() in
      let tenv,unif,t2 = aux (Environment.add x tv1 env) tenv unifier a in
      let t1 = substitute tenv unif tv1 in
      tenv, unif, arrow t1 t2
	
    | App (a1, a2) ->
      let tv1 = new_var() in 
      let tv2 = new_var() in
      let tenv,unifier,t1 = aux env tenv unifier a1 in
      let tenv,unifier,t2 = aux env tenv unifier a2 in
      let tenv,unifier,t1 = aux_unify (tenv,unifier) t1 (arrow tv1 tv2) in
      if debug then Printf.printf "befor app: %a\n" print (substitute tenv unifier tv2);
      let tenv,unifier,t2 = aux_unify (tenv,unifier) (substitute tenv unifier tv1) t2 in
      if debug then Printf.printf "after app: %a\n" print (substitute tenv unifier tv2);
      tenv, unifier, substitute tenv unifier tv2

    | Let (s, a1, a2) ->
      let tenv,unifier,t1 = aux env tenv unifier a1 in
      let env = Environment.add s t1 env in
      aux env tenv unifier a2

  in 
  
  let tenv,unifier,t = (aux env TypeEnv.empty Unifier.empty a) in
  t


let lexer = Genlex.make_lexer ["let";"=";"in";"fun";"->";"(";")"]

let parse stream =
  let rec parse_expr = parser 
  | [< 'Genlex.Kwd "("; e = parse_expr; 'Genlex.Kwd ")"; f = parse_after_expr e >] -> f
  | [< 'Genlex.Kwd "let"; 'Genlex.Ident a; 'Genlex.Kwd "="; e = parse_expr; 'Genlex.Kwd "in"; f = parse_expr >] -> Let (a,e,f)
  | [< 'Genlex.Kwd "fun"; 'Genlex.Ident a; 'Genlex.Kwd "->"; e = parse_expr >] -> Fun (a,e)
  | [< 'Genlex.Ident x; f = parse_after_expr (Var x) >] -> f
  and parse_after_expr accu = parser 
  | [< x = parse_expr; y = parse_after_expr (App(accu,x)) >] -> y
  | [< >] -> accu
  in

  let parse_decl = parser
  | [< 'Genlex.Kwd "let"; 'Genlex.Ident a; 'Genlex.Kwd "="; e = parse_expr >] -> Some (a,e)
  | [< >] -> None
  in
  let rec loop accu = match parse_decl stream with
    | None -> accu
    | Some x -> loop (x :: accu)
  in List.rev (loop [])
       

let main =
  let env = 
    List.fold_left
      (fun accu (s,t) ->
	Environment.add s t accu
      ) Environment.empty
      [
	"input",int 0;
	"and_gate",arrow (int_var "x") (arrow (int_var "x") (int_var "x"));
	"or_gate",arrow (int_var "x") (arrow (int_var "x") (int_var "x"));
	"not_gate",arrow (int_var "x") (int_var "x");
	"register",arrow (int_var "x") (int_var ~plus:1 "x");
	"cast",arrow (int_var "x") (int_var "y")
      ]
  in
  try
    let inch = open_in Sys.argv.(1) in
    let stream = Stream.of_channel inch in
    let token_stream = lexer stream in
    (try
      let prog = parse token_stream in
    (* Fun("lit" , App (Var "register", App(Var "not_gate",Var "lit"))) *)
      List.iter (fun (a,p) ->
	Printf.printf "%s : %a\n" a print (infer env p);
      ) prog
    with
    | e -> 	    
      print_endline "Remaining tokens:";
      print_endline (Common.remaining_tokens token_stream);
      raise e
    );
    close_in inch

  with 
  | Unify (a,b) -> 
    Printf.printf "Cannot unify: %s does not include %s\n" (to_string a) (to_string b)
  | UnifyInt (a,b) -> 
    Printf.printf "Cannot unify intervals: %s does not include %s\n" (string_of_int a) (string_of_int b)


