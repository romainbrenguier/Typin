let debug = false

type t = 
| Tvar of string
| Tcon of string * t array
| Tint of int
| TintVar of string * int

let base_type x = Tcon (x,[| |])
let [bool;string;char;float] = List.map base_type ["bool";"string";"char";"float"]

let var name = Tvar name

let _counter_var = ref 0
let new_var () = incr _counter_var; var ("'var_"^string_of_int !_counter_var)

let arrow a b = Tcon ("->", [| a ; b |])

let int a = Tint a
let int_var ?(plus=0) ?(minus=0) x = TintVar (x,plus-minus)

let add t x = match t with
  | Tint i -> Tint (i+x)
  | TintVar (s,i) -> TintVar (s,i+x)

let rec to_string = function
  | Tvar x -> x
  | Tcon ("->",a) -> "("^to_string a.(0) ^" -> "^to_string a.(1)^")"
  | Tcon (n,array) -> Array.fold_left (fun accu x -> accu^","^to_string x) (n^"(") array ^")"
  | Tint i -> "["^string_of_int i^"]"
  | TintVar (x,0) -> "["^x^"]"
  | TintVar (x,i) when i < 0 -> "["^x^"-"^string_of_int (-i)^"]"
  | TintVar (x,i) when i > 0 -> "["^x^"+"^string_of_int i^"]"

let print buf x = Printf.fprintf buf "%s" (to_string x)

exception Unify of t * t 
exception UnifyInt of int * int

module Unifier = Map.Make(String)
module TypeEnv = Map.Make(String)

let rec add_equation (tenv,unif) x y = 
  if debug then Printf.printf "adding %s = %a\n" x print y;
  if Unifier.mem x unif 
  then aux_unify (tenv,unif) y (Unifier.find x unif)
  else (tenv,(Unifier.add x y unif), y)

and aux_unify (tenv,accu) a b =
    if a = b then tenv,accu, a
    else match a, b with 
    | Tvar x, y | y, Tvar x -> add_equation (tenv,accu) x y
    | Tcon (xname,xterms), Tcon (yname,yterms) when xname = yname ->
      if Array.length xterms <> Array.length yterms 
      then raise (Unify (a,b))
      else
	let _,types,(tenv,accu) = 
	  Array.fold_left 
	    (fun (i,types,accu) t1 -> 
	      let tenv,accu,t = aux_unify accu t1 yterms.(i) in
	      (i+1, t :: types, (tenv,accu))
	    ) (0,[],(tenv,accu)) xterms
	in tenv, accu, Tcon(xname,Array.of_list (List.rev types))
    | Tint c, Tint d ->
      if c = d then tenv, accu, Tint c
      else raise (UnifyInt (c,d))

    | TintVar (x,i), Tint d
    | Tint d, TintVar (x,i) -> 
      Printf.printf "adding %s = %d\n" x (d-i);
      TypeEnv.add x (Tint (d-i)) tenv, accu, Tint d
    | TintVar (x,i), TintVar (y,j) -> 
      Printf.printf "adding %s = %s + %d\n" x y (j-i);
      TypeEnv.add x (TintVar (y,j-i)) tenv, accu, TintVar(y,j)

    | _ -> raise (Unify (a,b))

let unify a b = aux_unify (TypeEnv.empty,Unifier.empty) a b  

let merge_unifier tenv a b = 
  Unifier.fold 
    (fun k a (tenv,unif) -> let tenv, accu, _ = add_equation (tenv,unif) k a in (tenv,accu))
    a (tenv,b)

let rec substitute tenv unifier = function
  | Tvar x -> 
    (try 
       let f = Unifier.find x unifier in 
       substitute tenv unifier f
     with Not_found -> Tvar x)
  | Tcon (name,array) -> Tcon (name,Array.map (substitute tenv unifier) array)
  | Tint i -> Tint i    
  | TintVar(x,i) -> 
    try 
      let t = TypeEnv.find x tenv in
      add (substitute tenv unifier t) i
    with Not_found -> TintVar(x,i)

