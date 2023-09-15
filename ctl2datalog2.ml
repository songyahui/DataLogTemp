type terms = INT of int | STR of string | VAR of string       
(*Arithimetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of terms * terms
          | Lt of terms * terms
          | GtEq of terms * terms
          | LtEq of terms * terms
          | Eq of terms * terms
          | NEq of terms * terms
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure
          (* | Pos of terms *)

(*Arithimetic propositions formulae*)
type propositions = string * pure

type ctl = 
    Atom of propositions 
  | Neg of ctl 
  | Conj of ctl * ctl 
  | Disj of ctl * ctl 
  | Imply of ctl * ctl 
  | AX of ctl 
  | EX of ctl 
  | AF of ctl 
  | EF of ctl 
  | AG of ctl 
  | EG of ctl 
  | AU of ctl * ctl 
  | EU of ctl * ctl 

type basic_Type = Number | Symbol
type param  = string * basic_Type
type decl = string * (param list)

type relation = string * (terms list)
type head = relation
type body = Pos of relation | Neg of relation | Pure of pure 
type rule = head * (body list) 
type datalog = decl list * rule list

let rec expand_args (x:string list) = 
  match x with 
  [] -> ""
  | [x] -> x
  | x :: xs -> x ^ "," ^ (expand_args xs)


let string_of_term x =     
  match x with
  INT x -> Int.to_string x
| VAR x -> x 
| STR x -> "\"" ^ x ^ "\""

let string_of_param x =     
  match x with
  (n, Number) -> n ^ ":number"
| (s, Symbol) -> s ^ ":symbol" 

let string_of_relation (relation:relation) =
  match relation with
  (name,vars) -> let variables = expand_args (List.map string_of_term vars) in name ^ "(" ^ variables ^ ")"  

let rec string_of_pure (pure:pure) =
  match pure with 
  TRUE -> "true"
  | FALSE -> "false"
  | Gt (a,b) -> "(" ^ (string_of_term a) ^ " > " ^ (string_of_term b) ^ ")"
  | Lt (a,b) ->  "(" ^ (string_of_term a) ^ " < " ^ (string_of_term b) ^ ")"
  | GtEq (a,b) ->  "(" ^ (string_of_term a) ^ " >= " ^ (string_of_term b) ^ ")"
  | LtEq (a,b) ->  "(" ^ (string_of_term a) ^ " <= " ^ (string_of_term b) ^ ")"
  | Eq (a,b) -> "(" ^ (string_of_term a) ^ " == " ^ (string_of_term b) ^ ")"
  | NEq (a,b) -> "(" ^ (string_of_term a) ^ " != " ^ (string_of_term b) ^ ")"
  | PureOr(a,b) -> "(" ^ (string_of_pure a) ^ " || " ^ (string_of_pure b) ^ ")"
  | PureAnd(a,b) -> "(" ^ (string_of_pure a) ^ " && " ^ (string_of_pure b) ^ ")"
  | Neg a -> "!(" ^ (string_of_pure a) ^ ")"
  (* | Pos a -> string_of_term a *)


let string_of_bodies (bodies:body list) = 
  expand_args (List.map (fun body -> match body with
  Pos r -> string_of_relation r
  | Neg r -> "!"  ^ string_of_relation r
  | Pure p -> string_of_pure p ) bodies)


let string_of_decl (decl:decl) =
  match decl with
  name,args -> ".decl "^ name ^ "(" ^ (expand_args (List.map string_of_param args ))  ^ ");"

let string_of_decls = List.fold_left (fun acc decl -> acc ^ (if acc != "" then "\n" else "") ^ string_of_decl decl ) ""

let rec string_of_rules =  
  List.fold_left (fun acc (head,bodies) -> acc ^ (if acc != "" then "\n" else "") ^ string_of_relation head ^ " :- " ^ string_of_bodies bodies ^ ";" ) ""

let param_compare (a:param) (b:param) =
  match (a,b) with
  (a_name,a_type), (b_name,b_type) -> 
    let name_diff = String.compare a_name b_name in
    if name_diff == 0 then (
      match (a_type, b_type) with
      (Number, Number)
      | (Symbol, Symbol) -> 0
      | (Number,Symbol) -> 1
      | (Symbol,Number) -> -1
    ) else name_diff

let string_of_datalog (datalog:datalog) : string = 
  let (decls, rules) = datalog in 
  string_of_decls decls ^ string_of_rules rules

let rec infer_variables (pure:pure) =
  let get_variable_terms (x: terms) =
    match x with
    VAR x -> [VAR x]
    | _ -> [] in 
  match pure with 
  TRUE -> []
  | FALSE -> []
  | Gt (a,b) -> (get_variable_terms a) @ (get_variable_terms b) 
  | Lt (a,b) ->  (get_variable_terms a) @ (get_variable_terms b)
  | GtEq (a,b) ->  (get_variable_terms a) @ (get_variable_terms b)
  | LtEq (a,b) ->  (get_variable_terms a) @ (get_variable_terms b)
  | Eq (a,b) -> (get_variable_terms a) @ (get_variable_terms b)
  | NEq (a,b) -> (get_variable_terms a) @ (get_variable_terms b)
  | PureOr(a,b) -> (infer_variables a) @ (infer_variables b)
  | PureAnd(a,b) ->(infer_variables a) @ (infer_variables b)
  | Neg a -> infer_variables a
  (* | Pos a -> get_variable_terms a *)

let rec infer_params (pure:pure) : param list = 
  let get_variable_terms (x: terms) (y:basic_Type) =
    match x with
    VAR x -> [x,y]
    | _ -> [] in
  match pure with 
  TRUE -> []
  | FALSE -> []
  | Neg a -> infer_params a
  | Gt (a,b) -> (get_variable_terms a Number) @ (get_variable_terms b Number) 
  | Lt (a,b) ->  (get_variable_terms a Number) @ (get_variable_terms b Number)
  | GtEq (a,b) ->  (get_variable_terms a Number) @ (get_variable_terms b Number)
  | LtEq (a,b) ->  (get_variable_terms a Number) @ (get_variable_terms b Number)
  (* TODO *)
  | Eq (a,b) -> (get_variable_terms a Symbol) @ (get_variable_terms b Symbol)
  | NEq (a,b) -> (get_variable_terms a Symbol) @ (get_variable_terms b Symbol)
  | PureOr(a,b) -> (infer_params a) @ (infer_params b)
  | PureAnd(a,b) ->(infer_params a) @ (infer_params b)

  (*| Pos a -> get_variable_terms a *)



let translation (ctl:ctl) : datalog = 
  let get_params (declarations: decl list) =
    match declarations with
    [] -> []
    | x :: xs -> snd x in

  let get_args (rules: rule list) =
    match rules with
    | [] -> []
    | x::xs -> snd (fst x) in

  let process_args (args:terms list) =
    List.sort_uniq 
    (fun x y -> 
      match (x,y) with
      | (VAR x, VAR y) -> String.compare x y
      | _ -> failwith "Arguments should only be variables"
      )
    (List.filter (fun x -> match x with  VAR x -> true | _ -> false ) args ) in
  
  let rec translation_inner (ctl:ctl) : string * datalog =
    match ctl with 
    | Atom (pName, pure) -> 
      let vars = infer_variables pure in
      let params = infer_params pure in
      let decls = [(pName,params)] in 
      let head = (pName, vars) in 
      let bodies = [Pure pure] in 
      let rule = (head, bodies) in 
      pName,(decls, [rule])
    
    | Neg f -> 
      let fName,(declarations,rules) = translation_inner f in
        let newName = "NOT_" ^ fName in
        let fParams = get_params declarations in
        let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Neg (fName,fArgs) ]):: rules)

    | Conj (f1 , f2) -> 
        let x1,(declarations_f1,rules_f1) = translation_inner f1 in
        let x2,(declarations_f2,rules_f2) = translation_inner f2 in
        let f1_params = get_params declarations_f1 in
        let f2_params = get_params declarations_f2 in
        let f1_args = get_args rules_f1 in
        let f2_args = get_args rules_f2 in
        let decs = List.append declarations_f1 declarations_f2 in
        let ruls = List.append rules_f1 rules_f2 in
        let newParams = (List.sort_uniq param_compare (List.append f1_params f2_params)) in
        let newArgs = process_args (List.append f1_args f2_args) in
        let newName = x1 ^ "_AND_" ^ x2 in
        newName,( (newName,newParams) :: decs, ( (newName, newArgs) , [Pos(x1,f1_args); Pos(x2,f2_args)] )  :: ruls) 
      
    | Disj (f1,f2) ->
        let x1,(declarations_f1,rules_f1) = translation_inner f1 in
        let x2,(declarations_f2,rules_f2) = translation_inner f2 in
        let f1_params = get_params declarations_f1 in
        let f2_params = get_params declarations_f2 in
        let f1_args = get_args rules_f1 in
        let f2_args = get_args rules_f2 in
        let decs = List.append declarations_f1 declarations_f2 in
        let ruls = List.append rules_f1 rules_f2 in
        let newParams = (List.sort_uniq param_compare (List.append f1_params f2_params)) in
        let newArgs = process_args (List.append f1_args f2_args) in
        let newName = x1 ^ "_OR_" ^ x2 in
        newName,( (newName,newParams) :: decs, ( (newName, newArgs) , [Pos(x1,f1_args)] ) :: ( (newName, newArgs) , [Pos(x2,f2_args)] )  :: ruls) 
      
    | Imply (f1,f2) -> 
        let x1,(declarations_f1,rules_f1) = translation_inner f1 in
        let x2,(declarations_f2,rules_f2) = translation_inner f2 in
        let f1_params = get_params declarations_f1 in
        let f2_params = get_params declarations_f2 in
        let f1_args = get_args rules_f1 in
        let f2_args = get_args rules_f2 in
        let decs = List.append declarations_f1 declarations_f2 in
        let ruls = List.append rules_f1 rules_f2 in
        let newParams = (List.sort_uniq param_compare (List.append f1_params f2_params)) in
        let newArgs = process_args (List.append f1_args f2_args) in
        let newName = x1 ^ "_imply_" ^ x2 in
        newName,( (newName,newParams) :: decs, ( (newName, newArgs) , [Pos(x1,f1_args); Neg(x2,f2_args)] )  :: ruls) 

    (* Primary CTL Encoding *)
    | EX f ->   
      (* TODO *)  
      let fName,(declarations,rules) = translation_inner f in
        let newName = "EX_" ^ fName in
        let fParams = get_params declarations in
        let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Pos (fName,fArgs) ]):: rules)
    
    | AF f ->     
      (* TODO *)
      let x,(declarations,rules) = translation_inner f in
      "AF_" ^ x,(declarations,rules)
    
    | EU (f1,f2)->
      (* TODO *) 
      let x1,(declarations_f1,rules_f1) = translation_inner f1 in
      let x2,(declarations_f2,rules_f2) = translation_inner f2 in
      let f1_params = get_params declarations_f1 in
      let f2_params = get_params declarations_f2 in
      let f1_args = get_args rules_f1 in
      let f2_args = get_args rules_f2 in
      let decs = List.append declarations_f1 declarations_f2 in
      let ruls = List.append rules_f1 rules_f2 in
      let newParams = (List.sort_uniq param_compare (List.append f1_params f2_params)) in
      let newArgs = process_args (List.append f1_args f2_args) in
        x1 ^ "_EU_" ^ x2, (List.append declarations_f1 declarations_f2, List.append rules_f1 rules_f2)

    | EF f ->     
      let fName,(declarations,rules) = translation_inner f in
      (* TODO *)
        let newName = "EF_" ^ fName in
        let fParams = get_params declarations in
        let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Pos (fName,fArgs) ]):: rules) 
    
      
    (* Derivative rules *)
    | AX f ->
      (* AX f = !EX !f *)     
      let fName,(declarations,rules) = translation_inner  (EX (Neg f)) in
      let prefixLen = (List.fold_right (+) (List.map String.length [ "EX_"; "NOT_"]) 0) in
      let newName = "AX_" ^  (String.sub fName prefixLen (String.length fName - prefixLen)) in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Neg (fName,fArgs) ]):: rules)
    
    | AG f ->
      (* AG f  = !EF !f *)     
      let fName,(declarations,rules) = translation_inner (EF (Neg f)) in
      let prefixLen = (List.fold_right (+) (List.map String.length [ "EF_"; "NOT_"]) 0) in
      let newName = "AG_" ^  (String.sub fName prefixLen (String.length fName - prefixLen)) in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Neg (fName,fArgs) ]):: rules)

    | EG f ->
      (* EG f = !AF !f *)     
      let fName,(declarations,rules) = translation_inner (AF (Neg f)) in
      let prefixLen = (List.fold_right (+) (List.map String.length [ "AF_"; "NOT_"]) 0) in
      let newName = "EG_" ^  (String.sub fName prefixLen (String.length fName - prefixLen)) in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Neg (fName,fArgs) ]):: rules)

    | AU (f1,f2) ->
      (* f1 AU f2 = not (!f2 EU (!f1 and !f2) ) and AF f2 *)
      let x1,_ = translation_inner f1 in
      let x2,_ = translation_inner f2 in
      let eu = EU((Neg f2),(Conj((Neg f1),(Neg f2)))) in
      let fName,(declarations,rules) = translation_inner (Conj((AF f2),(Neg eu))) in
      let newName = x1 ^ "_AU_" ^ x2 in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Pos (fName,fArgs) ]):: rules)

    in let (decs,rules) = snd (translation_inner ctl) in
    let defaultDecs = [ 
      ("state", [ ("x", Number)]);
      ("flow", [ ("x", Number); ("y", Number) ]);
      ("transFlow", [ ("x", Number); ("y", Number) ]) 
      ] in
    let defaultRules = [ 
      ("transFlow", [VAR "x"; VAR "y"] ), [ Pos ("flow", [VAR "x"; VAR "y"]) ] ;
      ("transFlow", [VAR "x"; VAR "z"] ), [ Pos ("flow", [VAR "x"; VAR "y"]); Pos ("flow", [VAR "x"; VAR "y"]) ] 
      ] in
      (defaultDecs @ List.rev decs, defaultRules @ List.rev rules)

let tests  = 
  [
    Atom("xIsPos", (Gt(VAR "x", INT 0)));
    EX(Atom("y", (Gt(VAR "X", INT 0))));  
  ] 

let main = 
  List.map (fun item -> print_endline (string_of_datalog (translation item))) tests