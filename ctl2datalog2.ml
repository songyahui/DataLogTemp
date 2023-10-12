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

let rec expand_args (sep: string) (x:string list) = 
  match x with 
  [] -> ""
  | [x] -> x
  | x :: xs -> x ^ sep ^ (expand_args sep xs)


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
  (name,vars) -> let variables = expand_args "," (List.map string_of_term vars) in name ^ "(" ^ variables ^ ")"  

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
  expand_args ", " (List.map (fun body -> match body with
  Pos r -> string_of_relation r
  | Neg r -> "!"  ^ string_of_relation r
  | Pure p -> string_of_pure p ) bodies)


let string_of_decl (decl:decl) =
  match decl with
  name,args -> ".decl "^ name ^ "(" ^ (expand_args "," (List.map string_of_param args ))  ^ ");"

let string_of_decls = List.fold_left (fun acc decl -> acc ^ (if acc != "" then "\n" else "") ^ string_of_decl decl ) ""

let rec string_of_rules =  
  List.fold_left (fun acc (head,bodies) -> acc ^ (if acc != "" then "\n" else "") ^ string_of_relation head ^ " :- " ^ string_of_bodies bodies ^ "." ) ""

let param_compare (a:param) (b:param) =
  match (a,b) with
  (a_name,a_type), (b_name,b_type) -> 
    let nameDiff = String.compare a_name b_name in
    if nameDiff == 0 then (
      match (a_type, b_type) with
      (Number, Number)
      | (Symbol, Symbol) -> 0
      | (Number,Symbol) -> 1
      | (Symbol,Number) -> -1
    ) else nameDiff

let term_compare (a:terms) (b:terms) =
  match (a,b) with
  | VAR x, VAR y -> String.compare x y
  | _ ,_ -> 0

let string_of_datalog (datalog:datalog) : string = 
  let (decls, rules) = datalog in 
  string_of_decls decls ^ string_of_rules rules

let rec infer_variables (pure:pure) =
  let get_variable_terms (x: terms) =
    match x with
    VAR x -> [VAR x]
    | _ -> [] in 
 let x = match pure with 
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
  in List.sort_uniq term_compare x
  (* | Pos a -> get_variable_terms a *)

let rec infer_params (pure:pure) : param list = 
  let get_variable_terms (x: terms) (y:basic_Type) =
    match x with
    VAR x -> [x,y]
    | _ -> [] in
  let x = match pure with 
  TRUE -> []
  | FALSE -> []
  | Neg a -> infer_params a
  | Gt (a,b) -> (get_variable_terms a Number) @ (get_variable_terms b Number) 
  | Lt (a,b) ->  (get_variable_terms a Number) @ (get_variable_terms b Number)
  | GtEq (a,b) ->  (get_variable_terms a Number) @ (get_variable_terms b Number)
  | LtEq (a,b) ->  (get_variable_terms a Number) @ (get_variable_terms b Number)
  (* TODO *)
  | Eq (a,b) -> (get_variable_terms a Number) @ (get_variable_terms b Number)
  | NEq (a,b) -> (get_variable_terms a Number) @ (get_variable_terms b Number)
  | PureOr(a,b) -> (infer_params a) @ (infer_params b)
  | PureAnd(a,b) ->(infer_params a) @ (infer_params b)
  in List.sort_uniq param_compare x
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

    let processPair f1 f2 name (construct_rules: relation -> relation -> relation -> rule list) =
      let x1,(f1Declarations,f1Rules) = translation_inner f1 in
      let x2,(f2Declarations,f2Rules) = translation_inner f2 in
      let f1Params = get_params f1Declarations in
      let f2Params = get_params f2Declarations in
      let f1Args = get_args f1Rules in
      let f2Args = get_args f2Rules in
      let decs = List.append f1Declarations f2Declarations in
      let ruls = List.append f1Rules f2Rules in
      let newParams = (List.sort_uniq param_compare (List.append f1Params f2Params)) in
      let newArgs = process_args (List.append f1Args f2Args) in
      let newName = name x1 x2 in 
      newName, ( (newName,newParams) :: decs, ( (construct_rules (newName, newArgs) (x1,f1Args) (x2,f2Args))  @ ruls)) 
    in

    match ctl with 
    | Atom (pName, pure) -> 
      let vars = VAR "loc" :: infer_variables pure in
      let params =  ("loc" , Number) :: infer_params pure in
      pName,([(pName,params)], [  ((pName, vars), [Pos("state", [VAR "loc"]) ;Pure pure]) ])
    
    | Neg f -> 
      let fName,(declarations,rules) = translation_inner f in
        let newName = "NOT_" ^ fName in
        let fParams = get_params declarations in
        let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Pos("state", [VAR "loc"]) ;Neg (fName,fArgs) ]):: rules)

    | Conj (f1 , f2) -> 
        processPair f1 f2 
        (fun x1 x2 ->  x1 ^ "_AND_" ^ x2) 
        (fun (newName,newArgs) (x1,f1Args) (x2,f2Args) -> [( (newName, newArgs) , [Pos(x1,f1Args); Pos(x2,f2Args)] ) ])
      
    | Disj (f1,f2) ->
        processPair f1 f2 
        (fun x1 x2 ->  x1 ^ "_OR_" ^ x2) 
        (fun (newName,newArgs) (x1,f1Args) (x2,f2Args) -> [ ( (newName, newArgs) , [Pos(x1,f1Args)] ) ; ( (newName, newArgs) , [Pos(x2,f2Args)] ) ]);
      
    | Imply (f1,f2) -> 
        processPair f1 f2 
        (fun x1 x2 ->  x1 ^ "_IMPLY_" ^ x2) 
        (fun (newName,newArgs) (x1,f1Args) (x2,f2Args) -> 
        [ ( (newName, newArgs) , [Pos(x2,f2Args)]);
          ( (newName, newArgs) , [Neg(x1,f1Args)] )
        ])

    (* Primary CTL Encoding *)
    (* The idea behind this encoding is state encoding is to reuse the previous name when a transition is needed *)
    | EX f ->   
      (* TODO *)  
      let fName,(declarations,rules) = translation_inner f in
        let newName = "EX_" ^ fName in
        let fParams = get_params declarations in
        let fArgs = get_args rules in
        let arg = VAR "tempOne" in
        let firstArg, fNewArgs = match fArgs with
          [] -> failwith "confused"
          | x :: xs -> x, arg :: xs in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [  Pos("flow", [firstArg;arg] );    Pos (fName,fNewArgs) ]):: rules)
    | EF f ->     
      let fName,(declarations,rules) = translation_inner f in
      (* TODO *)
        let newName = "EF_" ^ fName in
        let fParams = get_params declarations in
        let fArgs = get_args rules in
        let arg = VAR "tempOne" in
        let firstArg, fNewArgs = match fArgs with
          [] -> failwith "confused"
          | x :: xs -> x, arg :: xs in
        newName,(  (newName,fParams) :: declarations, 
        [
          ( (newName,fArgs), [Pos (fName,fArgs) ]);
          ( (newName,fArgs), [Pos("flow",[firstArg;arg]); Pos(newName,fNewArgs)     ] )

        ]@ rules) 
    
    | AF f ->     
      (* Per Gottlob et al.

      AF_P_T(x,z) :- AF_P_T(x,y), !P(y), flow(y,z);
      AF_P_T(x,y) :- !P(x), flow(x,y);
      AF_P_S(x) :- AF_P_T(x,x);
      AF_P_S(x) :- !P(x), flow(x,y), S(y);
      AF_P(x) :- state(x) !AF_P_S(x);

      The approach here makes y and z first to allow for easier manipulation 

      *)
      let fName,(declarations,rules) = translation_inner f in
      let newName = "AF_" ^ fName in
      let sName = newName ^ "_S" in
      let tName = newName ^ "_T" in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
      let arg = VAR "tempOne" in
      let firstArg, fNewArgs = match fArgs with
        [] -> failwith "confused"
        | x :: xs -> x, arg :: xs in

      let tArg = VAR "__state_extra" in
      let tParam = "__state_extra" , Symbol in
      let tArgs = tArg :: fArgs in
      let tNewArgs = arg :: fArgs in
      let tParams = tParam :: fParams in

      let newDeclarations = [
        (newName,fParams);
        (sName,fParams);
        (tName, tParams);

      ] in
      let newRules = [
        (newName,fArgs), [Pos("state", [firstArg]); Neg (sName, fArgs)];

        (sName, fArgs), [Pos(tName, firstArg :: firstArg :: List.tl fArgs)];
        (sName,fArgs), [ Neg(fName, fArgs); Pos("flow", [firstArg; arg]); Pos(sName,fNewArgs)  ];

        (tName, tArgs), [ Neg(fName,fArgs); Pos("flow", [firstArg; tArg] ) ];
        (tName, tArgs), [ Pos(tName,tNewArgs) ;Neg(fName,fNewArgs); Pos("flow", [arg; tArg] ) ];
        

      ] in
      newName,( newDeclarations @ declarations, newRules @ rules)
 
    
    | EU (f1,f2)->
      processPair f1 f2 
      (fun x1 x2 ->  x1 ^ "_EU_" ^ x2) 
      (fun (newName,newArgs) (x1,f1Args) (x2,f2Args) -> 
        let arg = VAR "tempOne" in
        let firstArg, fNewArgs = match newArgs with
        [] -> failwith "confused"
        | x :: xs -> x, arg :: xs in
        [ 
        (newName,newArgs) , [ Pos(x2,f2Args) ];
        (newName,newArgs) , [ Pos(x1,f1Args); Pos("flow",[arg;firstArg]); Pos(newName,fNewArgs) ];
      ])

    
      
    (* Derivative rules *)
    | AX f ->
      (* AX f = !EX !f *)     
      let fName,(declarations,rules) = translation_inner  (EX (Neg f)) in
      let prefixLen = (List.fold_right (+) (List.map String.length [ "EX_"; "NOT_"]) 0) in
      let newName = "AX_" ^  (String.sub fName prefixLen (String.length fName - prefixLen)) in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Pos("state", [ VAR "loc"]);Neg (fName,fArgs) ]):: rules)
    
    | AG f ->
      (* AG f  = !EF !f *)     
      let fName,(declarations,rules) = translation_inner (EF (Neg f)) in
      let prefixLen = (List.fold_right (+) (List.map String.length [ "EF_"; "NOT_"]) 0) in
      let newName = "AG_" ^  (String.sub fName prefixLen (String.length fName - prefixLen)) in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Pos("state", [ VAR "loc"]) ;Neg (fName,fArgs) ]):: rules)

    | EG f ->
      (* EG f = !AF !f *)     
      let fName,(declarations,rules) = translation_inner (AF (Neg f)) in
      let prefixLen = (List.fold_right (+) (List.map String.length [ "AF_"; "NOT_"]) 0) in
      let newName = "EG_" ^  (String.sub fName prefixLen (String.length fName - prefixLen)) in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
        newName,(  (newName,fParams) :: declarations, ( (newName,fArgs), [Pos("state", [ VAR "loc"]) ;Neg (fName,fArgs) ]):: rules)

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
      ("entry",     [ ("x", Number)]); 
      ("state",     [ ("x", Number)]);
      ("flow",      [ ("x", Number); ("y", Number) ]);
      ("transFlow", [ ("x", Number); ("y", Number) ]); 
      ] in
    let defaultRules = [ 
      ("transFlow", [VAR "x"; VAR "y"] ), [ Pos ("flow", [VAR "x"; VAR "y"]) ] ;
      ("transFlow", [VAR "x"; VAR "z"] ), [ Pos ("flow", [VAR "x"; VAR "y"]); Pos ("transflow", [VAR "y"; VAR "z"]) ] 
      ] in
      (defaultDecs @ List.rev decs, defaultRules @ List.rev rules)

  (* core, EX, AF, AU, the rest needs to be translated *)

let tests  = 
  let xIsValue_1 = Atom("xIsValue_1", (Eq(VAR "x", INT 1))) in 
  let xIsValue_0 = Atom("xIsValue_0", (Eq(VAR "x", INT 0))) in 
  let aF_xIsValue_0 = AF(xIsValue_0) in 
  let xIsValue_1_Imply_AF_xIsValue_0 = Imply (xIsValue_1, aF_xIsValue_0) in 
  let eG_xIsValue_1_Imply_AF_xIsValue_0 = EG(xIsValue_1_Imply_AF_xIsValue_0) in 
  let aG_xIsValue_1_Imply_AF_xIsValue_0 = AG(xIsValue_1_Imply_AF_xIsValue_0) in 

  [
    Atom("xIsPos", (Gt(VAR "x", INT 0)));
    Atom("xIsPosAnd2", (PureAnd ((Gt(VAR "x", INT 0)),(Eq(VAR "x", INT 2)))));
    EX(Atom("y", (Gt(VAR "x", INT 0))));  
    EF(Atom("y", (Gt(VAR "x", INT 0))));
    EU ((Atom("z", (Gt(VAR "x", INT 0)))), (Atom("k", (LtEq(VAR "x", INT 0)))));
    EF(AG(Atom ("k", Gt(VAR "x", INT 0))));
    AF(Atom("y", Gt(VAR "x", INT 0)));
    eG_xIsValue_1_Imply_AF_xIsValue_0;
    aG_xIsValue_1_Imply_AF_xIsValue_0

  ] 
  (*
  
  .decl xIsPos (x:number)
  IsPos(x) :- x > 0. 

  *)

let main = 
  List.map (fun item -> print_endline (string_of_datalog (translation item) ^ "\n")) tests