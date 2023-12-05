type terms = INT of int | STR of string | VAR of string | Any   
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
| Any -> "_"

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
  | Eq (a,b) -> "(" ^ (string_of_term a) ^ " = " ^ (string_of_term b) ^ ")"
  | NEq (a,b) -> "(" ^ (string_of_term a) ^ " != " ^ (string_of_term b) ^ ")"
  | PureOr(a,b) -> "(" ^ (string_of_pure a) ^ " || " ^ (string_of_pure b) ^ ")"
  | PureAnd(a,b) -> "(" ^ (string_of_pure a) ^ " && " ^ (string_of_pure b) ^ ")"
  | Neg a -> "!(" ^ (string_of_pure a) ^ ")"
  (* | Pos a -> string_of_term a *)


let string_of_bodies (bodies:body list) = 
  expand_args ", " (List.map (fun body -> match body with
    Pos r -> string_of_relation r
  | Neg r -> "!"  ^ string_of_relation r
  | Pure p -> string_of_pure p 

  
  
  ) bodies)


let string_of_decl (decl:decl) =
  match decl with
  name,args -> ".decl "^ name ^ "(" ^ (expand_args "," (List.map string_of_param args ))  ^ ")"

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



let rec translation (ctl:ctl) : string * datalog = 
  let fname, (decs,rules) = (translation_inner ctl) in
  let defaultDecs = [
    ("entry",     [ ("x", Number)]);  
    ("end",       [ ("x", Number)]); 
    ("valuation", [ ("x", Symbol); ("loc", Number); ("n", Number)]);
    ("assign",    [ ("x", Symbol); ("loc", Number); ("n", Number)]);
    (*("assignNonDetermine",    [ ("x", Symbol); ("loc", Number)]);*)
    ("state",     [ ("x", Number)]);
    ("flow",      [ ("x", Number); ("y", Number) ]);
    ("transFlow", [ ("x", Number); ("y", Number) ]); 
    ] in
  let defaultRules = [ 
    ("transFlow", [VAR "x"; VAR "y"] ), [ Pos ("flow", [VAR "x"; VAR "y"]) ] ;
    ("transFlow", [VAR "x"; VAR "z"] ), [ Pos ("flow", [VAR "x"; VAR "y"]); Pos ("transFlow", [VAR "y"; VAR "z"]) ];
    
    ("valuation", [VAR "x"; VAR "loc"; VAR "n"] ), [ Pos ("assign", [VAR "x"; VAR "loc"; VAR "n"]) ] ;
    ("valuation", [VAR "x"; VAR "loc"; VAR "n"] ), 
      [ Pos ("valuation", [VAR "x"; VAR "locTemp"; VAR "n"] );  
        Pos ("flow", [VAR "locTemp"; VAR "loc"]); 
        Neg ("assign", [VAR "x"; VAR "loc"; Any]) ] ;
    ] in

    
    (**********************************************************************
    The following code is to add the top level rule to only show the 
    reslts on the entry pointes. For example, if the query is: 
    EF_terminating(loc); then it will be added with the following EF_terminatingFinal rule 
    EF_terminatingFinal(loc) :- entry(loc), EF_terminating(loc).     
    --- Yahui Song
    **********************************************************************)
    let decs, rules  = 
    (match rules with 
    | ((name, [VAR "loc"]), _)::_ -> 
      let nameFinal = name^"Final" in 
      let finaDecl = (nameFinal,     [ ("loc", Number)]) in 
      let finalRule = ((nameFinal, [VAR "loc"]), [Pos("entry",  [VAR "loc"]) ; Pos (name, [VAR "loc"])]) in 
      finaDecl::decs,  finalRule::rules
    | _ -> decs, rules
    ) in 

    fname, (defaultDecs @ List.rev decs, defaultRules @ List.rev rules)

  
and get_params (declarations: decl list) =
    match declarations with
    [] -> []
    | x :: xs -> snd x

and get_args (rules: rule list) =
    match rules with
    | [] -> []
    | x::xs -> snd (fst x) 

and process_args (args:terms list) =
    List.sort_uniq 
    (fun x y -> 
      match (x,y) with
      | (VAR x, VAR y) -> String.compare x y
      | _ -> failwith "Arguments should only be variables"
      )
    (List.filter (fun x -> match x with  VAR x -> true | _ -> false ) args ) 


and translation_inner (ctl:ctl) : string * datalog =

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
      let valuationAtom var = Pos ("valuation", [STR var; VAR "loc"; VAR (var^"_v")] ) in 
      (match pure with 
      | Gt(STR x, INT n ) -> 
        pName,([(pName,params)], [  ((pName, vars), [Pos("state", [VAR "loc"]) ; valuationAtom x; Pure (Gt(VAR (x^"_v"), INT n ))]) ])
      | GtEq(STR x, INT n ) -> 
        pName,([(pName,params)], [  ((pName, vars), [Pos("state", [VAR "loc"]) ; valuationAtom x; Pure (GtEq(VAR (x^"_v"), INT n ))]) ])

      | Lt(STR x, INT n ) -> 
        pName,([(pName,params)], [  ((pName, vars), [Pos("state", [VAR "loc"]) ; valuationAtom x; Pure (Lt(VAR (x^"_v"), INT n ))]) ])

      | Eq(STR x, INT n ) -> 
        pName,([(pName,params)], [  ((pName, vars), [Pos("state", [VAR "loc"]) ; valuationAtom x; Pure (Eq(VAR (x^"_v"), INT n ))]) ])

      (* *********************************************************************
      The above the pattern matching is needed for checking variables' values, for example, 
      "x" > 1 will be written as valuation("x", loc, x_v), x_v>1. 
      --- Yahui Song
      ********************************************************************* *)
      | _ ->  pName,([(pName,params)], [  ((pName, vars), [Pos("state", [VAR "loc"]) ; Pure pure]) ])
      )
    
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
      let fName,(declarations,rules) = 
        match f with 

        | Atom (notPName, Lt(STR x, INT n)) -> 

          let negetionName, (negetionDecl, negetionRules) = translation_inner (Atom ("not_"^notPName,  GtEq(STR x, INT n))) in 
          
          let findallDecl = (notPName, [ ("loc", Number)]) in  
          let findallRules = (notPName, [VAR "loc"] ), [ Pos ("state", [VAR "loc"]); Pos("valuation", [STR x; VAR "loc"; Any]); Neg(negetionName, [VAR "loc"]) ] in  


          notPName, (findallDecl :: negetionDecl, findallRules :: negetionRules)

      (* *********************************************************************
      The above the pattern matching is needed for constructing the atomic P rules, 
      when we are computing AF p. Usually the encoding needs simple "!P". 
      But since the analysis may be overapproximating, we need to compute “for sure !P” 
      For example, AF x<0, will be encoded as: 

      1) not_xIsSmallerThan_0(loc) :- state(loc), valuation("x",loc,x_v), (x_v >= 0).
      2) xIsSmallerThan_0(loc) :- state(loc), valuation("x",loc,_), !not_xIsSmallerThan_0(loc).

      where 1) captures the negation of x<0, then 2) captures "for sure x<0", 
      then subsequently, we can use !xIsSmallerThan_0 as usual. 

      --- Yahui Song
      ********************************************************************* *)

        | _ -> translation_inner f in
      let newName = "AF_" ^ fName in
      let sName = newName ^ "_S" in
      let tName = newName ^ "_T" in
      let fParams = get_params declarations in
      let fArgs = get_args rules in
      let arg = VAR "tempOne" in
      let firstArg, fNewArgs = match fArgs with
        [] -> failwith "confused"
        | x :: xs -> x, arg :: xs in

      let tArg = VAR "interm_state" in
      let tParam = "interm_state" , Number in
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
        (* for finite traces *)
        (sName,fArgs), [ Neg(fName, fArgs); Pos("end", [firstArg])]; 
        (* for infinite traces *)
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

    
  (* core, EX, AF, AU, the rest needs to be translated *)

let tests  = 
  let xIsValue_1 = Atom("xIsValue_1", (Eq(STR "x", INT 1))) in 
  let xIsValue_0 = Atom("xIsValue_0", (Eq(STR "x", INT 0))) in 
  let aF_xIsValue_0 = AF(xIsValue_0) in 
  let xIsValue_1_Imply_AF_xIsValue_0 = Imply (xIsValue_1, aF_xIsValue_0) in 
  let eG_xIsValue_1_Imply_AF_xIsValue_0 = EG(xIsValue_1_Imply_AF_xIsValue_0) in 
  let aG_xIsValue_1_Imply_AF_xIsValue_0 = AG(xIsValue_1_Imply_AF_xIsValue_0) in 
  let eF_terminate  = EF(Atom("terminating", (Eq(STR "term", INT 1)))) in 
  let aF_terminate  = AF(Atom("terminating", (Eq(STR "term", INT 1)))) in 
  let aF_yIsValue_1 = AF(Atom("yIsValue_1", (Eq(STR "y", INT 1)))) in 
  let eF_yIsValue_1 = EF(Atom("yIsValue_1", (Eq(STR "y", INT 1)))) in 
  let eF_xIsSmallerThan_0 = EF(Atom("xIsSmallerThan_0", (Lt(STR "x", INT 0)))) in 
  let aF_xIsSmallerThan_0 = AF(Atom("xIsSmallerThan_0", (Lt(STR "x", INT 0)))) in 

  [
    (*Atom("xIsPos", (Gt(STR "x", INT 0)));
    Atom("xIsPosAnd2", (PureAnd ((Gt(VAR "x", INT 0)),(Eq(VAR "x", INT 2)))));
    EX(Atom("y", (Gt(VAR "x", INT 0))));  
    EF(Atom("y", (Gt(VAR "x", INT 0))));
    EU ((Atom("z", (Gt(VAR "x", INT 0)))), (Atom("k", (LtEq(VAR "x", INT 0)))));
    EF(AG(Atom ("k", Gt(VAR "x", INT 0))));
    AF(Atom("y", Gt(VAR "x", INT 0)));
    eG_xIsValue_1_Imply_AF_xIsValue_0;
    aG_xIsValue_1_Imply_AF_xIsValue_0; *)
    eF_terminate;
    aF_terminate;
    aF_yIsValue_1;
    eF_yIsValue_1;
    eF_xIsSmallerThan_0;
    aF_xIsSmallerThan_0
    

  ] 

let main = 
  List.map (fun item -> 
    let fname, program = (translation item) in 
    print_endline (string_of_datalog program);
    (* Here is printing the datalog query command *)
    print_endline (".output "^ fname ^"Final(IO=stdout)\n")
    ) tests