type terms = INT of int | VAR of string      
(*Arithimetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of terms * terms
          | Lt of terms * terms
          | GtEq of terms * terms
          | LtEq of terms * terms
          | Eq of terms * terms
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure

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

type basic_Type = Number | Symble 
type param  = string * basic_Type
type decl = string * (param list)

type relation = string * (terms list)
type head = relation
type body = Pos of relation | Neg of relation | Pure of pure 
type rule = head * (body list) 
type datalog = decl list * rule list

let string_of_relation (relation:relation)= "string_of_relation TBD " 

let string_of_bodies (bodies:body list) = "string_of_bodies TBD " 

let string_of_decls (decls:decl list) = "string_of_decls TBD;\n"
let rec string_of_rules (rules: rule list) = 
  match rules with 
  | [] -> "" 
  | (head, bodies)::rest -> 
    string_of_relation head ^ " :- " ^ string_of_bodies bodies ^ ".\n" 
    ^ string_of_rules rest



let string_of_datalog (datalog:datalog) : string = 
  let (decls, rules) = datalog in 
  string_of_decls decls ^ string_of_rules rules

let translation (ctl:ctl) : datalog = 
  match ctl with 
  | Atom (pName, pure) -> 
    let decls = [] in 
    let head = (pName, []) in 
    let bodies = [Pure pure] in 
    let rule = (head, bodies) in 
    (decls, [rule])
  | _ -> failwith "TBD" 

  (* core, EX, AF, AU, the rest needs to be translated *)

let tests  = 
  [Atom("xIsPos", (Gt(VAR "x", INT 0)))] 
  (*
  
  .decl xIsPos (x:number)
  IsPos(x) :- x > 0. 

  *)

let main = 
  List.map (fun item -> print_endline (string_of_datalog (translation item))) tests