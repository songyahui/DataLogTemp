type terms = INT of int | VAR of string

(*Arithimetic propositions formulae*)
type propositions = string * (terms list)

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
type body = Pos of relation | Neg of relation
type rule = head * (body list) 
type datalog = decl list * rule list

let translation (ctl:ctl) : datalog = 
  match ctl with 
  | _ -> failwith "TBD"