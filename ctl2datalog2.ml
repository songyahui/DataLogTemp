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
type propositions = string * (pure)

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

let translation (ctl:ctl) : datalog = 
  match ctl with 
  | Atom (pName, pure) -> 
    let decls = [] in 
    let head = (pName, []) in 
    let bodies = [Pure pure] in 
    let rule = (head, bodies) in 
    (decls, [rule])
  | _ -> failwith "TBD" 