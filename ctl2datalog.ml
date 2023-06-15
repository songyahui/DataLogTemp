type relation = string * (string list)  

and state = 
  | Prop   of relation 
  | Neg    of state 
  | Conj   of state * state 
  | Forall of path
  | Exist  of path

and path = 
  | Next    of state 
  | Global  of state
  | Finally of state 
  | Until   of state * state 
  | Imply   of state * state 

and datalogType = Num | Sym 

and head = relation 
and body = Pos of relation | Neg of relation

and rule = (head * body list) 

let p = ("P", ["x"]) ;;

let (test_cases: ((string * state) list)) = [
 ("test_case_EF" , Exist  (Finally (Prop p)));
 ("test_case_AF" , Forall (Finally (Prop p)));
 ("test_case_EG" , Exist  (Global (Prop p)));
 ("test_case_AG" , Forall (Global (Prop p)));
 ("test_case_AGEF" , Exist  (Global (Prop p)))
]

let rec string_of_state state : string = 
  match state with 
  | Prop   (str, _) -> str 
  | Neg    state -> "not_" ^ string_of_state state
  | Conj   (state1, state2) ->  string_of_state state1 ^ "_and_" ^ string_of_state state2
  | Forall path -> "A" ^ string_of_path path
  | Exist  path -> "E" ^ string_of_path path

and string_of_path path : string = 
  match path with 
  | Next    state -> "X" ^ string_of_state state 
  | Global  state -> "G" ^ string_of_state state 
  | Finally state -> "F" ^ string_of_state state 
  | Until   (state1, state2) ->  string_of_state state1 ^ "_U_" ^ string_of_state state2
  | Imply   (state1, state2) ->  string_of_state state1 ^ "_Imply_" ^ string_of_state state2



let rec translateState (state:state) : (rule list) = 
  let head = (string_of_state state, ["x"]) in 
  match state with 
  | Forall path -> 
    [(head, [Pos("S", ["x"])])]
  | Exist  path -> translatePath path 
  | _ -> []

and translatePath (path:path) : (rule list) = 
  let head = (string_of_path path, ["x"]) in 
  match path with 
  | Finally (Prop p) -> 
    [(head, [Pos (p)]); 
     (head, [Pos (p)]);]
  | _ -> [] 

let rec string_of_arguments (argLi) : string = 
  match argLi with 
  | [] -> ""
  | [x] -> x 
  | x :: xs  -> x ^ ", " ^ string_of_arguments xs 

let string_of_relation (name, args) : string = 
  name ^ "("^  string_of_arguments args ^ ")" 

let rec string_of_body (bodyLi) : string = 
  match bodyLi with 
  | [] -> ""
  | [x] -> 
    (match x with 
    | Pos re -> string_of_relation re 
    | Neg re -> "!" ^ string_of_relation re )
  | x :: xs -> 
    (match x with 
    | Pos re -> string_of_relation re 
    | Neg re -> "!" ^ string_of_relation re ) ^ ", " ^ string_of_body xs 


let string_of_rule (head, body) = 
  string_of_relation head ^ " :- " ^ 
  string_of_body body ^ "."

let string_of_rules rules = List.fold_left (fun acc a -> acc ^ "\n"^ string_of_rule a) "" rules


let test_main () = 
  let ef = Exist (Finally (Prop p)) in 
  let rules = translateState (ef) in 
  print_endline (string_of_rules rules ^ "\n")


let appendNames (command: string list) : string = 
  List.fold_left (fun acc a -> acc ^ a) "" command

let rec tranlation2 (command: string list) : string = 
  let relation = appendNames (command) in
  match command with 
  | [] -> ""
  | [x] -> ""
  | x :: y :: rest  -> 
  

  if String.compare (String.sub x 0 1) "A" == 0 then 
    let subrelation = "N_" ^ appendNames (y::rest) in 
    relation ^"(x) :- S(x), !"^ subrelation ^"(x).\n\n" ^ 
    tranlation2 (("N_" ^ y)::rest) 

  else if String.compare (String.sub x 0 1) "E" == 0 then 
    let subrelation = appendNames (y::rest) in 
    relation ^"(x) :- "^ subrelation ^"(x).\n\n" ^ 
    tranlation2 (y::rest) 
  
  else 
    let subrelation = appendNames (y::rest) in 

    (
    if String.compare (String.sub x 0 1) "X" == 0 then 
      relation ^"(x) :- transition(x, y), "^
      relation ^"(y).\n\n"


    else if String.compare (String.sub x 0 1) "F" == 0 then 
      relation ^"(x) :- " ^ y ^ "(x). \n"^
      relation^"(x) :- transition(x, y), "^relation^"(y).\n\n"


    else if String.compare (String.sub x 0 1) "G" ==0  then 
      relation^"(x) :- End(x), " ^ y ^ "(x). \n"^
      relation^"(x) :- transition(x, y), " ^ y ^ "(x), "^relation^"(y).\n\n"

    else if  String.compare (String.sub x 0 3)  "N_G" == 0 then 
      relation ^"(x) :- " ^ subrelation ^ "(x), transition(x, y), "^ relation ^" (y). \n"^
      relation ^"(x) :- S(x), !" ^ subrelation ^ "(x).\n\n" 
    
    else if  String.compare (String.sub x 0 3) "N_F" == 0 then 
      relation ^"(x) :- S(x), !F" ^ y ^ "(x). \n"^
      relation ^"(x) :- S(x), !" ^ y ^ "(x), transition(x, y), "^relation^"(y).\n\n" 
      ^ tranlation2 ("F"::y::rest) 
     
    else "not supporting\n\n") ^ tranlation2 (y::rest) 
    ;;


let test_cases_tranlation2 = [
  ["E"; "G"; "Y"];
  ["N_G";"F";"Prop"];
  ["A";"G";"E";"F";"Prop"];
  ["E";"X";"Prop"];
  ["A";"F";"Prop"];
  ["A";"G";"Prop"]
  ];;

let results = List.fold_left (fun acc a -> 
  acc ^ 
  "\n=====" ^appendNames a^ "=====\n" ^ 
  tranlation2 a
  ) "" test_cases_tranlation2 ;;

print_endline (results);;