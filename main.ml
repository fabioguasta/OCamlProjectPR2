type ide = string;;
type exp =
    | CstInt of int
    | CstTrue
    | CstFalse
    | CstString of string
    | Sum of exp * exp
    | Sub of exp * exp
    | Eq of exp * exp
    | Iszero of exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    | Den of ide
    | Ifthenelse of exp * exp * exp
    | Let of ide * exp * exp
    | Fun of ide * exp (* Astrazione di funzione *)
    | FunArgs of ideList * exp (* Astrazione di funzione a più parametri *)
    | Apply of exp * exp (* Applicazione di funzione *)
    | ApplyArgs of exp * expList (* Applicazione di funzione a più parametri*)
    | Dictionary of assocList (* Dichiara un nuovo dizionario *)
    | InsertDict of ide * exp * exp (* Inserisce nuova associazione all'interno del dizionario *)
    | DeleteDict of ide * exp (* Rimuove associazione dal dizionario *)
    | HasKey of ide * exp (* Controlla se una chiave è presente all'intero del dizionario *)
    | Iterate of exp * exp (* Applica una funzione a tutti gli elementi del dizionario *)
    | Fold of exp * exp (*  calcola il valore ottenuto applicando la funzione sequenzialmente a tutti gli elementi del dizionario *)
    | Filter of ideList * exp (* restituisce solo le associazioni del dizionario che sono presenti anche all'interno della lista *)
    and expList = Empty | Exp of exp * expList
    and ideList = Empty | Ide of ide * ideList
    and assocList = Empty | Assoc of ide * exp * assocList
;;

type 't env = (string * 't) list;;
exception WrongBindlist;;
let emptyenv (x: 't) = [("", x)];;
let rec applyenv ((r : 't env), (i : ide)) = match r with
    | [(_, e)] -> e
    | (i1, e1) :: x1 -> if i = i1 then e1
        else applyenv(x1, i)
    | [] -> failwith("wrong env");;
let bind (r : 't env) (i : ide) (v : 't) = (i,v)::r;;
let rec bindlist (r: 't env) (il: ide list) (el: 't list) = match (il, el) with
    | ([], []) -> r
    | (i::il1, e::el1) -> bindlist (bind r i e) il1 el1
    | _ -> raise WrongBindlist;;

type evT = Int of int
    | Bool of bool
    | String of string
    | Closure of ide * exp * evT env        (* chiusura applicazione di funzione a un parametro*)
    | ClosureArgs of ide list * exp * evT env   (* chiusura applicazione di funzione a più parametri *)
    | DicClosure of (ide * evT) list  (*chiusura dizionario*)
    | Unbound;;

let typecheck (x, y) =
    match x with
        | "int" ->
            (match y with
                | Int(u) -> true
                | _ -> false)
        | "bool" ->
            (match y with
                | Bool(u) -> true
                | _ -> false)
        | "string" -> (match y with
                | String(u) -> true 
                | _ -> false)
        | _ -> failwith ("not a valid type");;

let is_zero x = match (typecheck("int",x), x) with
    | (true, Int(y)) -> Bool(y=0)
    | (_, _) -> failwith("run-time error");;

let int_eq(x,y) =
    match (typecheck("int",x), typecheck("int",y), x, y) with
        | (true, true, Int(v), Int(w)) -> Bool(v = w)
        | (_,_,_,_) -> failwith("run-time error ");;

let int_plus(x, y) =
    match(typecheck("int",x), typecheck("int",y), x, y) with
        | (true, true, Int(v), Int(w)) -> Int(v + w)
        | (_,_,_,_) -> failwith("run-time error ");; 

let int_sub(x, y) =
    match(typecheck("int",x), typecheck("int",y), x, y) with
        | (true, true, Int(v), Int(w)) -> Int(v - w)
        | (_,_,_,_) -> failwith("run-time error ");; 

let bool_and(x, y) =
    match(typecheck("bool",x), typecheck("bool",y), x, y) with
        | (true, true, Bool(v), Bool(w)) -> Bool(v && w)
        | (_,_,_,_) -> failwith("run-time error ");;

let bool_or(x, y) =
    match(typecheck("bool",x), typecheck("bool",y), x, y) with
        | (true, true, Bool(v), Bool(w)) -> Bool(v || w)
        | (_,_,_,_) -> failwith("run-time error ");; 

let bool_not x = match (typecheck("bool",x), x) with
    | (true, Bool(y)) -> Bool(not(y))
    | (_, _) -> failwith("run-time error");;

let rec eval (e: exp) (r: evT env) : evT = match e with
    | CstInt(n) -> Int(n)
    | CstString(s) -> String (s)
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Den(i) -> applyenv (r,i)
    | Iszero(e1) -> is_zero(eval e1 r)
    | Eq(e1, e2) -> int_eq((eval e1 r), (eval e2 r))
    | Sum(e1, e2) -> int_plus ((eval e1 r), (eval e2 r))
    | Sub(e1, e2) -> int_sub ((eval e1 r), (eval e2 r))
    | And(e1, e2) -> bool_and((eval e1 r), (eval e2 r))
    | Or(e1, e2) -> bool_or ((eval e1 r), (eval e2 r))
    | Not(e1) -> bool_not((eval e1 r))
    | Ifthenelse(cond,e1,e2) ->
        let g = eval cond r in
            (match (typecheck("bool", g), g) with
                | (true, Bool(true)) -> eval e1 r
                | (true, Bool(false)) -> eval e2 r
                | (_, _) -> failwith ("nonboolean guard"))
    | Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r))
    | Fun(i, a) -> Closure(i, a, r)     (* Astrazione di funzione *)
    | FunArgs(il, a) -> ClosureArgs(ideListToList il, a, r)     (* Astrazione di funzione a più parametri *)
    | Apply(f, eArg) ->               (* Applicazione di funzione *)
        let fclosure = eval f r in
            (match fclosure with
                | Closure(arg, fbody, fDecEnv) ->
                    let aVal = eval eArg r in
                    let aenv = bind fDecEnv arg aVal in
                        eval fbody aenv
                | _ -> failwith("non functional value"))
    | ApplyArgs(f, eArgs) ->          (* Applicazione di funzione a più parametri*)
        let fclosure = eval f r in
                (match fclosure with
                    | ClosureArgs(args, fbody, fDecEnv) ->
                        let aVals = evalList eArgs r in
                        let aenv = bindlist fDecEnv args aVals in
                            eval fbody aenv
                    | _ -> failwith("non functional value"))
    | Dictionary(assocl) -> DicClosure(evalAssocList assocl r)          (* Dichiara un nuovo dizionario *)
    | InsertDict(i, e, d) ->      (* Inserisce nuova associazione all'interno del dizionario *)
                            let eDic= eval d r in ( match eDic with
                                    |DicClosure(l) -> if not(checkIn i l) then DicClosure((i, eval e r)::l)
                                        else failwith("Existing key")
                                    | _ -> failwith("Not a dictionary") )
                          
    | DeleteDict(i, d) ->           (* Rimuove associazione dal dizionario *)
        let eDic = eval d r in
            (match eDic with
            | DicClosure(l) -> if (checkIn i l) then DicClosure(deleteFromDict i l)
                else failwith("Unexisting key")
            | _ -> failwith("Not a dictionary"))
    | HasKey(i, d) ->     (* Controlla se una chiave è presente all'interno del dizionario *)
                        let eDic = eval d r in
                                (match eDic with
                                | DicClosure(l) -> if (checkIn i l) then Bool(true)
                                                                    else Bool(false)
                                | _ -> failwith("Not a dictionary"))
       
    | Iterate(f, d) ->      (* Applica una funzione a tutti gli elementi del dizionario *)
        let eDic = eval d r in
        (let eFun = eval f r in
            (match eFun with 
            | Closure(arg, fbody, fDecEnv) ->
                        (match eDic with
                            | DicClosure(l) -> DicClosure(applyToDict arg fbody fDecEnv l)
                            | _ -> failwith("Not a dictionary"))
            | _ -> failwith("non functional value")))
    | Fold(f, d) -> (*  calcola il valore ottenuto applicando la funzione sequenzialmente a tutti gli elementi del dizionario *)
        let eDic = eval d r in
            let eFun = eval f r in
                (match eFun with 
                | ClosureArgs(args, fbody, fDecEnv) -> if ((List.length args) = 2)
                            then (match eDic with
                                | DicClosure(l) -> applyFoldToDict args fbody fDecEnv l
                                | _ -> failwith("Not a dictionary"))
                            else failwith("The function doesn't take 2 arguments")
                | _ -> failwith("non functional value"))
    | Filter(keys, d) -> (* restituisce solo le associazioni del dizionario che sono presenti anche all'interno della lista *)
        let eDic = eval d r in
            (match eDic with
            | DicClosure(l) -> DicClosure(filterList (ideListToList keys) l)
            | _ -> failwith("Not a dictionary"))
    and evalList (l : expList) (amb: evT env) = match l with (* Valuta tutte le espressioni di una lista *)
        | Empty -> []
        | Exp(e, ls) -> (eval e amb)::(evalList ls amb)
    and ideListToList (l: ideList) : ide list = match l with (* Converte una ideList in una ide list *)
        | Empty -> []
        | Ide(i, ls) -> i::(ideListToList ls)
    and evalAssocList (l: assocList) (amb: evT env) : (ide * evT) list = match l with (* Valuta una assocList per restituire una (ide * evT) list *)
        | Empty -> []
        | Assoc(i, e, ls) -> (i, (eval e amb))::(evalAssocList ls amb)
    and checkIn (i: ide) (l: (ide * evT) list) : bool = match l with (* Controlla se una chiave è contenuta in una lista di associazioni *)
        | [] -> false
        | (key, value)::ls -> if key = i then true
            else checkIn i ls
    and deleteFromDict (i: ide) (l: (ide * evT) list) : (ide * evT) list = match l with (* Elimina un elemento da una lista di associazioni chiave-valore *)
        | [] -> []
        | (key, value)::ls -> if (key = i) then deleteFromDict i ls
            else (key, value)::(deleteFromDict i ls)
    and applyToDict (arg: ide) (fbody: exp) (fEnv: evT env) (l: (ide * evT) list)  : (ide * evT) list = match l with (* Applica una funzione a tutti gli elementi di una lista di associazioni *)
        | [] -> []
        | (key, value)::ls -> let newValue = eval fbody (bind fEnv arg value) in
            (key, newValue)::(applyToDict arg fbody fEnv ls)
    and applyFoldToDict (args: ide list) (fbody: exp) (fEnv: evT env) (l: (ide * evT) list) : evT = match l with (* Applica la fold a una lista di associazioni *)
        | [] -> Int(0)
        | (key, value)::ls -> let newEnv = bindlist fEnv args [value; (applyFoldToDict args fbody fEnv ls)] in
            eval fbody newEnv
    and filterList (filters: ide list) (l: (ide * evT) list) : (ide * evT) list = match l with (* Filtra gli elementi di una lista di associazioni *)
        | [] -> []
        | (key, value)::ls -> if (List.mem key filters) then (key, value)::(filterList filters ls)
            else filterList filters ls
;;
