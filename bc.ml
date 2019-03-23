open Core
open Printf

(* let numbers = Map.of_alist_exn (module String) ["three", 3; "four", 4];; *)
(* let empty = Map.emtpy (module String) *)

(* data types ----------------------------------- *)

type sExpr = 
    | Atom of string
    | List of sExpr list

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 

exception NotImplemented of string
exception MissingScope of string

type block = statement list 

(* map of 2-tupes variable:value *)
type env = (string, float, String.comparator_witness) Map.t

(* a list of environments *)
type envQueue = env list


(* functions ----------------------------------- *)

(* 

type envQueue = env list;;

let l = [ Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
          Map.of_alist_exn (module String) ["scope1_var1", 11.; "scope1_var2", 21.]; 
          Map.of_alist_exn (module String) ["scope2_var1", 12.; "scope2_var2", 22.]];;
          
let varEval (_v:string) (_q:envQueue): float =
    match List.rev _q with (* look for value in right most context first *)
    | [] -> 0.
    | hd :: tl -> ( match Map.find hd _v with
        | Some value -> value
        | None -> (
        (* is not in the right most, look in the leftmost *)
            match List.rev tl with
            | [] -> 0.
            | hd :: _ -> ( match Map.find hd _v with
                | Some value -> value
                | None       -> 0.
            )
        )
    );;

varEval "global1" l;;
varEval "scope1_var1" l;;
varEval "scope1_var2" l;;
varEval "scope2_var1" l;;
varEval "scope2_var2" l;;

*)

let varEval (_v:string) (_q:envQueue): float =
    match List.rev _q with (* look for value in right most context first *)
    | [] -> 0.
    | hd :: tl -> ( match Map.find hd _v with
        | Some value -> value
        | None -> (
        (* is not in the right most, look in the leftmost *)
            match List.rev tl with
            | [] -> 0.
            | hd :: _ -> ( match Map.find hd _v with
                | Some value -> value
                | None       -> 0.
            )
        )
    )

let varAssign (_v:string) (_f:float) (_q:envQueue): envQueue =
    match List.rev _q with
    | hd :: tl -> ( 
        match Map.find hd _v with
        | Some _ -> List.rev (Map.set hd ~key:_v ~data:_f :: tl)
        | None -> (
        (* is doesn't exist in the right most, put in the leftmost, that is, global *)
            match _q with
            | hd :: tl -> List.rev (Map.set hd ~key:_v ~data:_f :: tl)
            | _ -> _q
        )
    )
    | _ -> raise (MissingScope "No scope to set vars") (* should never be empty *)

let rec evalExpr (_e: expr) (_q:envQueue): float =
    match _e with
    | Num(e) -> e
    | Var(e) -> varEval e _q
    | Op1(op, e) -> ( (* treat expressions with 1 operand *)
        match op with
        | "-"  -> evalExpr e _q *. -1.
        | "++" -> evalExpr e _q +. 1.
        | "--" -> evalExpr e _q -. 1.
        | "!"  -> if (compare (evalExpr e _q) 0.)=0 then 1. else 0.
        | _ -> 0.
    )
    | Op2(op, el, er) -> ( (* treat expressions with 2 operands *)
        match op with
        | "^"  -> evalExpr el _q ** evalExpr er _q
        | "/"  -> evalExpr el _q /. evalExpr er _q
        | "*"  -> evalExpr el _q *. evalExpr er _q
        | "-"  -> evalExpr el _q -. evalExpr er _q
        | "+"  -> evalExpr el _q +. evalExpr er _q
        | "==" -> if compare(evalExpr el _q) (evalExpr er _q) = 0 then 1. else 0.
        | "!=" -> if compare(evalExpr el _q) (evalExpr er _q) != 0 then 1. else 0.
        | "<" -> if compare(evalExpr el _q) (evalExpr er _q) < 0 then 1. else 0.
        | "<=" -> if compare(evalExpr el _q) (evalExpr er _q) <= 0 then 1. else 0.
        | ">" -> if compare(evalExpr el _q) (evalExpr er _q) > 0 then 1. else 0.
        | ">=" -> if compare(evalExpr el _q) (evalExpr er _q) >= 0 then 1. else 0.
        | "&&" -> if (compare (evalExpr el _q) 1. = 0 && compare (evalExpr er _q) 1. = 0) then 1. else 0.
        | "||" -> if (compare (evalExpr el _q) 1. = 0 || compare (evalExpr er _q) 1. = 0) then 1. else 0.
        | _ -> 0.
    )
    | Fct(identifier, params) -> raise ( NotImplemented (identifier ^ "Not implemented") )

let evalCode (_code: block) (_q:envQueue): unit = 
    let local = Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.] in
    let temp = _q@[local] in
    List.fold ~init:temp ~f:(fun acc stat -> evalStatement stat acc) _code
    (* find out how to return unit type with different types *)

        
    (* create the local environment *)
    (* let List.fold (fun ) *)
    (* user fold_left  *)
    (* pop the local environment *)
    print_endline "Not done yet!"
    (* raise (NotImplemented "Not implemented") *)
    
let evalStatement (s: statement) (q: envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> varAssign _v (evalExpr _e q) q
        | Expr(_e) -> evalExpr _e q |> printf "%F" ; q
        (*| Return(_e) -> raise (NotImplemented "Not implemented")
        | If(_, _, _) -> raise (NotImplemented "Not implemented")
            let condition = evalExpr e q in
                if(condition>0.0) then
                    evalCode codeT q 
                else
                    evalCode codeF q
            ; q
        | While(_, _) -> raise (NotImplemented "Not implemented")
        | For(_, _, _, _) -> raise (NotImplemented "Not implemented")
        | FctDef(_, _, _) -> raise (NotImplemented "Not implemented")*)
        | _ -> q (* ignore *)


(* TESTS ------------------------------------------------------------------------------- *)

(* varEval test *)
let%expect_test "test_varEval_globalScope" =
    let queue = [ Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
                  Map.of_alist_exn (module String) ["scope1_var1", 11.; "scope1_var2", 21.]; 
                  Map.of_alist_exn (module String) ["scope2_var1", 12.; "scope2_var2", 22.]] in
    varEval "global1" queue |>
    printf "%F";
    [%expect {| 10. |}]

(* varEval test *)
let%expect_test "test_varEval_currentScope" =
    let queue = [ Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
                  Map.of_alist_exn (module String) ["scope1_var1", 11.; "scope1_var2", 21.]; 
                  Map.of_alist_exn (module String) ["scope2_var1", 12.; "scope2_var2", 22.]] in
    varEval "scope2_var1" queue |>
    printf "%F";
    [%expect {| 12. |}]

(* varEval test *)
let%expect_test "test_varEval_intermediateScope" = (* should return zero *)
    let queue = [ Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
                  Map.of_alist_exn (module String) ["scope1_var1", 11.; "scope1_var2", 21.]; 
                  Map.of_alist_exn (module String) ["scope2_var1", 12.; "scope2_var2", 22.]] in
    varEval "scope1_var1" queue |>
    printf "%F";
    [%expect {| 0. |}]

(* varEval test *)
let%expect_test "test_varEval_shadowedGlobal" = (* should return the local var *)
    let queue = [ Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
                  Map.of_alist_exn (module String) ["scope1_var1", 11.; "scope1_var2", 21.]; 
                  Map.of_alist_exn (module String) ["scope2_var1", 12.; "global1", 22.]] in
    varEval "global1" queue |>
    printf "%F";
    [%expect {| 22. |}]

(* varEval test *)
let%expect_test "test_varEval_empty" = (* should return zero *)
    let queue = [] in
    varEval "global1" queue |>
    printf "%F";
    [%expect {| 0. |}]

(* ------------------------------------------------------------------------------------- *)

(* varAssign test *)
let%expect_test "test_varAssign_onEmpty" =
    let q = [Map.of_alist_exn (module String) []] in
    let q = varAssign "global1" 0.5 q in
    varEval "global1" q |>
    printf "%F";
    [%expect {| 0.5 |}]

(* varAssign test *)
let%expect_test "test_varAssign_onGlobalOnly" =
    let q = [Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.]] in
    let q = varAssign "global2" 0.1 q in
    varEval "global2" q |>
    printf "%F";
    [%expect {| 0.1 |}]

(* varAssign test *)
let%expect_test "test_varAssign_onCurrentScope" =
    let q = [Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
             Map.of_alist_exn (module String) ["scope1_var1", 11.; "scope1_var2", 21.]; 
             Map.of_alist_exn (module String) ["scope2_var1", 12.; "global1", 22.]] in
    let q = varAssign "scope2_var1" 1. q in
    varEval "scope2_var1" q |>
    printf "%F";
    [%expect {| 1. |}]





(* test for Expr statement *)
let%expect_test "test_varAssign_onCurrentScope" =
    let q = [Map.of_alist_exn (module String) ["global1", 0.; "global2", 20.]] in
    let q = evalStatement (Expr(Num(1.))) q in
    let _ = varEval "global1" q in (* the test is captured from std out *)
    print_string ""; (* dummy to return unit *)
    [%expect {| 1. |}]

(* test for Expr statement *)
let%expect_test "test_varAssign_onCurrentScope" =
    let q = [Map.of_alist_exn (module String) ["global1", 0.; "global2", 20.]] in
    let q = evalStatement (Expr(Op2("+", Num(1.0), Num(1.0)))) q in
    let _ = varEval "global1" q in (* the test is captured from std out *)
    print_string ""; (* dummy to return unit *)
    [%expect {| 2. |}]






(* Test for Num expression *)
let%expect_test "evalNum" = 
    evalExpr (Num 10.) [] |>
    printf "%F";
    [%expect {| 10. |}]

(* Test Op2 expressions *)

(* ^ *)
let%expect_test "evalPow" = 
    evalExpr (Op2 ("^", (Num 2.), (Num 3.))) [] |>
    printf "%F";
    [%expect {| 8. |}]

(* / *)
let%expect_test "evalDiv" = 
    evalExpr (Op2 ("/", (Num 5.), (Num 2.))) [] |>
    printf "%F";
    [%expect {| 2.5 |}]

(* * *)
let%expect_test "evalMult" = 
    evalExpr (Op2 ("*", (Num 10.5), (Num 100.))) [] |>
    printf "%F";
    [%expect {| 1050. |}]

(* - *)
let%expect_test "evalSubtract" = 
    evalExpr (Op2 ("-", (Num 10.), (Num 25.5))) [] |>
    printf "%F";
    [%expect {| -15.5 |}]

(* + *)
let%expect_test "evalAdd" = 
    evalExpr (Op2 ("+", (Num 10.), (Num 25.5))) [] |>
    printf "%F";
    [%expect {| 35.5 |}]

(* == positive case *)
let%expect_test "evalEqualPositive" = 
    evalExpr (Op2 ("==", (Num 10.), (Num 10.))) [] |>
    printf "%F";
    [%expect {| 1. |}]

(* == negative case *)
let%expect_test "evalEqualNegative" = 
    evalExpr (Op2 ("==", (Num 11.), (Num 10.))) [] |>
    printf "%F";
    [%expect {| 0. |}]

(* != positive case *)
let%expect_test "evalNotEqualPositive" = 
    evalExpr (Op2 ("!=", (Num 11.), (Num 10.))) [] |>
    printf "%F";
    [%expect {| 1. |}]

(* != negative case *)
let%expect_test "evalNotEqualNegative" = 
    evalExpr (Op2 ("!=", (Num 10.), (Num 10.))) [] |>
    printf "%F";
    [%expect {| 0. |}]

(* < positive case *)
let%expect_test "evalLessThanPositive" = 
    evalExpr (Op2 ("<", (Num 1.), (Num 2.))) [] |>
    printf "%F";
    [%expect {| 1. |}]

(* < negative case *)
let%expect_test "evalLessThanNegative" = 
    evalExpr (Op2 ("<", (Num 2.), (Num 1.))) [] |>
    printf "%F";
    [%expect {| 0. |}]

(* <= positive case *)
let%expect_test "evalLessThanOrEqualPositive" = 
    evalExpr (Op2 ("<=", (Num 1.), (Num 2.))) [] |>
    printf "%F";
    [%expect {| 1. |}]

(* <= negative case *)
let%expect_test "evalLessThanOrEqualNegative" = 
    evalExpr (Op2 ("<=", (Num 2.), (Num 1.))) [] |>
    printf "%F";
    [%expect {| 0. |}]

(* > positive case *)
let%expect_test "evalGreaterThanPositive" = 
evalExpr (Op2 (">", (Num 4.), (Num 2.))) [] |>
printf "%F";
[%expect {| 1. |}]

(* > negative case *)
let%expect_test "evalGreaterThanNegative" = 
    evalExpr (Op2 (">", (Num 1.), (Num 2.))) [] |>
    printf "%F";
    [%expect {| 0. |}]

(* >= positive case *)
let%expect_test "evalGreaterThanOrEqualPositive1" = 
evalExpr (Op2 (">=", (Num 2.), (Num 2.))) [] |>
printf "%F";
[%expect {| 1. |}]

(* >= positive case *)
let%expect_test "evalGreaterThanOrEqualPositive2" = 
evalExpr (Op2 (">=", (Num 4.), (Num 2.))) [] |>
printf "%F";
[%expect {| 1. |}]

(* >= negative case *)
let%expect_test "evalGreaterThanOrEqualNegative" = 
    evalExpr (Op2 (">=", (Num 1.), (Num 2.))) [] |>
    printf "%F";
    [%expect {| 0. |}]

(* 1 && 1 = 1 *)
let%expect_test "evalGreaterThanOrEqualNegative" = 
    evalExpr (Op2 ("&&", (Num 1.), (Num 1.))) [] |>
    printf "%F";
    [%expect {| 1. |}]

(* 1 && 0 = 0 *)
let%expect_test "evalGreaterThanOrEqualNegative" = 
    evalExpr (Op2 ("&&", (Num 1.), (Num 0.))) [] |>
    printf "%F";
    [%expect {| 0. |}]

(* 0 && 1 = 0 *)
let%expect_test "evalGreaterThanOrEqualNegative" = 
    evalExpr (Op2 ("&&", (Num 0.), (Num 1.))) [] |>
    printf "%F";
    [%expect {| 0. |}]

(* 0 && 0 = 0 *)
let%expect_test "evalGreaterThanOrEqualNegative" = 
    evalExpr (Op2 ("&&", (Num 0.), (Num 0.))) [] |>
    printf "%F";
    [%expect {| 0. |}]


(* 
    v = 10; 
    v // display v
 *)
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    evalCode p1 []; 
    [%expect {| 1. |}]

(*
    v = 1.0;
    if (v>10.0)
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p1" =
    evalCode p2 []; 
    [%expect {| 3628800. |}]

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0)
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)

let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]



