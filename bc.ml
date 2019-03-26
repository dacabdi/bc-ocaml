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
    match _q with (* look for value in right most context first *)
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
    match _q with
    | hd :: tl -> ( 
        match Map.find hd _v with
        | Some _ -> Map.set hd ~key:_v ~data:_f :: tl
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
        | "++" -> evalExpr e _q +.  1.
        | "--" -> evalExpr e _q -.  1.
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
    | Fct(identifier, params) -> raise ( NotImplemented (identifier ^ " Not implemented") )




let rec evalCode (_c: block) (_q:envQueue): envQueue =
    match _q with
    | [] -> evalCode _c [ Map.of_alist_exn (module String) [] ]
    | _  -> List.fold ~init:_q ~f:(fun acc stat -> evalStatement stat acc) _c

and evalStatement (s: statement) (q: envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> varAssign _v (evalExpr _e q) q
        | Expr(_e) -> evalExpr _e q |> printf "%F" ; q
        | If(condition, codeIf, codeElse) -> if compare (evalExpr condition q) 1. = 0 then evalCode codeIf q else evalCode codeElse q
        (*| Return(_e) -> raise (NotImplemented "Not implemented") *)
        | While(c, code) -> whileLoop code c q
        | For(i, c, m, code) -> 
            let foorLoopEnv = evalStatement i q in (* put initial condition in if neccesary *)
            forLoop c m code foorLoopEnv
        (*| FctDef(_, _, _) -> raise (NotImplemented "Not implemented")*)
        | _ -> q (* ignore *)

and whileLoop (c : block) (cond : expr) (q: envQueue) : envQueue =
    if compare (evalExpr cond q) 1. = 0 then
        let newQ = evalCode c q in
            whileLoop c cond newQ;
    else
        q

and forLoop (_c: expr) (_m: statement) (code: block) (q: envQueue) : envQueue =
    if compare (evalExpr _c q) 1. = 0 then
        let newQ = evalCode code q in (* loop maintenance *)
        let newQ = evalStatement _m newQ in
            forLoop _c _m code newQ;
    else
        q



(* TESTS ------------------------------------------------------------------------------- *)

let _test_env = List.rev [ Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
                           Map.of_alist_exn (module String) ["scope1_var1", 11.; "scope1_var2", 21.]; 
                           Map.of_alist_exn (module String) ["scope2_var1", 12.; "scope2_var2", 22.]]

let _test_shadowed = List.rev [ Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
                                Map.of_alist_exn (module String) ["scope1_var1", 11.; "scope1_var2", 21.]; 
                                Map.of_alist_exn (module String) ["global1", 22.; "scope2_var2", 22.]]

(* varEval test *)
let%expect_test "test_varEval_globalScope" =
    let q = _test_env  in
    varEval "global1" q |>
    printf "%F";
    [%expect {| 10. |}]

(* varEval test *)
let%expect_test "test_varEval_currentScope" =
    let q = _test_env  in
    varEval "scope2_var1" q |>
    printf "%F";
    [%expect {| 12. |}]

(* varEval test *)
let%expect_test "test_varEval_intermediateScope" = (* should return zero *)
    let q = _test_env  in
    varEval "scope1_var1" q |>
    printf "%F";
    [%expect {| 0. |}]

(* varEval test *)
let%expect_test "test_varEval_shadowedGlobal" = (* should return the local var *)
    let q = _test_shadowed  in
    varEval "global1" q |>
    printf "%F";
    [%expect {| 22. |}]

(* varEval test *)
let%expect_test "test_varEval_empty" = (* should return zero *)
    let q = []  in
    varEval "global1" q |>
    printf "%F";
    [%expect {| 0. |}]

(* ------------------------------------------------------------------------------------- *)

(* varAssign test *)
let%expect_test "test_varAssign_onEmpty" =
    let q = [ Map.of_alist_exn (module String) [] ] in
    let q = varAssign "global1" 0.5 q in
    varEval "global1" q |>
    printf "%F";
    [%expect {| 0.5 |}]

(* varAssign test *)
let%expect_test "test_varAssign_onGlobalOnly" =
    let q = [ Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.] ] in
    let q = varAssign "global2" 0.1 q in
    varEval "global2" q |>
    printf "%F";
    [%expect {| 0.1 |}]

(* varAssign test *)
let%expect_test "test_varAssign_onCurrentScope" =
    let q = List.rev [Map.of_alist_exn (module String) ["global1", 10.; "global2", 20.];
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



(* ------------------------------------------------------------------------------------- *)

let if1: block = [
    If(
        Op2(">", Var("v"), Num(10.0)),
        [Assign("v", Num(1.0))],
        [] (* no else *)
    );
    Expr(Var("v"))
]

let%expect_test "blockIf_negative_condition" = 
    let c = [Assign("v", Num(5.))]@if1 in
    let _ = evalCode c [] in
    [%expect {| 5. |}]

let%expect_test "blockIf_positive_condition" = 
    let c = [Assign("v", Num(20.))]@if1 in
    let _ = evalCode c [] in
    [%expect {| 1. |}]




let ifelse1: block = [
    If(
        (* if *) Op2(">", Var("v"), Num(10.0)),
        (* then *) [Assign("v", Num(1.))],
        (* else *) [Assign("v", Num(100.))] 
    );
    Expr(Var("v"))
]

let%expect_test "blockIfElse_goIf" = 
    let c = [Assign("v", Num(20.))]@ifelse1 in
    let _ = evalCode c [] in
    [%expect {| 1. |}]

let%expect_test "blockIfElse_goElse" = 
    let c = [Assign("v", Num(5.))]@ifelse1 in
    let _ = evalCode c [] in
    [%expect {| 100. |}]



let while1: block = [
    Assign("counter", Num(5.));
    While(Op2("<", Var("counter"), Num(10.0)),
         [
             Assign("v", Op2("+", Var("v"), Num(1.)));
             Assign("counter", Op2("+", Var("counter"), Num(1.)));
         ]);
    Expr(Var("v"))
]

let while2: block = [
    Assign("counter", Num(0.));
    While(Op2("<", Var("counter"), Num(10.0)),
         [
             Assign("v", Op2("*", Var("v"), Num(2.)));
             Assign("counter", Op2("+", Var("counter"), Num(1.)));
         ]);
    Expr(Var("v"))
]

let%expect_test "blockWhile_test1" = 
    let c = [Assign("v", Num(0.))]@while1 in
    let _ = evalCode c [] in
    [%expect {| 5. |}]

let%expect_test "blockWhile_test2" = 
    let c = [Assign("v", Num(1.))]@while2 in
    let _ = evalCode c [] in
    [%expect {| 1024. |}]



let forloop1: block = [
    Assign("v", Num(1.));
    For(Assign("counter", Num(0.)),                                 (* init loop *)
        Op2("<", Var("counter"), Num(10.0)),                        (* loop condition *)
        Assign("counter", Op2("+", Var("counter"), Num(1.))),       (* loop maintenance *)
        [                                                           (* loop body *)
            Assign("v", Op2("*", Var("v"), Num(2.)));
        ]);
    Expr(Var("v"))
]

let%expect_test "foorLoop_test1" =
    let _ = evalCode forloop1 [] in
    [%expect {| 1024. |}]


let forloop2_nested: block = [
    Assign("v", Num(1.));
    For(Assign("counter_inside", Num(0.)),                                 (* init loop *)
        Op2("<", Var("counter_inside"), Num(4.0)),                        (* loop condition *)
        Assign("counter_inside", Op2("+", Var("counter_inside"), Num(1.))),       (* loop maintenance *)
        [   
            For(Assign("counter_outside", Num(0.)),                                 (* init loop *)
            Op2("<", Var("counter_outside"), Num(5.0)),                        (* loop condition *)
            Assign("counter_outside", Op2("+", Var("counter_outside"), Num(1.))),       (* loop maintenance *)
            [                                                           (* loop body *)
                Assign("v", Op2("*", Var("v"), Num(2.)));
            ]);                                                        (* loop body *)
        ]);
    Expr(Var("v"))
]

let%expect_test "foorLoop_test2_nested" =
    let _ = evalCode forloop2_nested [] in
    [%expect {| 1048576. |}]






let p1: block = [
    Assign("v", Num(1.0));
    Expr(Var("v")) 
]

(* 
    v = 10; 
    v // display v
 *)
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    let _ = evalCode p1 [] in
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

(*let p2: block = [
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

let%expect_test "p2" =
    let _ = evalCode p2 [] in
    [%expect {| 3628800. |}]*)

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

(*let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]*)



