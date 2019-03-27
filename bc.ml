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
    | Break
    | Continue
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list
    | Unkown of string

and bindable = 
    | FctBody of string list * statement list
    | Num of float

and env = (string, bindable, String.comparator_witness) Map.t
and stack = env list

type block = statement list 

exception NotImplemented of string
exception MissingScope of string
exception InvalidOperator of string
exception UnkownStatement of string
exception ParametersMissmatch of string

exception ReturnValue of float
exception BreakSignal of stack
exception ContinueSignal of stack



let defaultExpr : expr = Num(1.0)

let bindFunction (name: string) (data: bindable) (env: stack): stack =
    match List.rev env with (* functions always bind to the global namespace*) 
    | hd :: tl -> List.rev ( Map.set hd ~key:name ~data:data :: tl )
    | _ -> raise (MissingScope ("No global scope provided to bind function '" ^ name ^ "'") )

let bindValue (name: string) (data: float) (env: stack): stack =
    match env with
    | hd :: tl -> ( 
        match Map.find hd name with
        | Some _ -> Map.set hd ~key:name ~data:(Num(data)) :: tl
        | None ->
            match env with
            | hd :: tl -> List.rev (Map.set hd ~key:name ~data:(Num(data)) :: tl)
            | _ -> raise (MissingScope "No global scope provided to bind function")
    )
    | _ -> raise (MissingScope "No global scope provided to bind function") (* should never be empty *)

let bind (name: string) (obj: bindable) (env: stack): stack =
    match obj with
    | Num(value) -> bindValue name value env
    | FctBody(_, _) -> bindFunction name obj env

let fetch (name: string) (env: stack): bindable =
    match env with
    | [] -> raise ( MissingScope("No scope provided to fetch name '" ^ name ^ "'") )
    | hd :: _ ->
        match Map.find hd name with
        | Some value -> value
        | None ->
            match List.rev env with
            | [] -> Num(0.)
            | hd :: _ -> match Map.find hd name with
                | Some value -> value
                | None       -> Num(0.)

let fetchValue (name: string) (env: stack): float =
    match fetch name env with
    | Num(value) -> value
    | _ -> 0.


(* evaluation functions *) 

let rec evalCode (code: block) (env: stack): stack =
    match env with
    | [] -> evalCode code [ Map.of_alist_exn (module String) [] ]
    | _  -> List.fold ~init:env ~f:(fun acc stat -> evalStatement stat acc) code

and evalStatement (stat: statement) (env: stack): stack =
    match stat with 
        | Assign(name, exp) -> bindValue name (evalExpr exp env) env
        | Expr(exp) -> evalExpr exp env |> printf "%F\n"; env
        | If(cond, codeIf, codeElse) -> 
            if compare (evalExpr cond env) 1. = 0 
                then evalCode codeIf env 
                else evalCode codeElse env
        | Return(exp) -> raise (ReturnValue (evalExpr exp env))
        | While(cond, code) -> whileLoop cond code env
        | For(init, cond, maint, code) -> 
            let env = evalStatement init env in
            forLoop cond maint code env
        | Break -> raise (BreakSignal(env))
        | Continue -> raise (ContinueSignal(env))
        | FctDef(name, params, body) -> bindFunction name (FctBody(params, body)) env
        | Unkown(str) -> raise ( UnkownStatement("Unkown statement: " ^ str))

and evalExpr (exp: expr) (env: stack): float =
    match exp with
    | Num(value) -> value
    | Var(name) -> fetchValue name env
    | Op1(op, e) -> ( (* treat expressions with 1 operand *)
        match op with
        | "-"  -> evalExpr e env *. -1.
        | "++" -> evalExpr e env +.  1.
        | "--" -> evalExpr e env -.  1.
        | "!"  -> if (compare (evalExpr e env) 0.)=0 then 1. else 0.
        | _    -> raise ( InvalidOperator( "Invalid or non implemented unary operator: " ^ op ))
    )
    | Op2(op, el, er) -> ( (* treat expressions with 2 operands *)
        match op with
        | "^"  -> evalExpr el env ** evalExpr er env
        | "/"  -> evalExpr el env /. evalExpr er env
        | "*"  -> evalExpr el env *. evalExpr er env
        | "-"  -> evalExpr el env -. evalExpr er env
        | "+"  -> evalExpr el env +. evalExpr er env
        | "==" -> if compare(evalExpr el env) (evalExpr er env) = 0 then 1. else 0.
        | "!=" -> if compare(evalExpr el env) (evalExpr er env) = 0 then 0. else 1.
        | "<" -> if compare(evalExpr el env) (evalExpr er env) < 0 then 1. else 0.
        | "<=" -> if compare(evalExpr el env) (evalExpr er env) <= 0 then 1. else 0.
        | ">" -> if compare(evalExpr el env) (evalExpr er env) > 0 then 1. else 0.
        | ">=" -> if compare(evalExpr el env) (evalExpr er env) >= 0 then 1. else 0.
        | "&&" -> if (compare (evalExpr el env) 1. = 0 && compare (evalExpr er env) 1. = 0) then 1. else 0.
        | "||" -> if (compare (evalExpr el env) 1. = 0 || compare (evalExpr er env) 1. = 0) then 1. else 0.
        | _ -> raise ( InvalidOperator( "Invalid or non implemented binary operator:" ^ op ))
    )
    | Fct(name, args) -> (
        match fetch name env with 
        | FctBody(params, body) -> (
            let frame = bindArguments params args env@[ Map.of_alist_exn (module String) [] ] in
            try let _ = evalCode body frame in 0.
            with ReturnValue(value) -> value
        )
        | _ -> raise (NotImplemented ("Function " ^ name ^ " not defined"))
    )

and whileLoop (cond: expr) (code: block) (env: stack) : stack =
    if compare (evalExpr cond env) 1. = 0 then
        try
            let env = evalCode code env in whileLoop cond code env
        with 
            | ContinueSignal(env) -> whileLoop cond code env;
            | BreakSignal(env) -> env 
    else env

and forLoop (cond: expr) (maint: statement) (code: block) (env: stack) : stack =
    if compare (evalExpr cond env) 1. = 0 then
        try
            let env = evalCode code env in
            let env = evalStatement maint env in forLoop cond maint code env;
        with 
            | ContinueSignal(env) ->  let env = evalStatement maint env in forLoop cond maint code env;
            | BreakSignal(env) -> env
    else env

and bindArguments (params: string list) (args: expr list) (env: stack) : stack =
    match params, args with
    | hd_params::tl_params, hd_args::tl_args -> (
        let env = evalStatement (Assign(hd_params, hd_args)) env in
        bindArguments tl_params tl_args env
    )
    | [], [] -> env
    | _ -> raise (ParametersMissmatch "Arguments missmatch")



(* TESTS ------------------------------------------------------------------------------- *)

let _test_env = List.rev [ Map.of_alist_exn (module String) ["var1", Num(10.); "var2", Num(20.)];
                           Map.of_alist_exn (module String) ["var3", Num(11.);                 ]; 
                           Map.of_alist_exn (module String) ["var1", Num(12.); "var5", Num(22.)]]

let%expect_test "fetchValue_globalScope" =
    fetchValue "var2" _test_env |> printf "%F";
    [%expect {| 20. |}]

let%expect_test "fetchValue_notBound" =
    fetchValue "not_existant" _test_env |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "fetchValue_topScope" =
    fetchValue "var5" _test_env |> printf "%F";
    [%expect {| 22. |}]

let%expect_test "fetchValue_shadowedGlobal" =
    fetchValue "var1" _test_env |> printf "%F";
    [%expect {| 12. |}]

let%expect_test "fetchValue_intermediateInacc" =
    fetchValue "var3" _test_env |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "fetchValue_noEnv" =
    try fetchValue "somename" [] |> printf "%F"
    with MissingScope(what)  -> print_string what;
    [%expect {| No scope provided to fetch name 'somename' |}]

let%expect_test "bindValue_onEmpty" =
    let env = bindValue "var_global" 0.5 [ Map.of_alist_exn (module String) [] ] in
    fetchValue "var_global" env |> printf "%F";
    [%expect {| 0.5 |}]

let%expect_test "bindValue_reset" =
    let env = bindValue "var_global" 0.5 [ Map.of_alist_exn (module String) ["var_global", Num(10.);] ] in
    fetchValue "var_global" env |> printf "%F";
    [%expect {| 0.5 |}]

let%expect_test "bindValue_resetShadowed" =
    let env = bindValue "var" 0.5 [ 
        Map.of_alist_exn (module String) ["var", Num(10.);];
        Map.of_alist_exn (module String) ["var", Num(12.);]] in
    fetchValue "var" env |> printf "%F";
    [%expect {| 0.5 |}]

let%expect_test "evalStatement_Assign1" =
    let env = [ Map.of_alist_exn (module String) ["global1", Num(0.); "global2", Num(20.)] ] in
    let env = evalStatement (Expr(Num(1.))) env in
    let _ = fetchValue "global1" env in (* the test is captured from std out *)
    print_string "";                    (* dummy *)
    [%expect {| 1. |}]

let%expect_test "evalExpr_Num" = 
    evalExpr (Num 10.) [] |> printf "%F";
    [%expect {| 10. |}]

let%expect_test "evalExpr_Op1_ArithmeticNegate" = 
    evalExpr (Op1 ("-", (Num 2.))) [] |> printf "%F";
    [%expect {| -2. |}]

let%expect_test "evalExpr_Op1_LogicNegate_True" = 
    evalExpr (Op1 ("!", (Num 1.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op1_LogicNegate_False" = 
    evalExpr (Op1 ("!", (Num 0.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op1_Increment" = 
    evalExpr (Op1 ("++", (Num 0.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op1_Decrement" = 
    evalExpr (Op1 ("--", (Num 0.))) [] |> printf "%F";
    [%expect {| -1. |}]

let%expect_test "evalExpr_Op2_Pow" = 
    evalExpr (Op2 ("^", (Num 2.), (Num 3.))) [] |> printf "%F";
    [%expect {| 8. |}]

let%expect_test "evalExpr_Op2_Div" = 
    evalExpr (Op2 ("/", (Num 5.), (Num 2.))) [] |> printf "%F";
    [%expect {| 2.5 |}]

let%expect_test "evalExpr_Op2_Mult" = 
    evalExpr (Op2 ("*", (Num 10.5), (Num 100.))) [] |> printf "%F";
    [%expect {| 1050. |}]

let%expect_test "evalExpr_Op2_Subtract" = 
    evalExpr (Op2 ("-", (Num 10.), (Num 25.5))) [] |> printf "%F";
    [%expect {| -15.5 |}]

let%expect_test "evalExpr_Op2_Add" = 
    evalExpr (Op2 ("+", (Num 10.), (Num 25.5))) [] |> printf "%F";
    [%expect {| 35.5 |}]

let%expect_test "evalExpr_Op2_Equal_PositiveCase" = 
    evalExpr (Op2 ("==", (Num 10.), (Num 10.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_Equal_NegativeCase" = 
    evalExpr (Op2 ("==", (Num 11.), (Num 10.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_NotEqual_PositiveCase" = 
    evalExpr (Op2 ("!=", (Num 11.), (Num 10.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_NotEqual_NegativeCase" = 
    evalExpr (Op2 ("!=", (Num 10.), (Num 10.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_LessThan_PositiveCase" = 
    evalExpr (Op2 ("<", (Num 1.), (Num 2.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_LessThan_NegativeCase" = 
    evalExpr (Op2 ("<", (Num 2.), (Num 1.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_LessThanOrEqual_PositiveCase" = 
    evalExpr (Op2 ("<=", (Num 1.), (Num 2.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_LessThanOrEqual_NegativeCase" = 
    evalExpr (Op2 ("<=", (Num 2.), (Num 1.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_GreaterThan_PositiveCase" = 
    evalExpr (Op2 (">", (Num 4.), (Num 2.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_GreaterThan_NegativeCase" = 
    evalExpr (Op2 (">", (Num 1.), (Num 2.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_GreaterThanOrEqual_PositiveCase1" = 
    evalExpr (Op2 (">=", (Num 2.), (Num 2.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_GreaterThanOrEqual_PositiveCase2" = 
    evalExpr (Op2 (">=", (Num 4.), (Num 2.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_GreaterThanOrEqual_NegativeCase" = 
    evalExpr (Op2 (">=", (Num 1.), (Num 2.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_And_CaseTrue" = 
    evalExpr (Op2 ("&&", (Num 1.), (Num 1.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_And_CaseFalse1" = 
    evalExpr (Op2 ("&&", (Num 1.), (Num 0.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_And_CaseFalse2" = 
    evalExpr (Op2 ("&&", (Num 0.), (Num 1.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_And_CaseFalse3" = 
    evalExpr (Op2 ("&&", (Num 0.), (Num 0.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_Or_CaseFalse" = 
    evalExpr (Op2 ("||", (Num 0.), (Num 0.))) [] |> printf "%F";
    [%expect {| 0. |}]

let%expect_test "evalExpr_Op2_Or_CaseTrue1" = 
    evalExpr (Op2 ("||", (Num 1.), (Num 0.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_Or_CaseTrue2" = 
    evalExpr (Op2 ("||", (Num 0.), (Num 1.))) [] |> printf "%F";
    [%expect {| 1. |}]

let%expect_test "evalExpr_Op2_Or_CaseTrue3" = 
    evalExpr (Op2 ("||", (Num 1.), (Num 1.))) [] |> printf "%F";
    [%expect {| 1. |}]

let if1: block = [
    If(
        Op2(">", Var("v"), Num(10.0)),
        [Assign("v", Num(1.0))],
        [] (* no else *)
    );
    Expr(Var("v"))
]

let%expect_test "evalCode_If_positive" =
    let _ = evalCode ([Assign("v", Num(5.))]@if1) [] in
    [%expect {| 5. |}]

let%expect_test "evalCode_If_negative" =
    let _ = evalCode ([Assign("v", Num(20.))]@if1) [] in
    [%expect {| 1. |}]

let ifelse1: block = [
    If(
        (* if *)   Op2(">", Var("v"), Num(10.0)),
        (* then *) [Assign("v", Num(1.))],
        (* else *) [Assign("v", Num(100.))] 
    );
    Expr(Var("v"))
]

let%expect_test "evalCode_IfElse_if" =
    let _ = evalCode ([Assign("v", Num(20.))]@ifelse1) [] in
    [%expect {| 1. |}]

let%expect_test "evalCode_IfElse_else" =
    let _ = evalCode ([Assign("v", Num(5.))]@ifelse1) [] in
    [%expect {| 100. |}]

let __while_base1: block = [
    While(Op2("<", Var("counter"), Num(10.0)), [
             Assign("v", Op2("*", Var("v"), Num(2.)));
             Assign("counter", Op2("+", Var("counter"), Num(1.)));
         ]);
    Expr(Var("v"))
]

let __while_base2: block = [
    While(Op2("<", Var("counter"), Num(10.0)), [
             Assign("counter", Op2("+", Var("counter"), Num(1.)));
             If(Op2(">", Var("counter"), Num(5.0)),
                [Continue],
                []);
             Assign("v", Op2("*", Var("v"), Num(2.)))
         ]);
    Expr(Var("v"))
]

let __while_base3: block = [
    While(Op2("<", Var("counter"), Num(10.0)), [
             Assign("counter", Op2("+", Var("counter"), Num(1.)));
             Expr(Var("v"));
             If(Op2(">", Var("counter"), Num(5.0)),
                [Break],
                []);
             Assign("v", Op2("*", Var("v"), Num(2.)));
         ]);
]

let while1: block = [Assign("v", Num(1.)); Assign("counter", Num(5. ));]@__while_base1
let while2: block = [Assign("v", Num(1.)); Assign("counter", Num(0. ));]@__while_base1
let while3: block = [Assign("v", Num(1.)); Assign("counter", Num(10. ));]@__while_base1
let while4: block = [Assign("v", Num(1.)); Assign("counter", Num(0. ));]@__while_base2
let while5: block = [Assign("v", Num(1.)); Assign("counter", Num(0. ));]@__while_base3

let%expect_test "evalCode_While_1" =
    let _ = evalCode while1 [] in
    [%expect {| 32. |}]

let%expect_test "evalCode_While_2" =
    let _ = evalCode while2 [] in
    [%expect {| 1024. |}]

let%expect_test "evalCode_While_condFailsFirst" =
    let _ = evalCode while3 [] in
    [%expect {| 1. |}]

let%expect_test "evalCode_While_withContinue" =
    let _ = evalCode while4 [] in
    [%expect {| 32. |}]

let%expect_test "evalCode_While_withBreak" =
    let _ = evalCode while5 [] in
    [%expect {| 
                1.
                2.
                4.
                8.
                16.
                32. 
            |}]


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

let%expect_test "evalCode_For" =
    let _ = evalCode forloop1 [] in
    [%expect {| 1024. |}]

let forloop2_nested: block = [
    Assign("v", Num(1.));
    For(Assign("counter_inside", Num(0.)),                                  (* init loop *)
        Op2("<", Var("counter_inside"), Num(4.0)),                          (* loop condition *)
        Assign("counter_inside", Op2("+", Var("counter_inside"), Num(1.))), (* loop maintenance *)
        [   
            For(Assign("counter_outside", Num(0.)),                                     (* init loop *)
            Op2("<", Var("counter_outside"), Num(5.0)),                                 (* loop condition *)
            Assign("counter_outside", Op2("+", Var("counter_outside"), Num(1.))),       (* loop maintenance *)
            [                                                                           (* loop body *)
                Assign("v", Op2("*", Var("v"), Num(2.)));
            ]);                                                                         
        ]);
    Expr(Var("v"))
]

let%expect_test "evalCode_For_Nested" =
    let _ = evalCode forloop2_nested [] in
    [%expect {| 1048576. |}]

let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    let _ = evalCode p1 [] in
    [%expect {| 1. |}]

let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<=", Var("i"), Num(10.0)),
            Assign("i", Op1("++", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p2" =
    let _ = evalCode p2 [] in
    [%expect {| 3628800. |}]


let simple_function: block = [
    Assign("v", Num(5.));
    FctDef("simple_function", [], [
        Expr(Var("v"));
        Assign("v", Num(25.));
        Expr(Var("v"));
        Return(Var("v"))
    ]);
    Expr(Fct("simple_function", []));
]

let%expect_test "Fct_simpleFunction" =
    let _ = evalCode simple_function [] in 
    [%expect {| 
        5.
        25.
        25.
    |}]

let fibo30: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(2.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        For(
            Assign("i", Num(0.)),
            Op2("<", Var("i"), Num(30.)),
            Assign("i", Op1("++", Var("i"))), 
            [ Expr(Fct("f", [Var("i")])) ]
        )
    ]

let%expect_test "Fct_fibo_rec_from_0_to_30" =
    let _ = evalCode fibo30 [] in 
    [%expect {|
        1.
        1.
        2.
        3.
        5.
        8.
        13.
        21.
        34.
        55.
        89.
        144.
        233.
        377.
        610.
        987.
        1597.
        2584.
        4181.
        6765.
        10946.
        17711.
        28657.
        46368.
        75025.
        121393.
        196418.
        317811.
        514229.
        832040.
    |}]

    