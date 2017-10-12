structure Eqn = struct
    open Interval

    exception EvalError of string

    datatype expr =
	  Const of real
	| Interval of interval
	| X
	| Y
	| Times of expr * expr
	| Pow of expr * int
	| Abs of expr
	| Plus of expr * expr
	| Neg of expr
	| Recip of expr
	| Sqrt of expr
	| Cos of expr
	| Sinc of expr
	| Tan of expr
	| Exp of expr
	| Ln of expr
	| Asin of expr
	| Atan of expr
	| Atan2 of expr * expr
        | Tanh of expr
	| Erf of expr
	| Floor of expr
	| Mod of expr * expr
	| Ltz of expr
	| Var of string
	| Apply of string * expr list
	| Cond of formula * expr * expr

    and formula = 
    	  Equation of expr
	| And of formula * formula
	| Or of formula * formula

    type defn = { name: string,
                  formals: string list,
		  body: expr }
    type venv = string->interval
    type env = {
	vars: venv,
	funs: string->defn,
	x: interval,
	y: interval
    }

    fun empty_env x = raise EvalError ("Unbound variable " ^ x)

    fun lookup_var {vars, funs, x, y} n = vars n

    fun extend vars (x: string) v =
	fn y => if x = y then v else vars y

    fun lookup_defn (defns: defn list) (f:string) : defn =
      case List.find (fn {name, formals, body} => name = f) defns of
	NONE => raise EvalError("Undefined variable or function "^f)
      | SOME d => d

    fun typecheck (e: expr) (vars: string list) = true

    val s = Real.toString
    fun si(x0, x1, y0, y1) =
		  "[" ^ (s x0) ^ "," ^ (s x1) ^ "] x [" ^
		        (s y0) ^ "," ^ (s y1) ^ "]"

    fun eval(e: expr, env as {vars=_, funs=_, x, y}): interval = case e of
      Const c => Interval.const(c)
    | Interval i => i
    | X => x
    | Y => y
    | Abs(e1) => abs(eval(e1,env))
    | Floor(e1) => floor(eval(e1,env))
    | Plus(e1, e2) => plus(eval(e1,env), eval(e2,env))
    | Times(e1, e2) => times(eval(e1,env), eval(e2,env))
    | Neg(e1) => neg(eval(e1,env))
    | Mod(e1, e2) => mod_(eval(e1,env), eval(e2,env))
    | Pow(e1, n) => pow(eval(e1,env), n)
    | Recip(e1) => recip(eval(e1,env))
    | Sqrt(e1) => sqrt(eval(e1,env))
    | Asin(e1) => asin(eval(e1,env))
    | Atan(e1) => atan(eval(e1,env))
    | Atan2(e1, e2) => atan2(eval(e1,env), eval(e2,env))
    | Sinc(e) => sinc(eval(e,env))
    | Tanh(e) => tanh(eval(e,env))
    | Erf(e) => erf(eval(e,env))
    | Exp(e1) => exp(eval(e1,env))
    | Ln(e1) => ln(eval(e1,env))
    | Cos(e1) => cos(eval(e1,env))
    | Tan(e1) => tan(eval(e1,env))
    | Ltz(e1) => ltz(eval(e1,env))
    | Var(x) => ((lookup_var env x)
                   handle EvalError(e) => apply x [] env)
    | Apply(f, args) => apply f args env
    | Cond(frm, e1, e2) => ifthenelse(frm, e1, e2) env

    and apply f args (env as {vars, funs, x, y}) : interval =
	let val {name=_, formals, body} = lookup_defn funs f
	in
	    if length formals <> length args then
		raise EvalError ("Function " ^ f ^ " applied to " ^
		                 Int.toString(length args) ^
				 " arguments, expected " ^
				 Int.toString(length formals))
	    else ();
	    let val vars':venv = 
		foldl (fn ((formal:string, arg:expr), vars'': venv) =>
		          extend vars'' formal (eval (arg, env)))
		    vars (ListPair.zip(formals, args))
	    in
		eval(body, {vars=vars', funs=funs, x=x, y=y})
	    end
	end

    and ifthenelse(f, e1, e2) env: interval = 
      let val (may, must, fin) = check_zeros(f, env)
      in
        union(if may then eval(e1, env) else Empty,
	      if must then Empty else eval(e2, env))
      end

    (* check_zeros(f,{funs, vars, x, y}) is (may, must, fin) where "may" is whether
       f _may_ take on the value zero in the x*y interval, "must" is
       whether f _must_ take on that value, and "fin" is whether the
       result is a "finite" interval. *)
    and check_zeros(f: formula, env as {funs, vars, x, y}):
	bool*bool*bool =
    case f of
      Equation(expr) => let val r = eval(expr, env) in
      			  (contains_zero(r), equals(r, 0.0), finite(r))
			end
    | And(f1, f2) =>
        let val (m1, M1, F1) = check_zeros(f1, env)
	    val (m2, M2, F2) = check_zeros(f2, env)
	in
	    (m1 andalso m2, M1 andalso M2, F1 andalso F2)
	end
    | Or(f1, f2) =>
        let val (m1, M1, F1) = check_zeros(f1, env)
	    val (m2, M2, F2) = check_zeros(f2, env)
	in
	    (m1 orelse m2, M1 orelse M2, F1 orelse F2)
	end
end
