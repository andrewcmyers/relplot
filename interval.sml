structure Interval = struct

    open Erf

    datatype interval =
	Empty | Nonempty of real * real

    val eps=1.0e~7
    val neareps=1.0e~6

    fun close(r0, r1) = let val d = Real.abs(r1-r0) in
      d < eps orelse d < eps*(Real.abs(r0) + eps)
    end

    fun near(r0, r1) = let val d = Real.abs(r1-r0) in
      d < neareps orelse d < neareps*(Real.abs(r0) + neareps)
    end

    fun toString i: string =
	case i of
	  Empty => "(empty)"
	| Nonempty (a,b) => "[" ^ (Real.toString a) ^ "," ^
				     (Real.toString b) ^ "]"

    fun create(a0, a1) =
        if a0 <= a1 then Nonempty (a0, a1)
        else Empty

    fun contains_zero(i: interval): bool =
    case i of
        Empty => false
    |   Nonempty (a0, a1) => a0 <= 0.0 andalso a1 >= 0.0

    fun finite(i: interval): bool = 
    case i of
      Empty => false
    | Nonempty (a0, a1) => Real.isFinite(a0) andalso Real.isFinite(a1)

    fun top(i: interval): real option = case i of
      Empty => NONE
    | Nonempty (a0,a1) => SOME a1

    fun bottom(i: interval): real option = case i of
      Empty => NONE
    | Nonempty (a0,a1) => SOME a0

    fun equals(i: interval, r: real) = case i of
      Empty => false
    | Nonempty(a0,a1) => close(a0,r) andalso close(a1,r)

    fun repOK(Empty) = true
      | repOK(Nonempty(a0, a1)) = a0 <= a1
				  (* Note: nan will not satisfy this test *)

    fun lift1(f: real*real->real*real) =
      fn r => case r of
        Empty => Empty
      | Nonempty(a0, a1) => Nonempty(f(a0,a1))

    fun lift1'(f: real*real->interval) : interval -> interval =
      fn r => case r of
        Empty => Empty
      | Nonempty(a0, a1) => f(a0,a1)

    fun lift2(f) =
      fn (r1, r2) => case (r1, r2) of
        (Empty, _) => Empty
      | (_, Empty) => Empty
      | (Nonempty(a0, a1),
         Nonempty(b0, b1)) => Nonempty(f((a0,a1), (b0,b1)))

    fun union(Nonempty(a0, a1), Nonempty(b0, b1)) =
	Nonempty(Real.min(a0, b0), Real.max(a1, b1))
      | union(Empty, b) = b
     | union(a, Empty) = a

    val plus = lift2 (fn ((a0, a1), (b0, b1)) =>
	let
	    val c0 = a0 + b0
	    val c1 = a1 + b1
	    val c0' = if Real.isNan(c0) then Real.negInf else c0
	    val c1' = if Real.isNan(c1) then Real.posInf else c1
	in
	    (c0', c1')
	end)

    val neg = lift1 (fn (a0, a1) => (~a1, ~a0))

    val abs = lift1 (fn (a0, a1) =>
	if a0 >= 0.0 then (a0, a1)
	else if a1 <= 0.0 then (~a1, ~a0)
	else (0.0, (Real.max(~a0, a1))))

    val floor = lift1(fn (a0, a1) =>
        (Real.realFloor(a0),
	 Real.realFloor(a1))
    )

    val times = lift2 (fn ((a0, a1), (b0, b1)) => 
	if a0 >= 0.0 then
	    if b0 >= 0.0 then (a0*b0, a1*b1)
	    else if b1 <= 0.0 then (a1*b0, a0*b1)
	    else (a1*b0, a1*b1)
	else if a1 < 0.0 then
	    if b0 >= 0.0 then (a0*b1, a1*b0)
	    else if b1 <= 0.0 then (a1*b1, a0*b0)
	    else (a0*b1, a0*b0)
	else
	    if b0 >= 0.0 then (a0*b1, a1*b1)
	    else if b1 <= 0.0 then (a1*b0, a0*b0)
	    else (Real.min(a0*b1, a1*b0),
		  Real.max(a1*b1, a0*b0)))

    fun realpow(r:real, n:int): real =
      if n mod 2 = 0 then
	if n = 0 then 1.0 else
	    let val x = realpow(r, n div 2) in x*x end
      else
	r * realpow(r, n-1)

    fun pow(i, n:int): interval = case i of
      Empty => Empty
    | Nonempty(a0, a1) => 
	let
	    val a0' = Real.abs(a0)
	    val a1' = Real.abs(a1)
	    val tmax = Real.max(a0', a1')
	in
	    if n mod 2 = 0 then
		if (a0 < 0.0) <> (a1 < 0.0) then
		Nonempty (0.0, realpow(tmax, n))
	    else let
		val tmin = Real.min(a0',a1')
	    in
		Nonempty(realpow(tmin, n), realpow(tmax, n))
	    end
	else
	    Nonempty(realpow(a0, n), realpow(a1, n))
	end

    val recip = lift1' (fn (a0, a1) => 
      if Real.sign a0 = 0 andalso Real.sign a1 = 0 then Empty else
      if a0 > 0.0 orelse a1 < 0.0 then Nonempty(1.0/a1, 1.0/a0)
      else if a0 >= 0.0 andalso a1 > 0.0 then Nonempty(1.0/a1, Real.posInf)
      else if a0 < 0.0 andalso a1 <= 0.0 then Nonempty(Real.negInf, 1.0/a0)
      else Nonempty(Real.negInf, Real.posInf))

    fun mod_(a,b) =
	case plus(a, neg(times(b, floor(times(a, recip(b)))))) of
	    Empty => Empty
	  | Nonempty(c0, c1) =>
	    (case b of
		Nonempty(b0, b1) => Nonempty(Real.max(0.0, c0), Real.min(b1, c1))
	      | _ => raise Fail "empty b")

    fun sqrt(i) = case i of
      Empty => Empty
    | Nonempty(a0, a1) =>
      if a0 >= 0.0 then
	Nonempty(Real.Math.sqrt(a0), Real.Math.sqrt(a1))
      else if a1 < 0.0 then Empty
      else Nonempty(0.0, Real.Math.sqrt(a1))

    fun const(c:real):interval = Nonempty(c,c)

    val exp = lift1 (fn (a0, a1) => (Real.Math.exp(a0), Real.Math.exp(a1)))
    val erf = lift1 (fn (a0, a1) =>
                       let val b0 = Erf.erf(a0)
                           val b1 = Erf.erf(a1)
                       in
                           if b0 < b1 then (b0, b1)
                           else (b1, b0)
                       end)

    (* SML's tanh is broken for large x *)
    fun tanh_ext x =
        if x > 100.0 then 1.0
        else if x < ~100.0 then ~1.0
        else Math.tanh(x)

    val tanh = lift1 (fn (a0, a1) => (tanh_ext(a0), tanh_ext(a1)))

    fun ln(i) = case i of
      Empty => Empty 
    | Nonempty(a0, a1) =>
        if a1 <= 0.0 then Empty
	else if a0 <= 0.0 then Nonempty(Real.negInf, Real.Math.ln(a1))
			  else Nonempty(Real.Math.ln(a0), Real.Math.ln(a1))

 (* cos x = 1 at x = 2n pi
    cos x = -1 at x = (2n+1)pi *)

    val cosmin = Real.Math.pi * Real.fromInt(valOf(Int.minInt))
    val cosmax = Real.Math.pi * Real.fromInt(valOf(Int.maxInt))

    val cos = lift1(fn (a0, a1) => let
	val c0 = if a0 > Real.negInf then Real.Math.cos(a0)
		 else ~1.0
	val c1 = if a1 < Real.posInf then Real.Math.cos(a1)
	         else 1.0
        val n0:int = if a0 > cosmin
	  then Real.floor(a0/Real.Math.pi)
	  else valOf(Int.minInt)
        val n1:int = if a1 < cosmax
	  then Real.floor(a1/Real.Math.pi)
	  else valOf(Int.maxInt)
    in
    	if n0 = n1 then (* no min/max crossing *)
	    (Real.min(c0, c1), Real.max(c0, c1))
	else if n1 > n0 + 1 then (* cross 1 and -1 *)
	    (~1.0, 1.0)
	else (* n1 = n0 + 1 *)
	    if n0 mod 2 = 0 then
		(~1.0, Real.max(c0, c1))
	    else
		(Real.min(c0, c1), 1.0)
    end)

    fun sinc' x = if Real.sign(x) = 0 then 1.0
                else Math.sin(x)/x

    fun sinc (i as Nonempty(a0, a1)) = let
        val mx = Real.max(Real.abs(a0), Real.abs(a1)) 
        val mn = Real.min(Real.abs(a0), Real.abs(a1)) 
    in
        if mx <= Math.pi * 1.4 then 
            create (sinc' mx, if contains_zero i then 1.0 else sinc' mn)
        else
            times(cos(plus(i, const(~Math.pi/2.0))), recip(i))
    end
    | sinc Empty = Empty

    val ltz = lift1(fn(a0, a1) => 
      case (a0 <= 0.0, a1 <= 0.0) of
        (true, true) =>  (0.0, 0.0)
      | (true, false) => (0.0, a1)
      | (false, _) => (a0, a1))

    fun asin(a) =
	case a
	 of Empty => Empty
	  | Nonempty(a0, a1) =>
	      if a0 < 1.0 andalso a1 > ~1.0 then
		Nonempty(Math.asin(Real.max(a0, ~1.0)),
			Math.asin(Real.min(a1, 1.0)))
	      else 
		Empty

    val tan = lift1'(fn(a0:real, a1:real) => let
	val t0 = if a0 > Real.negInf then Real.Math.tan(a0) else 0.0 
	val t1 = if a1 < Real.posInf then Real.Math.tan(a1) else 0.0
	val g0 = Real.floor((a0 + Real.Math.pi * 0.5)/ Real.Math.pi)
	val g1 = Real.floor((a1 + Real.Math.pi * 0.5)/ Real.Math.pi)
    in
        if g0 <> g1 then Nonempty(Real.negInf, Real.posInf)
	else if close(a0, a1) then if Real.isFinite(t0) then Nonempty(t0, t0)
						        else Empty
			else if t0 < t1 then Nonempty(t0, t1)
			                else Nonempty(t1, t0)
    end)

    val atan = lift1(fn (a0, a1) =>
      (Math.atan(a0), Math.atan(a1)))

    (* atan2 (y,x) gives the angle to point (x,y) in [-pi, pi] *)
    val atan2 = lift2(fn ((y0, y1), (x0, x1)) =>
      if x0 <= 0.0 andalso x1 > 0.0 andalso y0 < 0.0 andalso y1 > 0.0
	then
	  (~Math.pi, Math.pi)
	else
	  let val (minx, miny) =
	       (     if x1 >  0.0 andalso y0 >= 0.0 then (x1, y0)
	        else if x0 >= 0.0 andalso y0 <  0.0 then (x0, y0)
	        else if x0 <  0.0 andalso y1 <= 0.0 then (x0, y1)
	        else if x1 <= 0.0 andalso y1 >  0.0 then (x1, y1)
	        else raise Fail "unhandled min case in atan2")
	      val (maxx, maxy) =
	       (     if x1 <= 0.0 andalso y0 <  0.0 then (x1, y0)
	        else if x1 >  0.0 andalso y1 <= 0.0 then (x1, y1)
	        else if x0 >= 0.0 andalso y1 >  0.0 then (x0, y1)
	        else if x0 <  0.0 andalso y0 >= 0.0 then (x0, y0)
	        else raise Fail "unhandled max case in atan2")
	      val mn = Math.atan2(miny, minx)
	      val mx = Math.atan2(maxy, maxx)
	   in
	     if mn <= mx then (mn, mx)
	     else (mn, mx + 2.0*Math.pi)
	     (* else (~Math.pi, Math.pi) *)
	   end
    )
end
