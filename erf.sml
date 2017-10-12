structure Erf : ERF = struct

    val tol = 1.0e~8
    val r = Real.fromInt
    val cutoff = 5.2

   fun nonepow(n: int): real =
        if n mod 2 = 0 then 1.0 else ~1.0

   val reciprtpi:real = 2.0/Math.sqrt(Math.pi)
   val large:real = 4.0

    fun toPS(x: real):string =
      if x >= 0.0 then Real.fmt (StringCvt.FIX NONE) (x)
      else "-" ^ (Real.fmt (StringCvt.FIX NONE) (Real.abs(x)))

   fun maclaurin (z:real) =
        let 
            (* z^(2n+1)/n! *)
            fun product (n:int):real =
                if n = 0 then z
                else product(n-1) * z * z / r(n)
            (* MacLaurin term n *)
            fun maclaurin_term n =
                product(n) * (nonepow n)/r(2*n+1)
            (* sum of terms n.. from MacLaurin series *)
            fun maclaurin_sum (n:int) =
                let val x = maclaurin_term n in
                    if abs(x) < tol
                        then x
                        else x + maclaurin_sum (n+1)
                end
        in
            maclaurin_sum 0
        end

    fun erf z =
        if z >= cutoff then 1.0
        else if z <= ~cutoff then ~1.0
        else reciprtpi * maclaurin z
end
