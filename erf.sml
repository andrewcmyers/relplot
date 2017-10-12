structure Erf : ERF = struct

    val tol = 1.0e~25
    val r = Real.fromInt
    val cutoff = 5.2

   fun nonepow(n: int): real =
        if n mod 2 = 0 then 1.0 else ~1.0

   val reciprtpi:real = 2.0/Math.sqrt(Math.pi)
   val large:real = 4.0

    fun toPS(x: real):string =
      if x >= 0.0 then Real.fmt (StringCvt.FIX NONE) (x)
      else "-" ^ (Real.fmt (StringCvt.FIX NONE) (Real.abs(x)))

   val recips = Vector.tabulate(1000, fn x => 1.0/r(x))

   fun maclaurin (z:real) =
        let 
            (* z^(2n+1)/n! *)
            val z2 = z * z
            fun product (n:int):real =
                if n = 0 then z
                else product(n-1) * z2 * Vector.sub(recips, n)
            (* MacLaurin term n *)
            fun maclaurin_term n =
                product(n) * (nonepow n) * Vector.sub(recips, 2*n+1)
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
        else let val y = reciprtpi * maclaurin z
            in
               if y > 1.0 then 1.0
               else if y < ~1.0 then ~1.0
               else y
            end
end
