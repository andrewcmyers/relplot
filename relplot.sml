structure RelPlot = struct

    open Interval
    open Eqn

    val debug = false

    fun toPS(x: real):string =
      if Real.compare(x, 0.0) = GREATER then Real.fmt (StringCvt.FIX NONE) (x)
      else if Real.compare(x, 0.0) = LESS then "-" ^ (Real.fmt (StringCvt.FIX NONE) (~x))
      else "0.0"

    type real_interval = real*real

    fun range(x0:real, x1: real) = Nonempty(x0, x1)
    fun foreach l f = List.app f l
    fun fold l x f = foldl f x l

    type point = real*real
    type box = real_interval * real_interval
    datatype rendered_item = Line of point*point
			   | Box of box

    type id_item = int * rendered_item

    structure Items = RedBlackMapFn(struct
	type ord_key = int
	val compare = Int.compare
    end)
    fun point_compare ((x1, y1), (x2, y2)) =
	(case Real.compare(x1, x2) of
	    EQUAL => Real.compare(y1, y2)
	  | other => other)

    structure Lines = RedBlackMapFn(struct
	type ord_key = point
	val compare = point_compare
    end)

    structure Ids = RedBlackSetFn(struct
	type ord_key = int
	val compare = Int.compare
    end)

    type curve = point list

    (* A point between a and b.
       Requires: a < b *)
    fun midpoint (a:real) (b:real) : real =  (a+b)/2.0
    (*
      if Real.isFinite(a) then
	if Real.isFinite(b) then (a + b) * 0.5
	else a + Real.abs(a) + 1.0
      else
	if Real.isFinite(b) then b - Real.abs(b) - 1.0
	else 0.0
    *)
	 
    fun render(formula:formula, x: real_interval, y: real_interval, res: real,
	      curr_list: rendered_item list,
	      funs: defn list): rendered_item list =
	let val (x0, x1) = x
	    val (y0, y1) = y
            val _  = if debug then print((toPS x0) ^ " " ^ (toPS x1) ^ " "
                                    ^ (toPS y0) ^ " " ^ (toPS y1) ^ " frame\n")
                              else ()
	    val env = {vars = empty_env, funs = funs, x = range x, y = range y}
	    val (may_zero, must_zero, _) = (check_zeros(formula, env)
	      handle EvalError(msg) => (
		print("(Evaluation error: " ^ msg ^ ") reporterror\n");
		raise Fail msg
	      )
	      handle Fail(msg) => (
		print("% trouble with [" ^
		  (s x0) ^ "," ^ (s x1) ^ "] x [" ^
		  (s y0) ^ "," ^ (s y1) ^ "]: " ^
		  msg ^ "\n");
		raise Fail msg
	      ))

            fun close(x,y,x',y') = Interval.close(x, x') andalso
		                   Interval.close(y, y')
            fun near(x,y,x',y') = Interval.near(x, x') andalso
		                  Interval.near(y, y')
            fun left(x) = if Real.compare(x, 0.0) = EQUAL then ~0.0 else x

            (* Find a single zero along the line (x1,y1) - (x2,y2). z must be
             * the result of applying the function to this interval. *)
            fun find_zero(x1,y1, x2,y2, env as {funs, vars, x=_, y=_}): point option =
	    (let val (may, _, fin) =
		check_zeros(formula, {funs=funs, vars=vars, x=range(x1, x2), y=range(y1, y2)})
	     in
	      if may then
                let val xm = (x1+x2)*0.5
                    val ym = (y1+y2)*0.5
                in
		    if close(xm,ym, x1,y1) orelse close(xm,ym,x2,y2)
                    then
		      if fin then SOME (xm, ym) (* close enough! *)
		             else NONE (* looks like divergence *)
                    else case find_zero(x1, y1, xm, ym, env) of
			   pt as SOME _ => pt
			 | NONE => find_zero(xm, ym, x2, y2, env)
                end
              else NONE
	     end)

            fun fine_test(lst: rendered_item list) env = let
	      val intercepts = [find_zero(x0, y0, x0, y1, env),
				find_zero(x1, y0, x1, y1, env),
				find_zero(x0, y0, x1, y0, env),
				find_zero(x0, y1, x1, y1, env)]

	      val filtered = List.filter
		    (fn(z) => case z of NONE => false | _ => true) intercepts

	      val stripped = map (fn (xy) => case xy of SOME(p) => (p) |
	      					_ => raise Fail "filter")
				 filtered
	      fun contains_tol((x,y), lst: point list) = case lst of
		  [] => false
		| (x',y')::lst => near(x,y, x',y') orelse contains_tol((x,y),lst)

	      fun remove_dups(lst: point list) = case lst of
	        [] => []
	      | (x,y)::t =>
		  if contains_tol((x,y), t) then remove_dups(t)
					    else (x,y)::remove_dups(t)

	      val uniq_zeros = remove_dups(stripped)
	    in
                (*
	        List.app (fn (x, y) =>
				print ("% intersection at " ^ (Real.toString x) ^ "," ^ (Real.toString y) ^ "\n"))
		    uniq_zeros;
		print "\n"; *)
		case uniq_zeros of
		  (nil|[_]) => lst
		| [(x2,y2), (x3, y3)] => Line((x2, y2), (x3, y3))::lst
		| [(x2,y2), (x3, y3), (x4, y4)] =>
			Line((x2, y2), (x3, y3))::
			Line((x2, y2), (x4, y4))::
			Line((x3, y3), (x4, y4))::
			lst
		| [(x2,y2), (x3, y3), (x4, y4), (x5, y5)] =>
			Line((x2, y2), (x3, y3))::
			Line((x2, y2), (x4, y4))::
			Line((x3, y3), (x4, y4))::
			Line((x2, y2), (x5, y5))::
			Line((x3, y3), (x5, y5))::
			Line((x4, y4), (x5, y5))::lst

		| _ => (print ("%intersections: " ^ (Int.toString(length uniq_zeros)) ^ "\n"); Box(x,y)::lst)
	    end
        in
	    if must_zero then
		Box(x,y)::curr_list
	    else if may_zero then (
                if (x1 - x0 <= res) then (
                    fine_test(curr_list) env
                ) else let
                    val xm = (x0 + x1) * 0.5
                    val ym = (y0 + y1) * 0.5
                    val l1 = render(formula, (x0, left(xm)), (y0, left(ym)), res, curr_list, funs)
                    val l2 = render(formula, (x0, left(xm)), (ym, y1), res, l1, funs)
                    val l3 = render(formula, (xm, x1), (ym, y1), res, l2, funs)
                    val l4 = render(formula, (xm, x1), (y0, left(ym)), res, l3, funs)
                in
                    l4
                end
            ) else (
                curr_list
	    )
        end

    val color_names = ["black", "blue", "red", "green", "purple", "orange", "yellow", "magenta", "cyan"]

    val colors = [("black", 0.0, 0.0, 0.0),
		  ("blue", 0.0, 0.0, 0.6),
		  ("red", 0.8, 0.0 ,0.0),
		  ("green", 0.0, 0.5, 0.0),
		  ("purple", 0.5, 0.0, 0.5),
		  ("orange", 0.7, 0.5, 0.0),
		  ("yellow", 0.8, 0.8, 0.0),
		  ("magenta", 0.8, 0.0, 0.8),
		  ("cyan", 0.0, 0.8, 0.8)]

    fun setup_formula_render (counter:int, color, linetype, linewidth) = (
	print ("\n% Formula style: " ^ color ^ ", " ^ linetype ^ ", " ^ linewidth ^ " pt\n");
	case color of
	    "black" => print "0 0 0 setrgbcolor\n"
	|   "blue" => print "0 0 0.6 setrgbcolor\n"
	|   "red" => print "0.8 0 0 setrgbcolor\n"
	|   "green" => print "0 0.5 0 setrgbcolor\n"
	|   "purple" => print "0.5 0 0.5 setrgbcolor\n"
	|   "orange" => print "0.7 0.5 0 setrgbcolor\n"
	|   "yellow" => print "0.8 0.8 0 setrgbcolor\n"
	|   "magenta" => print "0.8 0 0.8 setrgbcolor\n"
	|   "cyan" => print "0 0.8 0.8 setrgbcolor\n"
	|   _ => print "autocolor\n";

	print (linewidth ^ " setlw\n");
	print (case linetype of
		 "solid" => "solid\n"
	       | "dashed" => "dashed\n"
	       |  _ => "dotted\n")
    )

    fun curves(formulas, x: real_interval, y: real_interval, res: real,
		funs: defn list)
    	: rendered_item list list =
      map (fn f => render(f, x, y, res, [], funs)) formulas

    type id = int

    fun restitch(renders: rendered_item list) : curve list * rendered_item list =
	let
	    (* Give each rendered item a unique id *)
	    val (_, rl) = fold renders (0, []) (fn (r, (i, rl)) => (i+1, (i,r) :: rl))
	    val _ = print ("% render list contains " ^ (Int.toString (length rl)) ^ " items\n")
	    (* Create map from line endpoints to line ids *)
	    val lines = 
		fold rl Lines.empty (fn ((id, r): id_item, lines) =>
		    let fun add_id lines p id =
			case Lines.find(lines, p) of
			  SOME ids => Lines.insert (lines, p, (Ids.add(ids, id)))
			| NONE => Lines.insert (lines, p, (Ids.singleton id))
		    in
			case r of
			  Line(p1, p2) => 
				add_id(add_id lines p1 id) p2 id
			| Box b => lines
		    end
		)
	    (* Create map from ids to all rendered items. *)
	    val items: id_item Items.map =
		fold rl Items.empty (fn ((id, r): id_item, items) =>
			    Items.insert(items, id, (id, r)))

	    (* grow_curve(c, lines, items) finds all line items that
	       can be glued onto the head of curve c.
	       The result is (curve, items') where curve is the grown version of
	       c.
	    *)
	    type ret = curve * id_item Items.map
	    fun grow_curve(c as (p1:point)::p1s, lines, items) : ret =
	    let
		fun giveup() : ret = (c, items)
	    in
		case Lines.find(lines, p1) of
		    SOME ids => 
			(* find an item that is still there *)
			let val (items, ropt: rendered_item option) =
			    Ids.foldl (fn (id, (items, ropt: rendered_item option)) =>
				(case ropt of NONE => ((case Items.remove(items, id) of
					(items, (id', r)) => (items, SOME r))
				      handle NotFound => (items, NONE))
				| _ => (items, ropt)))
			    (items, NONE)
			    ids
			in (case ropt of
			      (* found one *)
			      SOME(Line(p3, p4)) =>
				(case (point_compare(p1, p3), point_compare(p1, p4)) of
					(EQUAL, _) => grow_curve(p4::c, lines, items)
				    |   (_, EQUAL) => grow_curve(p3::c, lines, items)
				    |   _ => giveup())
			    | _ => giveup())
			end
		|   NONE => giveup()
			    handle NotFound => giveup()
	    end
	in
	    let val (curves, items) =
		    fold rl ([], items) (fn ((id, r): id_item, (curves, items)) => (
			case r of
			    Box _ => (curves, items)
			|   Line(p1, p2) => (* grow the curve in both directions *)
				let
				    val (items, _) = Items.remove(items, id)
				    val (c1, items) = grow_curve([p1], lines, items)
				    val (c2, items) = grow_curve([p2], lines, items)
				in
				    ((c1@(rev c2))::curves, items)
				end
				    handle NotFound => (curves, items)
		    ))
		val remainder = Items.foldl (fn ((id, r), l) => r::l) [] items
	    in
		print ("% Found " ^ (Int.toString (length curves)) ^ " curves\n");
		(curves, remainder)
	    end
	end
      
    fun plot(formulas: formula list,
             funs: defn list, x: real_interval, y: real_interval,
	     resolution: real, color: string, linetype: string, linewidth: string) = let
      val out = List.rev(curves(formulas, x, y, resolution, funs))
      val counter = ref 0
      in
	  foreach out (fn items => (
	     counter := !counter + 1;
	     setup_formula_render(!counter, color, linetype, linewidth);
	     if !counter > 1 then
		print "\nautocolor\n"
	     else
		();
	     let val (curves, items) = restitch items in
	        foreach curves (fn curve => (
		    print "startcurve\n";
		    case curve of
		      [] => print "% oops - empty curve!?\n"
		    | (x,y)::pts => (
			print ((toPS x) ^ " " ^ (toPS y) ^ " firstpt\n");
			foreach pts (fn (x, y) =>
			    print ((toPS x) ^ " " ^ (toPS y) ^ " pt\n"));
			print "endcurve\n"
		      )
		));
		foreach items (fn item =>
		    case item of
			Line((x0,y0), (x1,y1)) =>
			    print ((toPS(x0)) ^ " " ^ (toPS(y0)) ^ " " ^
				    (toPS(x1)) ^ " " ^ (toPS(y1)) ^ " seg\n")
		    | Box((x0,x1), (y0,y1)) =>
			    print ((toPS(x0)) ^ " " ^ (toPS(x1)) ^ " " ^
				    (toPS(y0)) ^ " " ^ (toPS(y1)) ^ " box\n"))
		end
	  ))
      end

    fun main (arg0: string, args: string list ) = let
      val usage = "Usage: " ^ arg0 ^ " <minx> <maxx> <miny> <maxy>" ^
                  " <n> <eqn_1> <color_1> <line_1> <width_1> ... <eqn_n> <color_n> <line_n> <width_n>\n"
    in
      if List.length(args) < 5 then (
	print usage;
	OS.Process.failure
      ) else let
	val [x0,x1,y0,y1] = map (fn s => valOf(Real.fromString s))
				(List.take (args, 4))
	val args = List.drop(args, 4)
	val numeqns = valOf(Int.fromString (hd args))
	fun parseEqns(args: string list, n: int) : (string*string*string*string) list = (
	    case (args, n) of ([], 0) => []
	     | (eqn :: color :: line :: width :: rest, _) =>
		    (eqn, color, line, width) :: parseEqns(rest, n-1)
	     | _ => (print usage; raise Fail usage)
	)
	val eqns = parseEqns(tl args, numeqns)
	val resolution = (x1 - x0)/800.0
      in
        print( (toPS(x0)) ^ " " ^ 
	       (toPS(x1)) ^ " " ^ 
	       (toPS(y0)) ^ " " ^ 
	       (toPS(y1)) ^ " setbounds\n");
	print("0.2 setgray\n");
        print("% Resolution: "^(toPS(resolution))^"\n");
	foreach eqns (fn (eqn, color, line, width) => 
	    let val (formulas, defns) = Parser.parseString(eqn) in
	        print ("% Formula: " ^ eqn ^ "\n");
		plot(formulas, defns, (x0,x1), (y0,y1), resolution, color, line, width)
	    end
	);
	print("0 setgray\n");
        print( (toPS(x0)) ^ " " ^ 
	       (toPS(x1)) ^ " " ^ 
	       (toPS(y0)) ^ " " ^ 
	       (toPS(y1)) ^ " setbounds\n");
	OS.Process.success
      end
    end

end
