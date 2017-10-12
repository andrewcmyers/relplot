structure Parser = struct
  structure EQNLrVals = EQNLrValsFun (structure Token = LrParser.Token)
  structure EQNLex = EQNLexFun (structure Tokens = EQNLrVals.Tokens)
  structure EQNParser = Join (structure ParserData = EQNLrVals.ParserData
                              structure Lex = EQNLex
                              structure LrParser = LrParser)

  structure PP = struct
    type bufstream = TextIO.instream * string * int * string
    type state = bufstream list ref
    val s : state = ref nil

    fun init (instream): unit =
      s := [(instream, "", 0, "<top>")]

  end

  exception ParseError

  fun error (s) =
    (print "("; print s; print ") reporterror\n"; raise ParseError)

  fun invoke (lexstream) = let
    val print_error = fn (s,i:int,f) =>
      error (concat [s," [in ", EQNLex.UserDeclarations.getFileName f, ":",(Int.toString i),"]"])
  in
    EQNParser.parse(0,lexstream,print_error,())
  end

  fun parse (lexer) = let
    val dummyEOF = EQNLrVals.Tokens.EOF(0,0)
    fun loop lexer = let
      val (result,lexer) = invoke lexer
      val (nextToken,lexer) = EQNParser.Stream.get lexer
    in
      if EQNParser.sameToken(nextToken,dummyEOF)
      then result
      else loop lexer
    end
  in
    EQNLex.UserDeclarations.init ();
    (SOME (loop lexer) handle _ => NONE)
  end

  fun parseString (string) = let
    val done = ref false
    fun input(n:int): string =
      if !done then ""
      else (done := true; string)
    val lexer = EQNParser.makeLexer (input)
    val r = parse (lexer)
  in
    case r of
      SOME e => e
    | NONE => raise ParseError
  end

end
