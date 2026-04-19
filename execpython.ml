(*
   Name: Ben Hamilton
   File: execpython.ml
*)

#use "lexer.mml.ml";;

let getOpValue op = (
  match op with
  | ExpTok -> 8
  | SqrtTok -> 8
  | NegateTok -> 7
  | MulTok -> 6
  | DivTok -> 6
  | ModTok -> 6
  | AddTok -> 5
  | SubTok -> 5
  | EqualTok -> 4
  | NotEqTok -> 4
  | GrTok    -> 4
  | LsTok    -> 4
  | GrEqTok  -> 4
  | LsEqTok  -> 4
  | NotTok -> 3
  | AndTok -> 2
  | OrTok  -> 1
  | AssignTok -> 0
  | AugAddTok -> 0
  | AugSubTok -> 0
  | AugMulTok -> 0
  | AugDivTok -> 0
  | AugModTok -> 0
  | LParenTok -> -1
  | RParenTok -> -2
  | PrintlnTok -> 0
  | PrintTok -> 0
  | InputTok -> 0
  | _ -> raise (Invalid_argument ((getTokString op)^" is not an operator"))
);;

let getOpAss op = (
  match op with
  | ExpTok -> 1
  | SqrtTok -> 1
  | NegateTok -> 1
  | PrintlnTok -> 1
  | PrintTok -> 1
  | InputTok -> 1
  | _ -> 0
);;

let isOperator token = (
  try
    let _ = getOpValue token in
    true
  with _ -> false
);;


let getNumOpValues op = (
  match op with
  | NegateTok -> 1
  | NotTok -> 1
  | PrintlnTok -> 1
  | PrintTok -> 1
  | InputTok -> 1
  | SqrtTok -> 1
  | _ -> 2
)


let cnvToBool value = (
  match value with
  | BoolTok(value) -> BoolTok(value)
  | IntTok(value) -> (
      if (value = 0) then BoolTok(false)
      else BoolTok(true)
    )
  | FloatTok(value) -> (
      if (value = 0.) then BoolTok(false)
      else BoolTok(true)
    )
  | _-> raise (WrongType("cnvToBool expects a bool, int, or float"))
);;
let cnvToInt value = (
  match value with
  | BoolTok(value) -> (
      if (value) then IntTok(1)
      else IntTok(0)
    )
  | IntTok(value) -> IntTok(value)
  | FloatTok(value) -> IntTok((int_of_float value))
  | _ -> raise (WrongType("cnvToInt expects a bool, int, or float"))
);;
let cnvToFloat value = (
  match value with
  | BoolTok(value) -> (
      if (value) then FloatTok(1.)
      else FloatTok(0.)
    )
  | IntTok(value) -> FloatTok((float_of_int value))
  | FloatTok(value) -> FloatTok(value)
  | _ -> raise (WrongType("convToFloat expects a bool, int, or float"))
);;


let getSymVal name symtbl = (
  match name with
  | NameTok(name) -> (
      if (Hashtbl.mem symtbl name) then
        Hashtbl.find symtbl name
      else raise (UnknownVarInt(name))
    )
  | _ -> name (* easier to let it pass thru if not var *)
);;

let setSymVal name symtbl value = (
  match name with
  | NameTok(name) -> Hashtbl.replace symtbl name value
  | _ -> raise (NotVarTypeInt((getTokString name)))
);;


let rec printValue value symtbl = (
  match value with
  | NameTok(name) -> (
      let value = getSymVal value symtbl in
      Printf.printf "%s: " name;
      printValue value symtbl;
      flush_all ()
    )
  | BoolTok(value) -> Printf.printf "bool: %b\n%!" value;
  | IntTok(value) -> Printf.printf "int: %d\n%!" value;
  | FloatTok(value) -> Printf.printf "float: %f\n%!" value;
  | _ -> raise (WrongType((getTokString value)^" is not a value type"))
);;


let promoteTypes left right symtbl = (
  let left = getSymVal left symtbl in
  let right = getSymVal right symtbl in
  match (left, right) with
  | ((BoolTok(_)),  (BoolTok(_)) ) -> (left, right)
  | ((IntTok(_)),   (IntTok(_))  ) -> (left, right)
  | ((FloatTok(_)), (FloatTok(_))) -> (left, right)

  | ((BoolTok(_)),  (IntTok(_))  ) -> (cnvToInt left,   right)
  | ((BoolTok(_)),  (FloatTok(_))) -> (cnvToFloat left, right)

  | ((IntTok(_)),   (BoolTok(_)) ) -> (left,   cnvToInt right)
  | ((IntTok(_)),   (FloatTok(_))) -> (cnvToFloat left, right)

  | ((FloatTok(_)), (BoolTok(_)) ) -> (left, cnvToFloat right)
  | ((FloatTok(_)), (IntTok(_))  ) -> (left, cnvToFloat right)

  | (_, _) -> raise (WrongType("promoteTypes expects bools, ints, and floats"))
);;


let promAtlInt left right symtbl = (
  let left, _ = promoteTypes left (IntTok(0)) symtbl in
  promoteTypes left right symtbl
);;


let getFloat tok = match tok with
  | FloatTok(value) -> value
  | _ -> raise (WrongType("getFloat expects a float"))
;;
let getInt tok = match tok with
  | IntTok(value) -> value
  | _ -> raise (WrongType("getInt expects an int"))
;;
let getBool tok = match tok with
  | BoolTok(value) -> value
  | _ -> raise (WrongType("getBool expects a bool"))
;;


let execArith op left right = (
  match left with
  | IntTok(_) -> (
      let left = getInt left in
      let right = getInt right in
      match op with
      | 0 -> IntTok(left * right)
      | 1 -> IntTok(left / right)
      | 2 -> IntTok(left + right)
      | 3 -> IntTok(left - right)
      | _ -> raise (InvalidEnum(op))
    )
  | FloatTok(_) -> (
      let left = getFloat left in
      let right = getFloat right in
      match op with
      | 0 -> FloatTok(left *. right)
      | 1 -> FloatTok(left /. right)
      | 2 -> FloatTok(left +. right)
      | 3 -> FloatTok(left -. right)
      | _ -> raise (InvalidEnum(op))
    )
  | BoolTok(_) -> raise (WrongType("bool is not arith type"))
  | _ -> raise (WrongType("execArith expects int or float. not a "^(getTokString left)))
);;

let execComp op left right = (
  match left with
  | BoolTok(_) -> (
      let left = getBool left in
      let right = getBool right in
      match op with
      | 0 -> BoolTok(left = right)
      | 1 -> BoolTok(left != right)
      | 2 -> BoolTok(left > right)
      | 3 -> BoolTok(left < right)
      | 4 -> BoolTok(left >= right)
      | 5 -> BoolTok(left <= right)
      | _ -> raise (InvalidEnum(op))
    )
  | IntTok(_) -> (
      let left = getInt left in
      let right = getInt right in
      match op with
      | 0 -> BoolTok(left = right)
      | 1 -> BoolTok(left != right)
      | 2 -> BoolTok(left > right)
      | 3 -> BoolTok(left < right)
      | 4 -> BoolTok(left >= right)
      | 5 -> BoolTok(left <= right)
      | _ -> raise (InvalidEnum(op))
    )
  | FloatTok(_) -> (
      let left = getFloat left in
      let right = getFloat right in
      match op with
      | 0 -> BoolTok(left = right)
      | 1 -> BoolTok(left != right)
      | 2 -> BoolTok(left > right)
      | 3 -> BoolTok(left < right)
      | 4 -> BoolTok(left >= right)
      | 5 -> BoolTok(left <= right)
      | _ -> raise (InvalidEnum(op))
    )
  | _ -> raise (WrongType("execComp expects a bool, int, or float. not a "^(getTokString left)))
);;


let rec execOp2 op left right symtbl = (
  match op with
  | ExpTok -> (
      let left = cnvToFloat (getSymVal left symtbl) in
      let right = cnvToFloat (getSymVal right symtbl) in
      FloatTok((getFloat left) ** (getFloat right))
    )
  | MulTok -> (
      let left, right = promAtlInt left right symtbl in
      execArith 0 left right
    )
  | DivTok -> (
      let left, right = promAtlInt left right symtbl in
      execArith 1 left right
    )
  | AddTok -> (
      let left, right = promAtlInt left right symtbl in
      execArith 2 left right
    )
  | SubTok -> (
      let left, right = promAtlInt left right symtbl in
      execArith 3 left right
    )
  | ModTok -> (
      let left = getInt (cnvToInt (getSymVal left symtbl)) in
      let right = getInt (cnvToInt (getSymVal right symtbl)) in
      (* default exception for div0 is fine *)
      IntTok(left mod right)
    )
  | EqualTok -> (
      let left, right = promoteTypes left right symtbl in
      execComp 0 left right
    )
  | NotEqTok -> (
      let left, right = promoteTypes left right symtbl in
      execComp 1 left right
    )
  | GrTok -> (
      let left, right = promoteTypes left right symtbl in
      execComp 2 left right
    )
  | LsTok -> (
      let left, right = promoteTypes left right symtbl in
      execComp 3 left right
    )
  | GrEqTok -> (
      let left, right = promoteTypes left right symtbl in
      execComp 4 left right
    )
  | LsEqTok -> (
      let left, right = promoteTypes left right symtbl in
      execComp 5 left right
    )
  | AndTok -> (
      let left = cnvToBool (getSymVal left symtbl) in
      let right = cnvToBool (getSymVal right symtbl) in
      BoolTok((getBool left) && (getBool right))
    )
  | OrTok -> (
      let left = cnvToBool (getSymVal left symtbl) in
      let right = cnvToBool (getSymVal right symtbl) in
      BoolTok((getBool left) || (getBool right))
    )
  | AssignTok -> (
      let right = getSymVal right symtbl in
      setSymVal left symtbl right;
      left
    )
  | AugAddTok -> (
      let right = (execOp2 AddTok left right symtbl) in
      (execOp2 AssignTok left right symtbl)
    )
  | AugSubTok -> (
      let right = (execOp2 SubTok left right symtbl) in
      (execOp2 AssignTok left right symtbl)
    )
  | AugMulTok -> (
      let right = (execOp2 MulTok left right symtbl) in
      (execOp2 AssignTok left right symtbl)
    )
  | AugDivTok -> (
      let right = (execOp2 DivTok left right symtbl) in
      (execOp2 AssignTok left right symtbl)
    )
  | AugModTok -> (
      let right = (execOp2 ModTok left right symtbl) in
      (execOp2 AssignTok left right symtbl)
    )
  | _ -> raise (InvalidOp(getTokString op))
);;

let fmtString str = (
  let strs = String.split_on_char '\"' str in
  match strs with
  | _::str::_ -> Scanf.unescaped str
  | _ -> raise (HowDidWeGetHere(0))
);;

let getInput () = (
  let input = read_line () in
  (*
     try bool,
     then int,
     then float
  *)
  if (input = "True") then (BoolTok(true))
  else (
    if (input = "False") then (BoolTok(false))
    else (
      try (
        IntTok(int_of_string input)
      ) with Failure(_) -> try (
        FloatTok(float_of_string input)
      ) with Failure(_) -> raise (BadInputInt(input))
    )
  )
);;

let rec execOp1 op right symtbl = (
  match op with
  | SqrtTok -> (
      let right = cnvToFloat (getSymVal right symtbl) in
      FloatTok(sqrt (getFloat right))
    )
  | NegateTok -> execOp2 SubTok (IntTok(0)) right symtbl
  | NotTok -> BoolTok(not (getBool (cnvToBool (getSymVal right symtbl))))
  | PrintTok -> (
      match right with
      | StringTok(value) -> Printf.printf "%s%!" (fmtString value); right
      | NameTok(value) -> execOp1 PrintTok (getSymVal right symtbl) symtbl
      | BoolTok(value) -> Printf.printf "%b%!" value; right
      | IntTok(value) -> Printf.printf "%d%!" value; right
      | FloatTok(value) -> Printf.printf "%f%!" value; right
      | _ -> printTok right; flush_all (); right
    )
  | PrintlnTok -> (
      let ret = execOp1 PrintTok right symtbl in
      Printf.printf "\n%!";
      ret
    )
  | InputTok -> (
      match right with
      | NameTok(name) -> (
          (setSymVal (NameTok(name)) symtbl (getInput ()));
          NameTok(name)
        )
      | _ -> raise (NotVarTypeInt(getTokString right))
    )
  | _ -> raise (InvalidOp(getTokString op))
);;




let rec execScopeRec scope symtbl wasif ifdone = (
  match scope with
  | Block(lines) -> (
      match lines with
      | [] -> ()
      | line::tail -> (
          try (
            match (getLineList line) with
            | [] -> ()
            | IfTok::_ -> (
                let ret = (execLine line symtbl) in
                (execScopeRec (Block(tail)) symtbl true ret)
              )
            | ElifTok::_ -> (
                if (wasif) then (
                  if (not ifdone) then (
                    let ret = (execLine line symtbl) in
                    (execScopeRec (Block(tail)) symtbl true ret)
                  )
                  else (
                    (execScopeRec (Block(tail)) symtbl true ifdone)
                  )
                )
                else raise (IllegalElse(getLineNum line))
              )
            | ElseTok::_ -> (
                if (wasif) then (
                  if (not ifdone) then (
                    let _ = (execLine line symtbl) in
                    (execScopeRec (Block(tail)) symtbl false false)
                  )
                  else (execScopeRec (Block(tail)) symtbl false false)
                )
                else raise (IllegalElse(getLineNum line))
              )
            | WhileTok::_ -> (
                let ret = (execLine line symtbl) in
                if (ret) then
                  (execScopeRec (Block(lines)) symtbl false false)
                else
                  (execScopeRec (Block(tail)) symtbl false false)
              )
            | _ -> (
                let _ = (execLine line symtbl) in
                (execScopeRec (Block(tail)) symtbl false false)
              )
          ) with
          | UnknownVarInt(str) -> raise (UnknownVar(str, (getLineNum line)))
          | ExpectedOpInt -> raise (ExpectedOp((getLineNum line)))
          | ExpectedValueInt(str) -> raise (ExpectedValue(str, (getLineNum line)))
          | NotVarTypeInt(str) -> raise (NotVarType(str, (getLineNum line)))
          | Division_by_zero -> raise (DivByZero((getLineNum line)))
          | BadInputInt(str) -> raise (BadInput(str, (getLineNum line)))
        )
    )
)
and execLineRPN toks symtbl nums isAss = (
  match toks with
  | [] -> (
      if (List.length nums = 1) then
        (getHead nums, isAss)
        else raise (ExpectedOpInt)
    )
            
  | BoolTok(value)::tail -> execLineRPN tail symtbl ((BoolTok(value))::nums) isAss
  | IntTok(value)::tail -> execLineRPN tail symtbl ((IntTok(value))::nums) isAss
  | FloatTok(value)::tail -> execLineRPN tail symtbl ((FloatTok(value))::nums) isAss
  | NameTok(value)::tail -> execLineRPN tail symtbl ((NameTok(value))::nums) isAss
  | StringTok(value)::tail -> execLineRPN tail symtbl ((StringTok(value))::nums) isAss

  | op::tail -> ( (* hopefully its just values and ops at this point *)
      if ((getNumOpValues op) = 1) then (
        if (List.length nums >= 1) then (
          let right = getHead nums in
          let ret = execOp1 op right symtbl in
          if ((getOpValue op) = 0) then
            execLineRPN tail symtbl (ret::(getTail nums)) true
          else
            execLineRPN tail symtbl (ret::(getTail nums)) isAss
        )
        else raise (ExpectedValueInt((getTokString op)))
      )
      else ( (* two ops or invalid *)
        if ((getOpValue op) = 0) then ( (* assignment ops *)
          if (List.length nums >= 2) then (
            let right = getHead nums in
            let left = getHead (getTail nums) in
            let ret = execOp2 op left right symtbl in
            execLineRPN tail symtbl (ret::(getTail (getTail nums))) true
          )
          else raise (ExpectedValueInt((getTokString op)))
        )
        else (
          if (List.length nums >= 2) then (
            let right = getHead nums in
            let left = getHead (getTail nums) in
            let ret = execOp2 op left right symtbl in
            execLineRPN tail symtbl (ret::(getTail (getTail nums))) isAss
          )
          else raise (ExpectedValueInt((getTokString op)))
        )
      )
    )
  )
and execScope scope symtbl = (execScopeRec scope symtbl false false)
and execLine line symtbl = (
  let toks = getLineList line in
  let tok = getHead toks in
  match tok with
  | IfTok -> (
      let ctrl = getLineScope line in
      if (getBlockLength ctrl > 0) then (  
        let ret, _ = (execLineRPN (getTail toks) symtbl [] false) in
        let value = getBool (cnvToBool (getSymVal ret symtbl)) in
        if (value) then (
          (execScope ctrl symtbl);
          value
        ) else value
      ) else raise (EmptyCtrlBlock(getLineNum line))
    )
  | ElifTok -> (
      let ctrl = getLineScope line in
      if (getBlockLength ctrl > 0) then (  
        let ret, _ = (execLineRPN (getTail toks) symtbl [] false) in
        let value = getBool (cnvToBool (getSymVal ret symtbl)) in
        if (value) then (
          (execScope ctrl symtbl);
          value
        ) else value
      ) else raise (EmptyCtrlBlock(getLineNum line))
    )
  | ElseTok -> (
      let ctrl = getLineScope line in
      if (getBlockLength ctrl > 0) then (
        (execScope ctrl symtbl);
        true
      ) else raise (EmptyCtrlBlock(getLineNum line))
    )
  | WhileTok -> (
      let ctrl = getLineScope line in
      if (getBlockLength ctrl > 0) then (
        let ret, _ = (execLineRPN (getTail toks) symtbl [] false) in
        let value = getBool (cnvToBool (getSymVal ret symtbl)) in
        if (value) then (
          (execScope ctrl symtbl);
          value
        ) else value
      ) else raise (EmptyCtrlBlock(getLineNum line))
    )
  | _ -> (
      let ctrl = getLineScope line in
      if (getBlockLength ctrl = 0) then (
        let ret, isAss = (execLineRPN toks symtbl [] false) in
        if (isAss) then true
        else (
          printValue ret symtbl;
          true
        )
      ) else raise (NonIfCtrlBlock(getLineNum line))
    )
);;





let rec convertUnaryRec line wasOp = (
  match line with
  | [] -> []
  | tok::tail -> (
      if (isOperator tok) then (
        match tok with
        | AddTok ->
          if (wasOp) then (convertUnaryRec tail true)
          else AddTok::(convertUnaryRec tail true)
        | SubTok ->
          if (wasOp) then NegateTok::(convertUnaryRec tail true)
          else SubTok::(convertUnaryRec tail true)
        | tok -> tok::(convertUnaryRec tail true) (* bad if not pars*)
      )
      else tok::(convertUnaryRec tail false)
    )
);;

let convertUnary line = convertUnaryRec line true;;
  

let rec loopOpRPN tokens op ops = (
  if (List.length ops = 0) then
    convertRPNRec tokens (op::ops)
  else (
    let opVal = getOpValue op in
    let topOp = getHead ops in
    let topOpVal = getOpValue topOp in
    
    if (topOpVal = -1) then (
      if (opVal = -2) then
        convertRPNRec tokens (getTail ops)
      else convertRPNRec tokens (op::ops)
    )
    else (
      if (opVal > topOpVal || (opVal = topOpVal && (getOpAss op) = 1)) then
        convertRPNRec tokens (op::ops)
      else
        topOp::(loopOpRPN tokens op (getTail ops))
    )
  )
)
and
convertRPNRec tokens ops = (
  match tokens with
  | [] -> []
  | IfTok::tail -> IfTok::(convertRPNRec tail ops)
  | ElifTok::tail -> ElifTok::(convertRPNRec tail ops)
  | ElseTok::tail -> ElseTok::(IntTok(0))::(convertRPNRec tail ops)
  | WhileTok::tail -> WhileTok::(convertRPNRec tail ops)
  | ColonTok::tail -> (convertRPNRec tail ops)

  | LineNumTok(num)::tail -> (LineNumTok(num))::(convertRPNRec tail ops)
  | NewScopeTok::tail -> NewScopeTok::(convertRPNRec tail ops)
  | EndScopeTok::tail -> EndScopeTok::(convertRPNRec tail ops)
                        
  | BoolTok(value)::tail  -> (BoolTok(value))::(convertRPNRec tail ops)
  | IntTok(value)::tail   -> (IntTok(value))::(convertRPNRec tail ops)
  | FloatTok(value)::tail -> (FloatTok(value))::(convertRPNRec tail ops)
  | NameTok(value)::tail  -> (NameTok(value))::(convertRPNRec tail ops)
  | LParenTok::tail -> convertRPNRec tail (LParenTok::ops)
  | StringTok(value)::tail -> (StringTok(value))::(convertRPNRec tail ops)
                         
  | RParenTok::tail -> loopOpRPN tail RParenTok ops
  | op::tail -> loopOpRPN tail op ops
);;

let convertRPN tokens = convertRPNRec (LParenTok::tokens@[RParenTok]) [];;

let rec checkParenthesis line count = (
  match line with
  | [] -> count = 0
  | LParenTok::tail -> checkParenthesis tail (count+1)
  | RParenTok::tail -> checkParenthesis tail (count-1)
  | tok::tail -> checkParenthesis tail count
);;

let rec applyRPN lines = (
  match lines with
  | [] -> []
  | line::tail ->
    if (checkParenthesis line 0) then
      (convertRPN (convertUnary line))::(applyRPN tail)
    else raise (MismatchedPar(
        match line with
        | LineNumTok(value)::tail -> value
        | _ -> raise (HowDidWeGetHere(0))
      ))
);;


let execPython input debug = (
  try (
    let tokens = (lexer0 0 (Lexing.from_string input)) in
    let tokens = insertScopeMarkers tokens in
    let lines = groupLines tokens in
    let lines = applyRPN lines in
    let scope = makeScopeTree lines in

    if (debug) then (
      let lineCount = (countNewLines tokens 0) in
      let lineNumWidth = int_of_float (log10 (float_of_int lineCount)) in
      printScope scope lineNumWidth;
      let (symtbl : (string, py_token) Hashtbl.t) = Hashtbl.create 100 in
      execScope scope symtbl
    )
    else (
      let (symtbl : (string, py_token) Hashtbl.t) = Hashtbl.create 100 in
      execScope scope symtbl
    )
  ) with
  | UnknownVar(str, num) ->
    Printf.printf "ERROR on line %d: Unknown variable: %s\n" num str
  | ExpectedOp(num) ->
    Printf.printf "ERROR on line %d: Missing operator\n" num
  | ExpectedValue(str, num) ->
    Printf.printf "ERROR on line %d: Missing value(s) for operator %s\n" num str
  | MismatchedPar(num) ->
    Printf.printf "ERROR on line %d: Mismatched parentheses\n" num
  | NotVarType(str, num) ->
    Printf.printf "ERROR on line %d: Cannot set value of non variable type %s\n" num str
  | IllegalElse(num) ->
    Printf.printf "ERROR on line %d: Else control missing if\n" num
  | EmptyCtrlBlock(num) ->
    Printf.printf "ERROR on line %d: Control missing block\n" num
  | NonIfCtrlBlock(num) ->
    Printf.printf "ERROR on line %d: Non-control statement has block\n" num
  | DivByZero(num) ->
    Printf.printf "ERROR on line %d: Division by 0\n" num
  | UnknownToken(tok, num) ->
    Printf.printf "ERROR on line %d: Unknown char %c\n" num tok
  | BadInput(str, num) ->
    Printf.printf "ERROR on line %d: Bad input \"%s\"\n" num str
);;
