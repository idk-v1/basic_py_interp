exception UnknownVar of string * int;;
exception UnknownVarInt of string;;
exception MismatchedPar of int;;
exception ExpectedValue of string * int;;
exception ExpectedValueInt of string;;
exception NotVarTypeInt of string;;
exception NotVarType of string * int;;
exception WrongType of string;; (*internal only*)
exception ExpectedOp of int;;
exception ExpectedOpInt;;
exception HowDidWeGetHere of int;; (*internal only*)
exception InvalidEnum of int;; (*internal only*)
exception InvalidOp of string;; (* shouldn't get here, internal error only probably *)
exception IllegalElse of int;;
exception EmptyCtrlBlock of int;;
exception NonIfCtrlBlock of int;;
exception DivByZero of int;;
exception UnknownToken of char;; (* tokenizing phase, not much to tell *)

type py_token =
  | PrintTok
  | StringTok of string
  | LineNumTok of int
  | ExpTok
  | NegateTok
  | AddTok
  | SubTok
  | MulTok
  | DivTok
  | ModTok
  | AssignTok
  | AugAddTok
  | AugSubTok
  | AugMulTok
  | AugDivTok
  | AugModTok
  | EqualTok
  | NotEqTok
  | NotTok
  | AndTok
  | OrTok
  | GrTok
  | LsTok
  | GrEqTok
  | LsEqTok
  | NewLineTok
  | TabTok
  | IntTok of int
  | FloatTok of float
  | BoolTok of bool
  | NameTok of string
  | ColonTok
  | IfTok
  | ElifTok
  | ElseTok
  | WhileTok
  | LParenTok
  | RParenTok
  | NewScopeTok
  | EndScopeTok
;;

let getTokString token = (
  match token with
  | PrintTok -> "print"
  | StringTok(_) -> "string"
  | LineNumTok(_) -> "lineNum"
  | NewScopeTok -> "{"
  | EndScopeTok -> "}"
  | ExpTok -> "**"
  | NegateTok -> "-"
  | AddTok -> "+"
  | SubTok -> "-"
  | MulTok -> "*"
  | DivTok -> "/"
  | ModTok -> "%"
  | AssignTok -> "="
  | AugAddTok -> "+="
  | AugSubTok -> "-="
  | AugMulTok -> "*="
  | AugDivTok -> "/="
  | AugModTok -> "%="
  | EqualTok -> "=="
  | NotEqTok -> "!="
  | NotTok -> "not"
  | AndTok -> "and"
  | OrTok -> "or"
  | GrTok -> ">"
  | LsTok -> "<"
  | GrEqTok -> ">="
  | LsEqTok -> "<="
  | NewLineTok -> "\n"
  | TabTok -> "---|"
  | ColonTok -> ":"
  | IfTok -> "if"
  | ElifTok -> "elif"
  | ElseTok -> "else"
  | WhileTok -> "while"
  | LParenTok -> "("
  | RParenTok -> ")"
  | IntTok(value) -> "int"
  | FloatTok(value) -> "float"
  | BoolTok(value) -> "bool"
  | NameTok(name) -> "var"
);;

let rec printTok token = (
  match token with
  | PrintTok -> Printf.printf "print "
  | StringTok(str) -> Printf.printf "%s " str
  | LineNumTok(num) -> Printf.printf "lineNum:%d " num
  | NewScopeTok -> Printf.printf "{\n"
  | EndScopeTok -> Printf.printf "}\n"
  | ExpTok -> Printf.printf "** "
  | NegateTok -> Printf.printf "- "
  | AddTok -> Printf.printf "+ "
  | SubTok -> Printf.printf "- "
  | MulTok -> Printf.printf "* "
  | DivTok -> Printf.printf "/ "
  | ModTok -> Printf.printf "%% "
  | AssignTok -> Printf.printf "= "
  | AugAddTok -> Printf.printf "+= "
  | AugSubTok -> Printf.printf "-= "
  | AugMulTok -> Printf.printf "*= "
  | AugDivTok -> Printf.printf "/= "
  | AugModTok -> Printf.printf "%%= "
  | EqualTok -> Printf.printf "== "
  | NotEqTok -> Printf.printf "!= "
  | NotTok -> Printf.printf "! "
  | AndTok -> Printf.printf "&& "
  | OrTok -> Printf.printf "|| "
  | GrTok -> Printf.printf "> "
  | LsTok -> Printf.printf "< "
  | GrEqTok -> Printf.printf ">= "
  | LsEqTok -> Printf.printf "<= "
  | NewLineTok -> Printf.printf "\n"
  | TabTok -> Printf.printf "---|"
  | ColonTok -> Printf.printf ": "
  | IfTok -> Printf.printf "if "
  | ElifTok -> Printf.printf "elif "
  | ElseTok -> Printf.printf "else "
  | WhileTok -> Printf.printf "while "
  | LParenTok -> Printf.printf "( "
  | RParenTok -> Printf.printf ") "
  | IntTok(value) -> Printf.printf "%d " value
  | FloatTok(value) -> Printf.printf "%f " value
  | BoolTok(value) -> Printf.printf "%b " value
  | NameTok(name) -> Printf.printf "%s " name
);;

let rec printToks tokens = (
  match tokens with
  | [] -> ()
  | token::list -> printTok token; printToks list
);;



(*
   0 | if thing:       | 0
   1 |   do_thing  |   |+1
   0 |             |-1
*)
let rec makeScope num = (
  if (num = 0) then []
  else (
    if (num < 0) then
      EndScopeTok::(makeScope (num+1))
    else
      NewScopeTok::(makeScope (num-1))
  )
);;

let rec insertScopeMarkersRec tokens indent last_indent found = (
  match tokens with
  | [] -> []
  | TabTok::tail ->
    if (not found) then
      (insertScopeMarkersRec tail (indent+1) last_indent found)
    else
      (insertScopeMarkersRec tail indent last_indent found)
  | NewLineTok::tail ->
    if (not found) then
      let scope = makeScope (indent - last_indent) in
      scope@(NewLineTok::(insertScopeMarkersRec tail 0 indent false))
    else
      NewLineTok::(insertScopeMarkersRec tail 0 indent false)
  | tok::tail ->
    if (not found) then
      let scope = makeScope (indent - last_indent) in
      scope@(tok::(insertScopeMarkersRec tail indent last_indent true))
    else
      tok::(insertScopeMarkersRec tail indent last_indent true)
);;

let insertScopeMarkers tokens = insertScopeMarkersRec (tokens) 0 0 false;;



let rec groupLine tokens = (
  match tokens with
  | [] -> []
  | NewLineTok::tail -> []
  | tok::tail -> tok::(groupLine tail)
);;

let rec skipTokens tokens skip = (
  if (skip = 0) then tokens
  else
    match tokens with
    | [] -> []
    | _::tail -> skipTokens tail (skip-1)
);;

let rec groupLinesRec tokens lineNum = (
  match tokens with
  | [] -> []
  | NewLineTok::tail -> groupLinesRec tail (lineNum+1)
  | NewScopeTok::tail -> [NewScopeTok]::(groupLinesRec tail lineNum)
  | EndScopeTok::tail -> [EndScopeTok]::(groupLinesRec tail lineNum)
  | tok::tail -> (
      let group = (LineNumTok(lineNum))::(groupLine tokens) in
      let groupLen = ((List.length group) - 1) in
      let skip = skipTokens tokens groupLen in
      group::(groupLinesRec skip lineNum)
    )
);;

let groupLines tokens = groupLinesRec tokens 0;;



type line = Line of py_token list * scope_block * int
and scope_block = Block of line list;;

let getLineList line = (
  match line with
  | Line(list, _, _) -> list
);;

let getLineScope line = (
  match line with
  | Line(_, scope, _) -> scope
);;

let getLineNum line = (
  match line with
  | Line(_, _, line) -> line
);;

let getBlockLength block = (
  match block with
  | Block(lines) -> List.length lines
);;

let getHead scope = (
  match scope with
  | [] -> raise Not_found
  | head::tail -> head
);;

let getTail scope = (
  match scope with
  | [] -> []
  | head::tail -> tail
);;

let rec revAcc list acc = (
  match list with
  | [] -> acc
  | head::tail -> revAcc tail (head::acc)
);;

let rev list = revAcc list [];;

let rec skipLines lines num = (
  if (num = 0) then lines
  else
    match lines with
    | [] -> []
    | _::tail -> skipLines tail (num-1)
);;


let rec printLine line indent = (
  if (indent > 0) then (
    printTok TabTok;
    printLine line (indent-1)
  )
  else
    match line with
    | [] -> printTok NewLineTok
    | tok::tail -> (
        printTok tok;
        printLine tail 0
      )
);;

let rec makeScopeTreeRec lines scope index lineNum = (
  index := !index + 1;
  let oldIndex = !index in
  match scope with
  | Block(scope) -> (
      match lines with
      | [] -> Block(rev scope)
      | [EndScopeTok]::tail -> Block(rev scope)
      | [NewScopeTok]::tail -> (
          let shead = getHead scope in
          let stail = getTail scope in
          let list = getLineList shead in
          let head = Line(list, (makeScopeTreeRec tail (Block([])) index lineNum), lineNum) in
          let newTail = skipLines tail (!index - oldIndex) in
          makeScopeTreeRec newTail (Block(head::stail)) index lineNum
        )
      | line::tail -> (
          let lineNum = getHead line in
          let line = getTail line in
          match lineNum with
          | LineNumTok(lineNum) ->
            makeScopeTreeRec tail (Block((Line(line, Block([]), lineNum))::scope)) index lineNum
          | _ -> raise (HowDidWeGetHere(0))
        )
    )
);;

let makeScopeTree lines = makeScopeTreeRec lines (Block([])) (ref (-1)) 0;;


let rec printSpace num = (
  if (num > 0) then (
    Printf.printf " ";
    printSpace (num-1)
  ) else ()
  
);;

let rec printScopeRec scope indent lineNumWidth = (
  match scope with
  | Block(lines) -> (
      match lines with
      | [] -> ()
      | line::tail -> (
          let num = getLineNum line in
          let toks = getLineList line in
          let numWidth = int_of_float (log10 (float_of_int num)) in
          printSpace (lineNumWidth-numWidth);
          Printf.printf "%d | " num;
          printLine toks indent;
          let ctrl = getLineScope line in
          printScopeRec ctrl (indent+1) lineNumWidth;
          printScopeRec (Block(tail)) indent lineNumWidth
        )
    )
);;

let printScope scope lineNumWidth = printScopeRec scope 0 lineNumWidth;;


let rec countNewLines tokens acc = (
  match tokens with
  | [] -> acc
  | NewLineTok::tail -> countNewLines tail (acc+1)
  | _::tail -> countNewLines tail acc
);;
