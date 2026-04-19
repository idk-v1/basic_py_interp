(*
   Name: Ben Hamilton
   File: python.ml
*)

#use "execpython.ml"


let readFile name = (
  let list = In_channel.with_open_text name In_channel.input_lines in
  let rec strcat list = (
    match list with
    | [] -> ""
    | str::tail -> str^"\n"^(strcat tail)
  ) in
  strcat list 
);;

let rec readCLArg i = (
  if (i < Array.length Sys.argv) then (
    Printf.printf "[%d] %s\n" i Sys.argv.(i);
    let prog = readFile Sys.argv.(i) in
    if (i+1 < Array.length Sys.argv) then (
      if (Sys.argv.(i+1) = "debug") then (
        execPython prog true;
        readCLArg (i+2)
      ) else (
        execPython prog false;
        readCLArg (i+1)
      )
    )
    else
      execPython prog false
  )
  else ()
);;

readCLArg 1;;
