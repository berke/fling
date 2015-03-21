(* Fling *)

open Sexplib
open Conv

type expr =
  | Alt of expr list
  | Fields of string list * expr
  | Seq of expr list
  | Const of string
  | Emit
  | Newline
  | Tag of string * expr
with sexp

module Opt = struct
  let args : string list ref = ref []
  let start = ref ""
  let sep   = ref " "
  let stop  = ref "\n"
end

module Spec = struct
  open Arg
  open Opt

  let specs =
    align @@
    [
      "--start",
      Set_string start,
      "<string> Field started";

      "--sep",
      Set_string sep,
      "<string> Field separator";

      "--stop",
      Set_string stop,
      "<string> Field terminator";

    ]
end

let wrap x cleanup f =
  try
    let y = f x in
    cleanup x;
    y
  with
  | e ->
    cleanup x;
    raise e

open Sexp

let parse expr =
  try
    Sexp.of_string expr |> expr_of_sexp
  with
  | Pre_sexp.Of_sexp_error(x,y) ->
    Printf.eprintf "%s: Bad expression\n%a\n%!" Sys.argv.(0)
      Sexp.output_hum y;
    exit 1

let rec sexp_assoc sx = function
  | [] -> raise Not_found
  | List[sx'; y] :: _ when sx = sx' -> y
  | _ :: rest -> sexp_assoc sx rest

type context =
  {
    mutable fields : int
  }

let emit ctx u =
  if ctx.fields = 0 then output_string stdout !Opt.start;
  if ctx.fields > 0 then output_string stdout !Opt.sep;
  ctx.fields <- ctx.fields + 1;
  output_string stdout u

let rec execute ctx expr sx = 
  match expr, sx with
  | Tag(s, expr'), List[Atom s'; sx'] when s = s' -> execute ctx expr' sx'
  | Alt[], _ -> ()
  | Alt(expr1 :: rest), _ -> execute ctx expr1 sx; execute ctx (Alt rest) sx
  | Seq[], _ -> ()
  | Seq(expr1 :: rest), _ -> execute ctx expr1 sx; execute ctx (Seq rest) sx
  | Const u, _ -> emit ctx u
  | Emit, Atom u -> emit ctx u
  | Emit, _ -> emit ctx (Sexp.to_string sx)
  | Newline, _ ->
    ctx.fields <- 0;
    output_string stdout !Opt.stop
  | Fields(us, expr'), List l ->
    List.iter
      (fun u ->
         try
           let sx' = sexp_assoc (Atom u) l in
           execute ctx expr' sx'
         with
         | Not_found -> ()
      )
      us
  | _ -> ()

let perform expr ic =
  try
    while true do 
      let sx = Sexp.input_sexp ic in
      execute { fields = 0 } expr sx
    done
  with
  | End_of_file -> ()

let main = function
  | [expr] ->
    perform (parse expr) stdin
  | expr :: rest ->
    List.iter
      (fun fn ->
         wrap (open_in fn) close_in (perform (parse expr)))
      rest
  | [] -> raise (Arg.Bad "No expression specified")

let _ =
  (
    try
      Arg.parse
        Spec.specs
        (fun u -> Opt.args := u :: !Opt.args)
        (Printf.sprintf "Usage: %s [options] <expr> [file]" Sys.argv.(0));
    with
    | Arg.Bad b ->
      Printf.fprintf Pervasives.stderr "%s\n%s\n"
        b
        (Arg.usage_string Spec.specs "Usage:")
  );
  main (List.rev !Opt.args)
