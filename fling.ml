(* Fling *)

open Sexplib
open Conv

type expr =
  | Field of string * expr
  | Fields of string list * expr
  | Index of int * expr
  | Indices of int list * expr
  | All of expr
  | Seq of expr list
  | Alt of expr list
  | Equal of string
  | Const of string
  | Emit
  | Pad of int * expr
  | Tag of string * expr
  with sexp

exception Mismatch

module Opt = struct
    let args : string list ref = ref []
    let start = ref ""
    let sep   = ref " "
    let stop  = ref "\n"
    let file : string option ref = ref None
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

	  "--file",
	  String(fun fn -> file := Some fn),
	  "<file> Process given input file instead of stdin";
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
    mutable emission : string list;
    mutable padding  : int;
  }

let backup ctx = { ctx with emission = ctx.emission }

let restore ctx ctx' =
  ctx'.emission <- ctx.emission;
  ctx'.padding <- ctx.padding

let emit ctx u =
  let m = String.length u in
  let u =
    if ctx.padding > 0 then
      if m < ctx.padding then
        u ^ String.make (ctx.padding - m) ' '
      else
        String.sub u 0 ctx.padding
    else
      u
  in
  ctx.emission <- u :: ctx.emission

let flush ctx =
  output_string stdout !Opt.start;
  match ctx.emission with
  | [] -> ()
  | fields ->
     let i = ref 0 in
     List.iter
       (fun u ->
        if !i > 0 then output_string stdout !Opt.sep;
        incr i;
        output_string stdout u
       )
       (List.rev fields);
     output_string stdout !Opt.stop;
     ctx.emission <- []

let rec execute ctx expr sx = 
  let fields us expr' l =
    List.iter
      (fun u ->
       try
         let sx' = sexp_assoc (Atom u) l in
         execute ctx expr' sx'
       with
       | Not_found -> ()
      )
      us
  in
  let indices ns expr' l =
    let sxs' = try List.map (List.nth l) ns with _ -> raise Mismatch in
    List.iter (execute ctx expr') sxs'
  in
  match expr, sx with
  | Tag(s, expr'), List[Atom s'; sx'] ->
     if s = s' then
       execute ctx expr' sx'
     else
       raise Mismatch
  | Seq[], _ -> ()
  | Seq(expr1 :: rest), _ -> execute ctx expr1 sx; execute ctx (Seq rest) sx
  | Alt[], _ -> ()
  | Alt(expr1 :: rest), _ ->
     (
       let ctx' = backup ctx in
       try
         execute ctx expr1 sx
       with
       | Mismatch ->
          restore ctx' ctx;
          execute ctx (Alt rest) sx
     )
  | Const u, _ -> emit ctx u
  | Equal u, Atom v -> if u <> v then raise Mismatch
  | Emit, Atom u -> emit ctx u
  | Emit, _ -> emit ctx (Sexp.to_string sx)
  | Pad(n, expr), _ ->
     let padding = ctx.padding in
     ctx.padding <- n;
     execute ctx expr sx;
     ctx.padding <- padding
  | Field(u, expr'), List l -> fields [u] expr' l
  | Fields(us, expr'), List l -> fields us expr' l
  | Index(n, expr'), List l -> indices [n] expr' l
  | Indices(ns, expr'), List l -> indices ns expr' l
  | All expr', List l -> List.iter (execute ctx expr') l
  | _ -> ()

let perform ic expr =
  try
    while true do 
      let sx = Sexp.input_sexp ic in
      let ctx = { emission = []; padding = 0 } in
      try
        execute ctx expr sx;
        flush ctx
      with
      | Mismatch -> ()
    done
  with
  | End_of_file -> ()

let main exprs =
  let on_file f =
    match !Opt.file with
    | None -> f stdin
    | Some fn -> wrap (open_in fn) close_in f
  in
  match exprs with
  | [] -> raise (Arg.Bad "No expression specified")
  | exprs ->
     let exprs' = List.map parse exprs in
     on_file (fun ic -> List.iter (perform ic) exprs')

let _ =
  (
    try
      Arg.parse
        Spec.specs
        (fun u -> Opt.args := u :: !Opt.args)
        (Printf.sprintf "Usage: %s [options] [--file <file>] <expr1...>" Sys.argv.(0));
    with
    | Arg.Bad b ->
       Printf.fprintf Pervasives.stderr "%s\n%s\n"
		      b
		      (Arg.usage_string Spec.specs "Usage:")
  );
  main (List.rev !Opt.args)
