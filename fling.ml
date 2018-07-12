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
  | Raw of string
  | Emit
  | Pad of int * expr
  | Tag of string * expr
  | Tight of expr
  | Sep of string * expr
  | Select of range * expr
and range =
  | Full
  | Empty
  | Inter of range * range
  | Union of range * range
  | From of prop
  | While of prop
  | First of prop
and prop =
  | Atomic of string
  | And of prop list
  | Or of prop list
  | Not of prop
  [@@deriving sexp]

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
    mutable emission  : string list;
    mutable padding   : int;
    mutable separator : string;
    mutable first     : bool;
  }

let backup ctx = { ctx with emission = ctx.emission }

let restore ?(emission=true) ctx ctx' =
  if emission then
    (
      ctx'.emission <- ctx.emission;
      ctx'.first <- ctx.first
    );
  ctx'.padding <- ctx.padding;
  ctx'.separator <- ctx.separator

let with_context ctx f =
  let old = backup ctx in
  try
    let y = f ctx in
    restore ~emission:false old ctx;
    y
  with
  | e ->
     restore old ctx;
     raise e

let emit ?(separate=true) ctx u =
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
  ctx.emission <-
    if separate then
      if ctx.first then
	(
	  ctx.first <- false;
	  u :: ctx.emission
	)
      else
	u :: ctx.separator :: ctx.emission
    else
      u :: ctx.emission

let flush ctx =
  output_string stdout !Opt.start;
  match ctx.emission with
  | [] -> ()
  | fields ->
     List.iter
       (fun u -> output_string stdout u)
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
  let push_sep sep expr sx =
    with_context ctx
    @@ fun ctx ->
       ctx.separator <- sep;
       execute ctx expr sx
  in
  let range rng l =
    let a = Array.of_list l in
    let m = Array.length a in
    let indicator f = Array.init m (fun i -> f i a.(i)) in
    let rec eval = function
      | Full -> indicator (fun _ _ -> true)
      | Empty -> indicator (fun _ _ -> false)
      | Inter(rng1, rng2) ->
	 let u1, u2 = eval rng1, eval rng2 in
	 indicator (fun i _ -> u1.(i) && u2.(i))
      | Union(rng1, rng2) ->
	 let u1, u2 = eval rng1, eval rng2 in
	 indicator (fun i _ -> u1.(i) || u2.(i))
      | From p ->
	 let b = eval_prop p in
	 let q = ref false in
	 indicator (fun i _ -> let x = !q || b.(i) in q := !q || b.(i); x)
      | While p ->
	 let b = eval_prop p in
	 let q = ref true in
	 indicator (fun i _ -> let x = !q && b.(i) in q := !q && b.(i); x)
      | First p ->
	 let b = eval_prop p in
	 let q = ref true in
	 indicator (fun i _ -> let x = !q && b.(i) in q := !q && not b.(i); x)
    and eval_prop p = indicator (prop p)
    and prop q i x =
      match q, x with
      | Atomic u, Atom u' when u = u' -> true
      | And pl, _ -> List.for_all (fun q' -> prop q' i x) pl
      | Or pl, _ -> List.exists (fun q' -> prop q' i x) pl
      | Not q', _ -> not (prop q' i x)
      | _ -> false
    in
    let r = eval rng in
    let rec loop res i =
      if i = m then
	res
      else
	loop (if r.(i) then a.(i) :: res else res) (i + 1)
    in
    List.rev (loop [] 0)
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
  | Raw u, _ -> emit ~separate:false ctx u; ctx.first <- true
  | Equal u, Atom v -> if u <> v then raise Mismatch
  | Emit, Atom u -> emit ctx u
  | Emit, _ -> emit ctx (Sexp.to_string sx)
  | Pad(n, expr), _ -> let padding = ctx.padding in ctx.padding <- n; execute ctx expr sx; ctx.padding <- padding
  | Tight expr, _ ->
     emit ctx "";
     ctx.first <- true;
     push_sep "" expr sx
  | Sep(u, expr), _ ->
     emit ctx "";
     ctx.first <- true;
     push_sep u expr sx
  | Field(u, expr'), List l -> fields [u] expr' l
  | Fields(us, expr'), List l -> fields us expr' l
  | Index(n, expr'), List l -> indices [n] expr' l
  | Indices(ns, expr'), List l -> indices ns expr' l
  | Select(rng, expr'), List l -> execute ctx expr' (List(range rng l))
  | All expr', List l -> List.iter (execute ctx expr') l
  | _ -> ()

let perform ic expr =
  try
    while true do 
      let sx = Sexp.input_sexp ic in
      let ctx = { emission = []; padding = 0; separator = !Opt.sep; first = true } in
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
