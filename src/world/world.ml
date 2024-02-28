(*
Copyright (c) 2024 Raphaël Proust

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

let name = "sudoku"

let cartesian_product xs ys =
  List.fold_left (fun acc x -> List.fold_left (fun acc y -> (x, y) :: acc) acc ys) [] xs

module TerminatedSeq
: sig
  type ('a, 'b) node =
    | Nil of 'b
    | Cons of 'a * ('a, 'b) t
  and ('a, 'b) t = unit -> ('a, 'b) node
  val append : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val terminate : 'b -> ('a, 'b) t
  val iter : ('a -> unit) -> ('a, 'b) t -> 'b
  val map_terminator : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val to_seq : ('a, unit) t -> 'a Seq.t
end
= struct
  type ('a, 'b) node =
    | Nil of 'b
    | Cons of 'a * ('a, 'b) t
  and ('a, 'b) t = unit -> ('a, 'b) node
  let rec append s k () =
    match s () with
    | Nil b -> k b ()
    | Cons (a, s) -> Cons (a, append s k)
  let terminate x () = Nil x
  let rec iter f s =
    match s () with
    | Nil v -> v
    | Cons (x, s) -> f x; iter f s
  let rec map_terminator f s () =
    match s () with
    | Nil v -> Nil (f v)
    | Cons (x, s) -> Cons (x, map_terminator f s)
  let rec to_seq s () =
    match s () with
    | Nil () -> Seq.Nil
    | Cons (x, s) -> Seq.Cons (x, to_seq s)
end

module Main
(Size: sig val v : int end)
= struct

  let () = assert (Size.v = 4 || Size.v = 9)
  let size = Size.v
  let sqrtsize = if size = 4 then 2 else if size = 9 then 3 else assert false

  module Symbol
  : sig
    type t
    val none : t
    val all : t list
    val compare : t -> t -> int
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end
  = struct
    type t = int
    let none = 0
    let all = List.init size succ
    let compare = Stdlib.compare
    let to_string t =
       if t = none then "-" else string_of_int t
    let pp fmt t =
      if t = none then
        Format.fprintf fmt " "
      else
        Format.fprintf fmt "%d" t
  end

  module Space
  : sig
    type offset = private int
    val offsets : offset list

    type coordinate = { vertical: offset; horizontal: offset }
    val coordinates : coordinate list
    val pp_coordinates : Format.formatter -> coordinate -> unit

    type region = private int
    val region_of_coordinates : coordinate -> region
    val coordinates_of_region : region -> coordinate list
  end
  = struct
    type offset = int
    let offsets = List.init size Fun.id

    type coordinate = { vertical: offset; horizontal: offset }
    let coordinates =
      List.map
        (fun (vertical, horizontal) -> {vertical; horizontal})
        (cartesian_product offsets offsets)
    let pp_coordinates fmt { vertical; horizontal } =
      Format.fprintf fmt "%d-%d" vertical horizontal

    type region = int
    let region_of_coordinates {vertical; horizontal} =
      let low = horizontal / sqrtsize in
      let high = vertical / sqrtsize in
      high * sqrtsize + low
    let region_map =
      Array.init size (fun vertical ->
        Array.init size (fun horizontal ->
          region_of_coordinates {vertical; horizontal}
        )
      )
    let region_of_coordinates {vertical; horizontal} =
      region_map.(vertical).(horizontal)
    let coordinates_of_region_map =
      Array.init size (fun region ->
        List.filter
          (fun coordinate ->
            region_of_coordinates coordinate = region
          )
          coordinates
      )
    let coordinates_of_region region = coordinates_of_region_map.(region)
  end

  module Board
  : sig
    type t
    val make : unit -> t
    val copy : t -> t
    val read : t -> Space.coordinate -> Symbol.t
    val write : t -> Space.coordinate -> Symbol.t -> unit
    val pp : Format.formatter -> t -> unit
  end
  = struct
    type t = Symbol.t array array
    let make () =
      Array.init size (fun _vertical ->
        Array.init size (fun _horizontal ->
          Symbol.none
        )
      )
    let copy t =
      Array.init size (fun vertical ->
        Array.init size (fun horizontal ->
          t.(vertical).(horizontal)
        )
      )
    let read t {Space.vertical; horizontal} =
      t.((vertical :> int)).((horizontal :> int))
    let write t {Space.vertical; horizontal} symbol =
      t.((vertical :> int)).((horizontal :> int)) <- symbol
    let pp4 fmt t =
      match t with
      | [|
          [|s00; s01; s02; s03|];
          [|s10; s11; s12; s13|];
          [|s20; s21; s22; s23|];
          [|s30; s31; s32; s33|];
        |] ->
            Format.fprintf fmt
                "┏━┯━┳━┯━┓\n\
                 ┃%a│%a┃%a│%a┃\n\
                 ┠─┼─╂─┼─┨\n\
                 ┃%a│%a┃%a│%a┃\n\
                 ┣━┿━╋━┿━┫\n\
                 ┃%a│%a┃%a│%a┃\n\
                 ┠─┼─╂─┼─┨\n\
                 ┃%a│%a┃%a│%a┃\n\
                 ┗━┷━┻━┷━┛\n"
                 Symbol.pp s00 Symbol.pp s01 Symbol.pp s02 Symbol.pp s03
                 Symbol.pp s10 Symbol.pp s11 Symbol.pp s12 Symbol.pp s13
                 Symbol.pp s20 Symbol.pp s21 Symbol.pp s22 Symbol.pp s23
                 Symbol.pp s30 Symbol.pp s31 Symbol.pp s32 Symbol.pp s33
      | _ -> assert false
    let pp9 fmt t =
      match t with
      | [|
          [|s00; s01; s02; s03; s04; s05; s06; s07; s08|];
          [|s10; s11; s12; s13; s14; s15; s16; s17; s18|];
          [|s20; s21; s22; s23; s24; s25; s26; s27; s28|];
          [|s30; s31; s32; s33; s34; s35; s36; s37; s38|];
          [|s40; s41; s42; s43; s44; s45; s46; s47; s48|];
          [|s50; s51; s52; s53; s54; s55; s56; s57; s58|];
          [|s60; s61; s62; s63; s64; s65; s66; s67; s68|];
          [|s70; s71; s72; s73; s74; s75; s76; s77; s78|];
          [|s80; s81; s82; s83; s84; s85; s86; s87; s88|];
        |] ->
            Format.fprintf fmt
                "┏━┯━┯━┳━┯━┯━┳━┯━┯━┓\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┣━┿━┿━╋━┿━┿━╋━┿━┿━┫\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┣━┿━┿━╋━┿━┿━╋━┿━┿━┫\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n\
                 ┃%a│%a│%a┃%a│%a│%a┃%a│%a│%a┃\n\
                 ┗━┷━┷━┻━┷━┷━┻━┷━┷━┛\n"
                 Symbol.pp s00 Symbol.pp s01 Symbol.pp s02 Symbol.pp s03 Symbol.pp s04 Symbol.pp s05 Symbol.pp s06 Symbol.pp s07 Symbol.pp s08
                 Symbol.pp s10 Symbol.pp s11 Symbol.pp s12 Symbol.pp s13 Symbol.pp s14 Symbol.pp s15 Symbol.pp s16 Symbol.pp s17 Symbol.pp s18
                 Symbol.pp s20 Symbol.pp s21 Symbol.pp s22 Symbol.pp s23 Symbol.pp s24 Symbol.pp s25 Symbol.pp s26 Symbol.pp s27 Symbol.pp s28
                 Symbol.pp s30 Symbol.pp s31 Symbol.pp s32 Symbol.pp s33 Symbol.pp s34 Symbol.pp s35 Symbol.pp s36 Symbol.pp s37 Symbol.pp s38
                 Symbol.pp s40 Symbol.pp s41 Symbol.pp s42 Symbol.pp s43 Symbol.pp s44 Symbol.pp s45 Symbol.pp s46 Symbol.pp s47 Symbol.pp s48
                 Symbol.pp s50 Symbol.pp s51 Symbol.pp s52 Symbol.pp s53 Symbol.pp s54 Symbol.pp s55 Symbol.pp s56 Symbol.pp s57 Symbol.pp s58
                 Symbol.pp s60 Symbol.pp s61 Symbol.pp s62 Symbol.pp s63 Symbol.pp s64 Symbol.pp s65 Symbol.pp s66 Symbol.pp s67 Symbol.pp s68
                 Symbol.pp s70 Symbol.pp s71 Symbol.pp s72 Symbol.pp s73 Symbol.pp s74 Symbol.pp s75 Symbol.pp s76 Symbol.pp s77 Symbol.pp s78
                 Symbol.pp s80 Symbol.pp s81 Symbol.pp s82 Symbol.pp s83 Symbol.pp s84 Symbol.pp s85 Symbol.pp s86 Symbol.pp s87 Symbol.pp s88
      | _ -> assert false
    let pp fmt t =
      if size = 4 then pp4 fmt t
      else if size = 9 then pp9 fmt t
      else assert false
  end

  (* board access syntax *)
  let ( .:[] ) = Board.read
  let ( .:[]<- ) = Board.write

  module Constraints
  : sig
    (* [true] if there are no conflicts for the symbol at the coordinates on the board *)
    val is_ok : Board.t -> Space.coordinate -> Symbol.t -> bool
  end
  = struct
    let is_ok_vertically board ~horizontal value =
      List.for_all
        (fun vertical -> value != board.:[{vertical; horizontal}])
        Space.offsets
    let is_ok_horizontally board ~vertical value =
      List.for_all
        (fun horizontal -> value != board.:[{vertical; horizontal}])
        Space.offsets
    let is_ok_squarelly board ~vertical ~horizontal value =
      List.for_all
        (fun coordinate -> value != board.:[coordinate])
        (Space.coordinates_of_region (Space.region_of_coordinates {vertical;
        horizontal}))
    let is_ok board {Space.vertical; horizontal} value =
      is_ok_vertically board ~horizontal value
      && is_ok_horizontally board ~vertical value
      && is_ok_squarelly board ~vertical ~horizontal value
  end

  module Solve
  : sig
    (* [solve board] is a pair of a board and an sequence of human-readable
       messages describing the process of solving the [board].
       Each step modifies the returned board: the sequence is ephemeral. *)
    val solve : Board.t -> Board.t * (string, bool) TerminatedSeq.t

    val states : Board.t -> (string * Board.t) Seq.t
  end
  = struct

    let rec solve_board init working coordinates () =
      match coordinates with
      | [] ->
          TerminatedSeq.Cons ("Solved!", TerminatedSeq.terminate true)
      | here :: there ->
          if Symbol.compare Symbol.none (Board.read init here) <> 0 then begin
            assert (Board.read working here = Board.read init here);
            let msg =
              Format.asprintf
                "Constraint at %a, continuing onto the rest of the board"
                Space.pp_coordinates
                here
            in
            TerminatedSeq.Cons (msg, solve_board init working there)
          end else
            solve_here init working here there Symbol.all ()

    and solve_here init working here there symbols () =
      match symbols with
      | [] ->
          (* no more symbols, go back *)
          Board.write working here Symbol.none;
          let msg =
            Format.asprintf
              "Ran out of symbols for %a, erased, backtracking"
              Space.pp_coordinates
              here
          in
          TerminatedSeq.Cons (msg, TerminatedSeq.terminate false)
      | this :: that ->
          if Constraints.is_ok working here this then begin
            Board.write working here this;
            let msg =
              Format.asprintf
                "Written symbol %a at %a, solving the rest of the board"
                Symbol.pp
                this
                Space.pp_coordinates
                here
            in
            TerminatedSeq.Cons (msg,
              (TerminatedSeq.append
                (solve_board init working there)
                (function
                  | true -> TerminatedSeq.terminate true
                  | false -> solve_here init working here there that)
              ))
          end else
            let msg =
              Format.asprintf
                "Cannot write %a at %a, trying the next symbol"
                Symbol.pp this
                Space.pp_coordinates here
            in
            TerminatedSeq.Cons (msg, solve_here init working here there that)

    let solve init =
      let working = Board.copy init in
      (working, solve_board init working Space.coordinates)

    let states init =
      let (board, s) = solve init in
      let s = TerminatedSeq.(to_seq @@ map_terminator ignore s) in
      Seq.memoize (Seq.map (fun msg -> msg, Board.copy board) s)
  end

end

module M = Main (struct let v = 4 end)

let initboard =
  let board = M.Board.make () in
  let () =
    let w c s =
      M.Board.write
        board
        (List.nth M.Space.coordinates c)
        (List.nth M.Symbol.all s)
    in
    w 4 0;
    w 6 1;
    w 10 2;
    w 15 3;
    ()
  in
  board

let boardseq = M.Solve.states initboard
let boards : (string * M.Board.t) list = List.of_seq boardseq
