type point = int * int
module Point = struct
  type t = point
  let compare = compare
  [@@deriving sexp]
end

module GridMap = Map.Make(Point)
type 'a t = 'a GridMap.t
(* Simple 2x2 grid*)
let m = GridMap.empty
  |> GridMap.add (0,0) 1
  |> GridMap.add (1,0) 2
  |> GridMap.add (0,1) 3
  |> GridMap.add (1,1) 4

let%test _ = GridMap.find (0,0) m = 1

(* Larger grid*)
let makeGrid n f =
  let indices = List.init n Fun.id in
  List.fold_left (fun m x -> 
    List.fold_left (fun m y -> 
    GridMap.add (x,y) (f (x, y)) m) m indices) GridMap.empty indices 
let m = makeGrid 10 (fun (x,y) -> x + y)
let%test _ = GridMap.find (3,3) m = 6
(* Use case : modifying - can we do that?*)
let m = GridMap.add (3,3) 60 m
let%test _ = GridMap.find (3,3) m = 60