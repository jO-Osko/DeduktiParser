module MMap = Map

module Map = struct
  module type S = sig
    include MMap.S

    type 'a entry = key * 'a

    val of_bindings : 'a entry list -> 'a t
    val union_overwrite : 'a t -> 'a t -> 'a t
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
    val add_list : 'a entry list -> 'a t -> 'a t
    val add_option : 'a entry option -> 'a t -> 'a t
  end

  module Make (Ord : MMap.OrderedType) : S with type key = Ord.t = struct
    include MMap.Make (Ord)

    type 'a entry = key * 'a

    let of_bindings list =
      List.fold_left (fun map (key, v) -> add key v map) empty list

    let union_overwrite m1 m2 = union (fun _ v1 _ -> Some v1) m1 m2
    let keys m = List.map fst (bindings m)
    let values m = List.map snd (bindings m)

    let add_list list map =
      List.fold_left (fun map (key, v) -> add key v map) map list

    let add_option option map =
      match option with None -> map | Some (key, v) -> add key v map
  end
end

module Counter = struct
  module type S = sig
    include Map.S

    val plus : key -> int -> int t -> int t
    val most_common : int t -> (key * int) list
  end

  module Make (Ord : MMap.OrderedType) : S with type key = Ord.t = struct
    include Map.Make (Ord)

    let plus k v m =
      update k (function None -> Some v | Some v' -> Some (v + v')) m

    let most_common m =
      m |> bindings |> List.sort (fun (_, v1) (_, v2) -> Stdlib.compare v2 v1)
  end
end

let string_of_loc loc =
  let x, y = Kernel.Basic.of_loc loc in
  "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let pair_of_name name =
  let md = Kernel.Basic.md name in
  let id = Kernel.Basic.id name in
  (md, id)

let string_of_name (nm : Kernel.Basic.name) =
  "[\""
  ^ Kernel.Basic.(md nm |> string_of_mident)
  ^ "\",\""
  ^ Kernel.Basic.(id nm |> string_of_ident)
  ^ "\"]"
