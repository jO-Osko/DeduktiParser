open Parsers.Entry
open Kernel.Basic
open Kernel.Term
open Kernel.Rule
open Utils

(* open Kernel.Term *)

module IdentMap = Map.Make (struct
  type t = ident

  let compare = compare
end)

module IdentSet = Set.Make (struct
  type t = ident

  let compare = compare
end)

module LocMap = Map.Make (struct
  type t = loc

  let compare = compare
end)

module NameMap = Map.Make (struct
  type t = name

  let compare = compare
end)

type ptr = string printer

let parsed_result filename =
  let input = Parsers.Parser.input_from_file filename in
  let parsed = Parsers.Parser.parse input in
  (filename, parsed)

let reverse_tuple (a, b) = (b, a)
let r_toplevel toplevel t = if toplevel then reverse_tuple t else t
let flat_tuple (a, b) = a @ b

type 'a type_info = { hypothesis : 'a list; conclusion : 'a list }

type ('a, 'b) term_info = {
  names : 'a type_info; (* Global names *)
  idents : 'b type_info; (* local deBruijn *)
}

let combine ti1 ti2 =
  {
    hypothesis = ti1.hypothesis @ ti2.hypothesis;
    conclusion = ti1.conclusion @ ti2.conclusion;
  }

let combine_term_info t1 t2 =
  { names = combine t1.names t2.names; idents = combine t1.idents t2.idents }

let add_hypothesis h ti = { ti with hypothesis = h :: ti.hypothesis }
let add_conclusion c ti = { ti with conclusion = c :: ti.conclusion }
let empty_type_info = { hypothesis = []; conclusion = [] }
let empty_term_info = { names = empty_type_info; idents = empty_type_info }
let flat_info { hypothesis; conclusion } = hypothesis @ conclusion

let string_of_type_info { hypothesis; conclusion } =
  "{\"hypotehsis\":["
  ^ (List.map string_of_name hypothesis |> String.concat ", ")
  ^ "], \"conclusion\":["
  ^ (List.map string_of_name conclusion |> String.concat ", ")
  ^ "]}"

let rec process_term ?(toplevel = false) (term : term) =
  (* Printf.printf "Processing term: %s\n" (string_of pp_term term); *)
  match term with
  | Kind ->
      (* Printf.printf "Kind\n"; *)
      empty_term_info
      (* TODO: Somehow make type a first class citizen *)
  | Type _ ->
      (* Printf.printf "Type %s\n" (string_of_loc l); *)
      empty_term_info
  | DB (_, ident, _) ->
      (* Printf.printf "DB %s\n" (string_of_ident ident); *)
      {
        empty_term_info with
        idents =
          (if toplevel then add_conclusion ident empty_type_info
           else add_hypothesis ident empty_type_info);
      }
  | Const (_loc, name) ->
      (* Printf.printf "Const %s %s\n" (string_of_loc loc) (string_of_name name); *)
      {
        empty_term_info with
        names =
          (if toplevel then add_conclusion name empty_type_info
           else add_hypothesis name empty_type_info);
      }
  | App (t1, t2, tl) ->
      t1 :: t2 :: tl
      |> List.map (process_term ~toplevel)
      |> List.fold_left combine_term_info empty_term_info
  | Lam (_, _, at, rt) ->
      let args =
        at
        |> Option.map (process_term ~toplevel:false)
        |> Option.value ~default:empty_term_info
      in
      let t_info' = process_term ~toplevel rt in
      combine_term_info args t_info'
  | Pi (_, _, t1, t2) ->
      (* TODO properly check ident *)
      let t_info = process_term t1 ~toplevel:false in
      let t_info' = process_term ~toplevel t2 in
      combine_term_info t_info t_info'

let combine_list l1 l2 =
  let a, b = List.split l1 in
  let c, d = List.split l2 in
  (a @ c, b @ d)

let rec process_pattern (pat : pattern) : name list * ident list =
  match pat with
  | Var (_, ident, _, plist) ->
      let a, b = plist |> List.map process_pattern |> List.split in
      (List.flatten a, ident :: List.flatten b)
  | Pattern (_, name, plist) ->
      let a, b = plist |> List.map process_pattern |> List.split in
      (name :: List.flatten a, List.flatten b)
  | Lambda (_, _, p) ->
      (* We can't get a type here so this is ok *)
      process_pattern p
  | Brackets term ->
      let type_info = process_term term in
      (flat_info type_info.names, flat_info type_info.idents)

type rule_info = {
  name : string;
  pattern_names : name list; (* Global names used in pattern  *)
  pattern_params : ident list; (* Params used in patterns *)
  pattern_types : name type_info IdentMap.t; (* Optionaly annotated params  *)
  rhs_names : (name, ident) term_info;
}

type fold_state = {
  ident_loc_map : ident LocMap.t;
  collected_defs : (name * name type_info * name list) list;
  collected_rules : rule_info list;
  mident : mident;
}

let main filename =
  Printf.printf "Processing file: %s\n" filename;
  let filename, parsed = parsed_result filename in
  let _filename = String.sub filename 0 (String.length filename - 3) in
  let initial_state =
    {
      ident_loc_map = LocMap.empty;
      collected_defs = [];
      collected_rules = [];
      mident = mk_mident _filename;
    }
  in
  let state =
    List.fold_left
      (fun state e ->
        let nw, data', rule_data, name_pragma =
          match e with
          | Decl (loc, ident, _, _, t1) ->
              ( Some (loc, ident),
                [ (ident, (process_term ~toplevel:true t1).names, []) ],
                [],
                None )
          | Def (loc, ident, _, _, t1, t2) ->
              ( Some (loc, ident),
                [
                  ( ident,
                    (t1
                    |> Option.map (process_term ~toplevel:true)
                    |> Option.value ~default:empty_term_info)
                      .names,
                    (process_term ~toplevel:true t2).names |> flat_info );
                ],
                [],
                None )
          | Rules (_loc, partially_typed_rule_list) ->
              let data =
                List.map
                  (fun ({ name; ctx; pat; rhs } : partially_typed_rule) ->
                    let term_data = process_term ~toplevel:false rhs in
                    let names, idents = process_pattern pat in
                    let param_types, _params =
                      List.fold_left
                        (fun (t, l) (_, ident, t') ->
                          assert (not (IdentSet.mem ident l));
                          ( t
                            |> IdentMap.add_option
                                 (Option.map
                                    (fun t -> (ident, (process_term t).names))
                                    t'),
                            l |> IdentSet.add ident ))
                        (IdentMap.empty, IdentSet.empty)
                        ctx
                    in
                    {
                      name = string_of pp_rule_name name;
                      pattern_names = names;
                      pattern_params = idents;
                      pattern_types = param_types;
                      rhs_names = term_data;
                    })
                  partially_typed_rule_list
              in
              (None, [], data, None)
          | Name (_, mident) -> (None, [], [], Some mident)
          | _ -> (None, [], [], None)
        in
        let m = state.ident_loc_map |> LocMap.add_option nw in
        {
          ident_loc_map = m;
          collected_defs =
            (data'
            |> List.map (fun (ident, a, b) ->
                   (mk_name state.mident ident, a, b)))
            @ state.collected_defs;
          collected_rules = rule_data @ state.collected_rules;
          mident = Option.value ~default:state.mident name_pragma;
        })
      initial_state parsed
  in

  List.iter
    (fun (name, type_info, term_info) ->
      Printf.printf "Name: %s \n Type: %s \n Term: %s \n\n"
        (string_of_name name)
        (string_of_type_info type_info)
        (String.concat "," (List.map string_of_name term_info)))
    state.collected_defs;
  List.iter
    (*  Rule name is ussually the name of the first thing in the rule syntax*)
      (fun rinfo ->
      Printf.printf
        "Rule: %s\n\
         PatternTypes: {%s}\n\
         LHSParams:[%s]\n\
         LHSNames:[%s]\n\
         RHSNames:[%s]\n\
         RHSIdents:[%s]\n\n"
        rinfo.name
        (rinfo.pattern_types |> IdentMap.bindings
        |> List.map (fun (ident, t_info) ->
               "\"" ^ string_of_ident ident ^ "\":" ^ string_of_type_info t_info)
        |> String.concat ",")
        (rinfo.pattern_params
        |> List.map (fun ident -> "\"" ^ string_of_ident ident ^ "\"")
        |> String.concat ",")
        (rinfo.pattern_names
        |> List.map (fun name -> string_of_name name)
        |> String.concat ",")
        (rinfo.rhs_names.names |> flat_info
        |> List.map (fun name -> string_of_name name)
        |> String.concat ",")
        (rinfo.rhs_names.idents |> flat_info
        |> List.map (fun ident -> "\"" ^ string_of_ident ident ^ "\"")
        |> String.concat ","))
    state.collected_rules;
  let _ = state.collected_defs in
  ()

let () =
  Printexc.record_backtrace true;
  for i = 1 to Array.length Sys.argv - 1 do
    main Sys.argv.(i)
  done
