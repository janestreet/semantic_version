module Stable = struct
  open Core.Core_stable

  module V1 = struct
    module Pre_release_tags = struct
      type tag = string [@@deriving bin_io, stable_witness]
      type t = tag list [@@deriving bin_io, stable_witness]

      let compare_tag t1 t2 =
        let maybe_int x = Core.(Option.try_with (fun () -> Int.of_string x)) in
        match maybe_int t1, maybe_int t2 with
        | None, None -> [%compare: string] t1 t2
        | Some x, Some y -> [%compare: int] x y
        | None, Some _ -> 1
        | Some _, None -> -1
      ;;

      let compare t1 t2 =
        (* The pre-release tag list comparison is a bit quirky. An empty list is greater
           than a non-empty list, but a longer list is greater than a shorter list if all
           preceding tags are equal. *)
        match t1, t2 with
        | [], [] -> 0
        | [], _ :: _ -> 1
        | _ :: _, [] -> -1
        | _ :: _, _ :: _ -> [%compare: tag list] t1 t2
      ;;
    end

    type t =
      { major : int
      ; minor : int
      ; patch : int
      ; pre_release_tags : Pre_release_tags.t
      ; build_metadata : string list [@compare.ignore]
      }
    [@@deriving bin_io, compare, stable_witness]

    let to_string { major; minor; patch; pre_release_tags; build_metadata } =
      let version = Printf.sprintf "%d.%d.%d" major minor patch in
      let version =
        match pre_release_tags with
        | [] -> version
        | _ :: _ -> version ^ "-" ^ Core.String.concat ~sep:"." pre_release_tags
      in
      match build_metadata with
      | [] -> version
      | _ :: _ -> version ^ "+" ^ Core.String.concat ~sep:"." build_metadata
    ;;

    let regex =
      lazy
        (let open Re in
         let zero = char '0' in
         let dot = char '.' in
         let dash = char '-' in
         let plus = char '+' in
         let positive_digit = diff digit zero in
         let numeric_id = alt [ zero; seq [ positive_digit; rep digit ] ] in
         let non_digit = alt [ dash; inter [ alpha; ascii ] ] in
         let ident = alt [ digit; non_digit ] in
         let idents = rep1 ident in
         let alnum_id =
           (* An alphanumeric identifier must have a non-digit. Unlike a numeric
              identifier, it can have leading zeroes. *)
           seq [ rep digit; non_digit; rep ident ]
         in
         let major = group numeric_id in
         let minor = group numeric_id in
         let patch = group numeric_id in
         let dot_separated pattern = seq [ pattern; rep (seq [ dot; pattern ]) ] in
         let pre_release_tags = group (dot_separated (alt [ numeric_id; alnum_id ])) in
         let build_metadata = group (dot_separated idents) in
         seq
           [ bol
           ; major
           ; dot
           ; minor
           ; dot
           ; patch
           ; opt (seq [ dash; pre_release_tags ])
           ; opt (seq [ plus; build_metadata ])
           ; eol
           ]
         |> compile)
    ;;

    let of_string str =
      let get_int groups nth = Re.Group.get groups nth |> Core.Int.of_string in
      let get_opt_list groups nth =
        match Re.Group.test groups nth with
        | false -> []
        | true -> Re.Group.get groups nth |> Core.String.split ~on:'.'
      in
      let groups = Re.exec (Core.force regex) str in
      let major = get_int groups 1 in
      let minor = get_int groups 2 in
      let patch = get_int groups 3 in
      let pre_release_tags = get_opt_list groups 4 in
      let build_metadata = get_opt_list groups 5 in
      { major; minor; patch; pre_release_tags; build_metadata }
    ;;

    let sexp_of_t t = t |> to_string |> [%sexp_of: string]
    let t_of_sexp sexp = sexp |> [%of_sexp: string] |> of_string

    include (val Comparator.V1.make ~compare ~sexp_of_t)
  end

  module Latest = V1
end

open Core
include Semantic_version_intf

module Make () = struct
  module Stable = Stable
  include Stable.Latest
  include Comparable.Make_using_comparator (Stable.Latest)

  let initial_dev_release =
    { major = 0; minor = 1; patch = 0; pre_release_tags = []; build_metadata = [] }
  ;;

  let next_patch_release t =
    { t with patch = t.patch + 1; pre_release_tags = []; build_metadata = [] }
  ;;

  let next_minor_release t =
    { t with minor = t.minor + 1; patch = 0; pre_release_tags = []; build_metadata = [] }
  ;;

  let next_major_release t =
    { major = t.major + 1
    ; minor = 0
    ; patch = 0
    ; pre_release_tags = []
    ; build_metadata = []
    }
  ;;

  let arg_type = Command.Arg_type.create of_string
end
