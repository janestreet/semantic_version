open Core

module type S = sig
  type t = private
    { major : int
    ; minor : int
    ; patch : int
    ; pre_release_tags : string list
    ; build_metadata : string list
    }
  [@@deriving sexp]

  include Comparable.S with type t := t
  include Stringable.S with type t := t

  val initial_dev_release : t
  val next_patch_release : t -> t
  val next_minor_release : t -> t
  val next_major_release : t -> t
  val arg_type : t Command.Arg_type.t

  (* [Stable] versions below are unrelated to and should not be confused with the version
     of the Semantic Versioning specification, which as of this writing is 2.0.0. *)
  module Stable : sig
    module V1 :
      Stable_with_witness with type t = t and type comparator_witness = comparator_witness
  end
end

(** The rules for Semantic Versioning are described at semver.org. *)
module type Semantic_version = sig
  module type S = S

  module Make () : S
end
