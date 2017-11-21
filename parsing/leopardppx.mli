open Parsetree
val structure : structure -> structure
val signature : signature -> signature

module Imp : sig
  val from_payload_to_core_type_forward : (Location.t -> payload -> core_type) ref
end
