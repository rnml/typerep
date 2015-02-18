open Core.Std

val to_sexp : 'a Type.Rep.t -> 'a -> Sexp.t
val of_sexp : 'a Type.Rep.t -> Sexp.t -> 'a

val register_to_sexp : 'a Type.Name.t -> ('a -> Sexp.t) -> unit
val register_of_sexp : 'a Type.Name.t -> (Sexp.t -> 'a) -> unit
