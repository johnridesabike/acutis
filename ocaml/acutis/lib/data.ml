(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Const = struct
  type t = PInt of int | PString of string | PFloat of float
  [@@deriving eq, ord, show]

  let of_tpat = function
    | Typechecker.Pattern.TString x -> PString x
    | TFloat x -> PFloat x
    | TInt x -> PInt x

  let to_tpat = function
    | PString x -> Typechecker.Pattern.TString x
    | PFloat x -> TFloat x
    | PInt x -> TInt x
end
