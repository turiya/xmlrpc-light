(*
 * XmlRpc Light, a small XmlRpc library based on Xml Light and Ocamlnet
 * Copyright (C) 2007 Dave Benjamin (dave@ramenlabs.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type t = (int * int * int * int * int * int * int)

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val now : unit -> t
val now_utc : unit -> t

val from_unixfloat : float -> t
val from_unixfloat_utc : float -> t
val to_unixfloat : t -> float
val to_unixfloat_utc : t -> float

val from_unixtm : Unix.tm -> t
val from_unixtm_utc : Unix.tm -> t
val to_unixtm : t -> Unix.tm
val to_unixtm_utc : t -> Unix.tm

val of_string : string -> t
val to_string : t -> string