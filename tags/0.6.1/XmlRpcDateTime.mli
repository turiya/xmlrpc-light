(*
 * XmlRpc Light, a small XmlRpc library based on Xml Light and Ocamlnet
 * Copyright (C) 2007-2009 Dave Benjamin (dave@ramenlabs.com)
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

(** Date/time type. *)

(** {2 Types} *)

(** Raised by {!of_string} if a string could not be parsed. The exception
    contains the input string. *)
exception Parse_error of string

(** Type of XmlRpc-compatible date/time values.
    (year, month, day, hour, minute, second, time zone offset in minutes) *)
type t = (int * int * int * int * int * int * int)

(** {2 Comparison} *)

(** Standard comparator for date/time values. Converts all values to UTC
    before comparing to ensure correct behavior with values of differing
    time zones. *)
val compare : t -> t -> int

(** Standard equality function for date/time values. Converts all values
    to UTC before comparing. *)
val equal : t -> t -> bool

(** Standard hash function for date/time values. Converts values to UTC
    before hashing. *)
val hash : t -> int

(** {2 Current date and time} *)

(** Returns the current date and time in the local time zone. *)
val now : unit -> t

(** Returns the current date and time in UTC. *)
val now_utc : unit -> t

(** {2 Time zone adjustments} *)

(** Adjusts the time zone offset, preserving equality. *) 
val set_tz_offset : int -> t -> t

(** Forces the time zone offset to a different value, ignoring all other
    fields. Use this to correct the time zone of a date/time value that
    was received without a time zone offset and is known not to be UTC. *)
val fix_tz_offset : int -> t -> t

(** {2 Conversion} *)

(** Builds a date/time value from epoch seconds in the local time zone. *)
val from_unixfloat : float -> t

(** Builds a date/time value from epoch seconds in UTC. *)
val from_unixfloat_utc : float -> t

(** Converts a date/time value to epoch seconds in the local time zone. *)
val to_unixfloat : t -> float

(** Converts a date/time value to epoch seconds in UTC. *)
val to_unixfloat_utc : t -> float

(** Builds a date/time value from a Unix.tm value in the local time zone. *)
val from_unixtm : Unix.tm -> t

(** Builds a date/time value from a Unix.tm value in UTC. *)
val from_unixtm_utc : Unix.tm -> t

(** Converts a date/time value to a Unix.tm value in the local time zone. *)
val to_unixtm : t -> Unix.tm

(** Converts a date/time value to a Unix.tm value in UTC. *)
val to_unixtm_utc : t -> Unix.tm

(** {2 ISO-8601 parsing and generation} *)

(** Parses an (XmlRpc-flavor) ISO-8601 date/time value from a string. *)
val of_string : string -> t

(** Generates an ISO-8601 string from a date/time value. *)
val to_string : t -> string
