(*
 * XmlRpcBase64 - Base64 codec for XmlRpc
 * Copyright (C) 2007-2010 Dave Benjamin
 * Copyright (C) 2003 Nicolas Cannasse
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * This module is based on the ExtLib Base64 codec, written by
 * Nicolas Cannasse, with the following modifications to enable
 * compatibility with various XmlRpc implementations:
 *
 *  - For encoding, output is padded with '=' to a multiple of four characters.
 *  - For decoding, the input characters '\r', '\n', and '=' are ignored.
 *
 * To avoid a dependency on ExtLib, this version uses OCaml's built-in Stream
 * type instead of ExtLib's IO type.
 *
 * Original copyright:
 *
 * Base64 - Base64 codec
 * Copyright (C) 2003 Nicolas Cannasse
 *)

(** Base64 codec.

	8-bit characters are encoded into 6-bit ones using ASCII lookup tables.
	Default tables maps 0..63 values on characters A-Z, a-z, 0-9, '+' and '/'
	(in that order).
*)

(** This exception is raised when reading an invalid character
	from a base64 input. *)
exception Invalid_char

(** This exception is raised if the encoding or decoding table
	size is not correct. *)
exception Invalid_table

(** An encoding table maps integers 0..63 to the corresponding char. *)
type encoding_table = char array

(** A decoding table maps chars 0..255 to the corresponding 0..63 value
 or -1 if the char is not accepted. *)
type decoding_table = int array

(** Encode a string into Base64. *)
val str_encode : ?tbl:encoding_table -> string -> string

(** Decode a string encoded into Base64, raise [Invalid_char] if a
	character in the input string is not a valid one. *)
val str_decode : ?tbl:decoding_table -> string -> string

(** Generic base64 encoding over a character stream. *)
val encode : ?tbl:encoding_table -> char Stream.t -> char Stream.t

(** Generic base64 decoding over a character stream. *)
val decode : ?tbl:decoding_table -> char Stream.t -> char Stream.t

(** Create a valid decoding table from an encoding one. *)
val make_decoding_table : encoding_table -> decoding_table
