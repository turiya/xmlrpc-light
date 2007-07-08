(*
 * XmlRpcBase64 - Base64 codec for XmlRpc
 * Copyright (C) 2007 Dave Benjamin
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

exception Invalid_char
exception Invalid_table

external unsafe_char_of_int : int -> char = "%identity"

type encoding_table = char array
type decoding_table = int array

let chars = [|
              'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
              'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
              'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
              'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/'
            |]

let make_decoding_table tbl =
  if Array.length tbl <> 64 then raise Invalid_table;
  let d = Array.make 256 (-1) in
  for i = 0 to 63 do
    Array.unsafe_set d (int_of_char (Array.unsafe_get tbl i)) i;
  done;
  d

let inv_chars = make_decoding_table chars

let encode ?(tbl=chars) strm =
  if Array.length tbl <> 64 then raise Invalid_table;
  let data = ref 0 in
  let count = ref 0 in
  let pad = ref None in
  let rec next i =
    if !count >= 6
    then
      begin
        count := !count - 6;
        let d = (!data asr !count) land 63 in
        Some (Array.unsafe_get tbl d)
      end
    else
      begin
        try
          let c = int_of_char (Stream.next strm) in
          data := (!data lsl 8) lor c;
          count := !count + 2;
          let d = (!data asr !count) land 63 in
          Some (Array.unsafe_get tbl d)
        with Stream.Failure ->
          match !pad with
            | Some 0 ->
                None
            | Some n ->
                pad := Some (n - 1);
                Some '='
            | None ->
                if !count > 0
                then (pad := Some (3 - (i mod 4));
                      let d = (!data lsl (6 - !count)) land 63 in
                      Some (Array.unsafe_get tbl d))
                else None
      end in
  Stream.from next

let decode ?(tbl=inv_chars) strm =
  if Array.length tbl <> 256 then raise Invalid_table;
  let data = ref 0 in
  let count = ref 0 in
  let rec next i =
    if !count >= 8
    then
      begin
        count := !count - 8;
        let d = (!data asr !count) land 0xFF in
        Some (unsafe_char_of_int d)
      end
    else
      begin
        try
          match Stream.next strm with
            | '\r' | '\n' | '=' ->
                next i
            | c ->
                let c = int_of_char c in
                let c = Array.unsafe_get tbl c in
                if c = -1 then raise Invalid_char;
                data := (!data lsl 6) lor c;
                count := !count + 6;
                next i
        with Stream.Failure ->
          None
      end in
  Stream.from next

let string_of_stream stream =
  let buffer = Buffer.create 16 in
  Stream.iter (Buffer.add_char buffer) stream;
  Buffer.contents buffer

let str_encode ?(tbl=chars) s =
  string_of_stream (encode ~tbl (Stream.of_string s))

let str_decode ?(tbl=inv_chars) s =
  string_of_stream (decode ~tbl (Stream.of_string s))
