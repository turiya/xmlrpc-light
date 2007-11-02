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

exception Parse_error of string

type t = (int * int * int * int * int * int * int)

let local_tz_offset () =
  let time = Unix.time () in
  let utc = fst (Unix.mktime (Unix.gmtime time)) in
  int_of_float (time -. utc) / 60

let from_unixtm tm =
  (tm.Unix.tm_year + 1900,
   tm.Unix.tm_mon + 1,
   tm.Unix.tm_mday,
   tm.Unix.tm_hour,
   tm.Unix.tm_min,
   tm.Unix.tm_sec,
   local_tz_offset ())

let from_unixtm_utc tm =
  (tm.Unix.tm_year + 1900,
   tm.Unix.tm_mon + 1,
   tm.Unix.tm_mday,
   tm.Unix.tm_hour,
   tm.Unix.tm_min,
   tm.Unix.tm_sec,
   0)

let from_unixfloat time = from_unixtm (Unix.localtime time)
let from_unixfloat_utc time = from_unixtm_utc (Unix.localtime time)

let to_unixfloat_utc (y, m, d, h, m', s, tz) =
  fst (Unix.mktime {Unix.tm_year=y - 1900;
                    tm_mon=m - 1;
                    tm_mday=d;
                    tm_hour=h;
                    tm_min=m';
                    tm_sec=s;
                    tm_wday=0;
                    tm_yday=0;
                    tm_isdst=false}) -. (float tz *. 60.0)

let to_unixfloat dt =
  to_unixfloat_utc dt +. (float (local_tz_offset ()) *. 60.0)

let to_unixtm dt = Unix.localtime (to_unixfloat dt)
let to_unixtm_utc dt = Unix.localtime (to_unixfloat_utc dt)

let now () = from_unixfloat (Unix.time ())
let now_utc () =
  from_unixfloat_utc (Unix.time () -. (float (local_tz_offset ()) *. 60.0))

let set_tz_offset offset dt =
  let time = to_unixfloat_utc dt +. (float offset *. 60.0) in
  match from_unixfloat_utc time
  with (y, m, d, h, m', s, _) -> (y, m, d, h, m', s, offset)

let fix_tz_offset offset dt =
  match dt with (y, m, d, h, m', s, _) -> (y, m, d, h, m', s, offset)

let compare a b = compare (to_unixfloat_utc a) (to_unixfloat_utc b)
let equal a b = (to_unixfloat_utc a) = (to_unixfloat_utc b)
let hash a = Hashtbl.hash (to_unixfloat_utc a)

let string_of_tz_offset offset =
  if offset = 0 then "Z" else
    Printf.sprintf "%c%02d:%02d"
      (if offset >= 0 then '+' else '-')
      (abs (offset / 60))
      (abs (offset mod 60))

let tz_offset_of_string = function
  | "" | "Z" -> 0
  | string ->
      Scanf.sscanf string "%c%02d%_[:]%02d"
        (fun sign hour min ->
           min + hour * (if sign = '-' then -60 else 60))

let to_string (y, m, d, h, m', s, tz_offset) =
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02d%s"
    y m d h m' s (string_of_tz_offset tz_offset)

let of_string string =
  try
    Scanf.sscanf string "%04d%_[-]%02d%_[-]%02d%_[T ]%02d:%02d:%02d%s"
      (fun y m d h m' s tz ->
         (y, m, d, h, m', s, (tz_offset_of_string tz)))
  with
    | Scanf.Scan_failure _
    | End_of_file ->
        raise (Parse_error string)
