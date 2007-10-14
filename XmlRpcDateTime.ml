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

let local_tz_offset () =
  let time = Unix.time () in
  let utc = fst (Unix.mktime (Unix.gmtime time)) in
  int_of_float (time -. utc) / 60

let of_unix tm =
  (tm.Unix.tm_year + 1900,
   tm.Unix.tm_mon + 1,
   tm.Unix.tm_mday,
   tm.Unix.tm_hour,
   tm.Unix.tm_min,
   tm.Unix.tm_sec,
   local_tz_offset ())

let of_unix_utc tm =
  (tm.Unix.tm_year + 1900,
   tm.Unix.tm_mon + 1,
   tm.Unix.tm_mday,
   tm.Unix.tm_hour,
   tm.Unix.tm_min,
   tm.Unix.tm_sec,
   0)

let of_epoch time = of_unix (Unix.localtime time)
let of_epoch_utc time = of_unix_utc (Unix.localtime time)

let to_epoch_utc (y, m, d, h, m', s, tz) =
  fst (Unix.mktime {Unix.tm_year=y - 1900;
                    tm_mon=m - 1;
                    tm_mday=d;
                    tm_hour=h;
                    tm_min=m';
                    tm_sec=s;
                    tm_wday=0;
                    tm_yday=0;
                    tm_isdst=false}) -. (float tz *. 60.0)

let to_epoch dt = to_epoch_utc dt +. (float (local_tz_offset ()) *. 60.0)

let to_unix dt = Unix.localtime (to_epoch dt)
let to_unix_utc dt = Unix.localtime (to_epoch_utc dt)

let now () = of_epoch (Unix.time ())
let now_utc () =
  of_epoch_utc (Unix.time () -. (float (local_tz_offset ()) *. 60.0))

let compare a b = compare (to_epoch_utc a) (to_epoch_utc b)
let equal a b = (to_epoch_utc a) = (to_epoch_utc b)
let hash a = Hashtbl.hash (to_epoch_utc a)

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
        failwith "XmlRpcDateTime.of_string"
