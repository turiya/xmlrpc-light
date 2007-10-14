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
 *
 * Local time zone offset calculation algorithm based on 822-date, a
 * public-domain Perl script by Ian Jackson (1995) and Klee Dienes (1997).
 *)

type t = (int * int * int * int * int * int * int)

let local_tz_offset () =
  let curtime = Unix.time () in
  let localtm = Unix.localtime curtime in
  let gmttm   = Unix.gmtime curtime in

  if localtm.Unix.tm_sec <> gmttm.Unix.tm_sec
  then failwith "local timezone differs by GMT by a non-minute interval";

  let localmin = ref (localtm.Unix.tm_min + localtm.Unix.tm_hour * 60) in
  let gmtmin   = ref (gmttm.Unix.tm_min + gmttm.Unix.tm_hour * 60) in

  if (gmttm.Unix.tm_wday + 7 + 1) mod 7 = localtm.Unix.tm_wday
  then localmin := !localmin + 1440
  else if (gmttm.Unix.tm_wday + 7 - 1) mod 7 = localtm.Unix.tm_wday
  then localmin := !localmin - 1440
  else if gmttm.Unix.tm_wday <> localtm.Unix.tm_wday
  then failwith "local time offset greater than or equal to 24 hours";

  !localmin - !gmtmin

let of_unix tm =
  (tm.Unix.tm_year + 1900,
   tm.Unix.tm_mon + 1,
   tm.Unix.tm_mday,
   tm.Unix.tm_hour,
   tm.Unix.tm_min,
   tm.Unix.tm_sec,
   local_tz_offset ())

let of_unix_gmt tm =
  (tm.Unix.tm_year + 1900,
   tm.Unix.tm_mon + 1,
   tm.Unix.tm_mday,
   tm.Unix.tm_hour,
   tm.Unix.tm_min,
   tm.Unix.tm_sec,
   0)

let of_epoch time = of_unix (Unix.localtime time)
let of_epoch_gmt time = of_unix_gmt (Unix.gmtime time)

let to_epoch_gmt (y, m, d, h, m', s, tz) =
  fst (Unix.mktime {Unix.tm_year=y - 1900;
                    tm_mon=m - 1;
                    tm_mday=d;
                    tm_hour=h;
                    tm_min=m';
                    tm_sec=s;
                    tm_wday=0;
                    tm_yday=0;
                    tm_isdst=false}) -. (float tz *. 60.0)

let to_epoch dt = to_epoch_gmt dt +. (float (local_tz_offset()) *. 60.0)

let to_unix dt = Unix.localtime (to_epoch dt)
let to_unix_gmt dt = Unix.localtime (to_epoch_gmt dt)

let now () = of_epoch (Unix.time ())
let now_gmt () = of_epoch_gmt (Unix.time ())

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
