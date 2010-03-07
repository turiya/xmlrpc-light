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

let string_of_unixtm
    {Unix.tm_sec=tm_sec; tm_min=tm_min; tm_hour=tm_hour;
     tm_mday=tm_mday; tm_mon=tm_mon; tm_year=tm_year;
     tm_wday=tm_wday; tm_yday=tm_yday; tm_isdst=tm_isdst} =
  sprintf "{Unix.tm_year=%d; tm_mon=%d; tm_mday=%d; tm_hour=%d; tm_min=%d; tm_sec=%d; tm_yday=%d; tm_wday=%d; tm_isdst=%b}"
    tm_year tm_mon tm_mday
    tm_hour tm_min tm_sec
    tm_yday tm_wday tm_isdst

let string_of_datetime (y, m, d, h, m', s, tz) =
  sprintf "(%d, %d, %d, %d, %d, %d, %d)"
    y m d h m' s tz

let test = "test_datetime" >:::
  [
    "unixfloat" >::
      (fun () ->
         let unixfloat = Unix.time () in
         assert_equal
           ~printer:string_of_float
           unixfloat
           (XmlRpcDateTime.to_unixfloat (XmlRpcDateTime.from_unixfloat unixfloat)));

    "unixfloat_utc" >::
      (fun () ->
         let unixfloat = Unix.time () in
         assert_equal
           ~printer:string_of_float
           unixfloat
           (XmlRpcDateTime.to_unixfloat_utc (XmlRpcDateTime.from_unixfloat_utc unixfloat)));

    "unix" >::
      (fun () ->
         let time = Unix.localtime (Unix.time ()) in
         assert_equal
           ~printer:string_of_unixtm
           time
           (XmlRpcDateTime.to_unixtm (XmlRpcDateTime.from_unixtm time)));

    "unix_utc" >::
      (fun () ->
         let time = Unix.gmtime (Unix.time ()) in
         assert_equal
           ~printer:string_of_unixtm
           time
           (XmlRpcDateTime.to_unixtm_utc (XmlRpcDateTime.from_unixtm_utc time)));

    "to_unixfloat" >::
      (fun () ->
         let dt_local = (2007, 10, 14, 7, 16, 18, -420) in
         let dt_utc = (2007, 10, 14, 14, 16, 18, 0) in
         assert_equal
           ~printer:string_of_float
           (XmlRpcDateTime.to_unixfloat dt_local)
           (XmlRpcDateTime.to_unixfloat dt_utc));

    "to_unixfloat_utc" >::
      (fun () ->
         let dt_local = (2007, 10, 14, 7, 16, 18, -420) in
         let dt_utc = (2007, 10, 14, 14, 16, 18, 0) in
         assert_equal
           ~printer:string_of_float
           (XmlRpcDateTime.to_unixfloat_utc dt_local)
           (XmlRpcDateTime.to_unixfloat_utc dt_utc));

    "to_unixtm" >::
      (fun () ->
         let dt = (2007, 1, 1, 14, 34, 9, Netdate.localzone) in
         assert_equal
           ~printer:string_of_unixtm
           {Unix.tm_year=107;
            tm_mon=0;
            tm_mday=1;
            tm_hour=14;
            tm_min=34;
            tm_sec=9;
            tm_yday=0;
            tm_wday=1;
            tm_isdst=false}
           (XmlRpcDateTime.to_unixtm dt));

    "to_unixtm_utc" >::
      (fun () ->
         let dt = (2007, 1, 1, 14, 34, 9, -420) in
         assert_equal
           ~printer:string_of_unixtm
           {Unix.tm_year=107;
            tm_mon=0;
            tm_mday=1;
            tm_hour=21;
            tm_min=34;
            tm_sec=9;
            tm_yday=0;
            tm_wday=1;
            tm_isdst=false}
           (XmlRpcDateTime.to_unixtm_utc dt));

    "daylight_savings" >::
      (fun () ->
         let dt1 = (2007, 3, 11, 2, 57, 38, 0) in
         let dt2 = (2007, 3, 11, 3, 57, 38, 0) in
         assert_equal
           ~printer:string_of_float
           (1173581858.)
           (XmlRpcDateTime.to_unixfloat_utc dt1);
         assert_equal
           ~printer:string_of_float
           (XmlRpcDateTime.to_unixfloat_utc dt1 +. 3600.)
           (XmlRpcDateTime.to_unixfloat_utc dt2));

    "to_string" >::
      (fun () ->
         let dt = (2007, 1, 1, 14, 34, 9, 0) in
         assert_equal
           ~printer:(fun s -> s)
           "20070101T14:34:09"
           (XmlRpcDateTime.to_string dt));

    "to_string_tz" >::
      (fun () ->
         let dt = (2007, 1, 1, 14, 34, 9, -420) in
         assert_equal
           ~printer:(fun s -> s)
           "20070101T14:34:09-07:00"
           (XmlRpcDateTime.to_string dt));

    "of_string" >::
      (fun () ->
         let s = "20070101T14:34:09" in
         assert_equal
           ~printer:string_of_datetime
           (2007, 1, 1, 14, 34, 9, 0)
           (XmlRpcDateTime.of_string s));

    "of_string_z" >::
      (fun () ->
         let s = "20070101T14:34:09Z" in
         assert_equal
           ~printer:string_of_datetime
           (2007, 1, 1, 14, 34, 9, 0)
           (XmlRpcDateTime.of_string s));

    "to_string_tz" >::
      (fun () ->
         let s = "20070101T14:34:09-07:00" in
         assert_equal
           ~printer:string_of_datetime
           (2007, 1, 1, 14, 34, 9, -420)
           (XmlRpcDateTime.of_string s));

    "optional_delimiters" >::
      (fun () ->
         let s1 = "20071013T22:03:09-0700" in
         let s2 = "2007-10-13 22:03:09-07:00" in
         assert_equal
           ~printer:string_of_datetime
           (XmlRpcDateTime.of_string s1)
           (XmlRpcDateTime.of_string s2));

    "set_tz_offset" >::
      (fun () ->
         let offset = 360 in
         let dt1 = XmlRpcDateTime.now () in
         let dt2 = XmlRpcDateTime.set_tz_offset offset dt1 in
         assert_equal
           ~printer:string_of_int
           offset
           (match dt2 with (_, _, _, _, _, _, tz) -> tz);
         assert_equal
           ~printer:string_of_datetime
           ~cmp:XmlRpcDateTime.equal
           dt1 dt2);

    "fix_tz_offset" >::
      (fun () ->
         let offset = -420 in
         let dt1 = XmlRpcDateTime.now_utc () in
         let dt2 = XmlRpcDateTime.fix_tz_offset offset dt1 in
         assert_equal
           ~printer:string_of_int
           offset
           (match dt2 with (_, _, _, _, _, _, tz) -> tz);
         assert_equal
           ~printer:string_of_int
           (int_of_float (XmlRpcDateTime.to_unixfloat dt1))
           (int_of_float (XmlRpcDateTime.to_unixfloat dt2)
            + offset * 60));
  ]

let tests = test :: tests
