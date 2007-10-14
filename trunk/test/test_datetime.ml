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

let string_of_unix_tm
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
    "epoch" >: TestCase
      (fun () ->
         let epoch = Unix.time () in
         assert_equal
           ~printer:string_of_float
           epoch
           (XmlRpcDateTime.to_epoch (XmlRpcDateTime.of_epoch epoch)));

    "epoch_gmt" >: TestCase
      (fun () ->
         let epoch = Unix.time () in
         assert_equal
           ~printer:string_of_float
           epoch
           (XmlRpcDateTime.to_epoch_gmt (XmlRpcDateTime.of_epoch_gmt epoch)));

    "unix" >: TestCase
      (fun () ->
         let time = Unix.localtime (Unix.time ()) in
         assert_equal
           ~printer:string_of_unix_tm
           time
           (XmlRpcDateTime.to_unix (XmlRpcDateTime.of_unix time)));

    "unix_gmt" >: TestCase
      (fun () ->
         let time = Unix.gmtime (Unix.time ()) in
         assert_equal
           ~printer:string_of_unix_tm
           time
           (XmlRpcDateTime.to_unix_gmt (XmlRpcDateTime.of_unix_gmt time)));

    "to_epoch" >: TestCase
      (fun () ->
         let dt_local = (2007, 10, 14, 7, 16, 18, -420) in
         let dt_gmt = (2007, 10, 14, 14, 16, 18, 0) in
         assert_equal
           ~printer:string_of_float
           (XmlRpcDateTime.to_epoch dt_local)
           (XmlRpcDateTime.to_epoch dt_gmt));

    "to_epoch_gmt" >: TestCase
      (fun () ->
         let dt_local = (2007, 10, 14, 7, 16, 18, -420) in
         let dt_gmt = (2007, 10, 14, 14, 16, 18, 0) in
         assert_equal
           ~printer:string_of_float
           (XmlRpcDateTime.to_epoch_gmt dt_local)
           (XmlRpcDateTime.to_epoch_gmt dt_gmt));

    "to_unix" >: TestCase
      (fun () ->
         let dt = (2007, 1, 1, 18, 34, 9, -420) in
         assert_equal
           ~printer:string_of_unix_tm
           (snd (Unix.mktime
                   {Unix.tm_year=107;
                    tm_mon=0;
                    tm_mday=1;
                    tm_hour=18;
                    tm_min=34;
                    tm_sec=9;
                    tm_yday=0;
                    tm_wday=0;
                    tm_isdst=false}))
           (XmlRpcDateTime.to_unix dt));

    "to_unix_gmt" >: TestCase
      (fun () ->
         let dt = (2007, 1, 1, 14, 34, 9, -420) in
         assert_equal
           ~printer:string_of_unix_tm
           (snd (Unix.mktime
                   {Unix.tm_year=107;
                    tm_mon=0;
                    tm_mday=1;
                    tm_hour=21;
                    tm_min=34;
                    tm_sec=9;
                    tm_yday=0;
                    tm_wday=0;
                    tm_isdst=false}))
           (XmlRpcDateTime.to_unix_gmt dt));

    "to_string" >: TestCase
      (fun () ->
         let dt = (2007, 1, 1, 14, 34, 9, 0) in
         assert_equal
           ~printer:(fun s -> s)
           "20070101T14:34:09Z"
           (XmlRpcDateTime.to_string dt));

    "to_string_tz" >: TestCase
      (fun () ->
         let dt = (2007, 1, 1, 14, 34, 9, -420) in
         assert_equal
           ~printer:(fun s -> s)
           "20070101T14:34:09-07:00"
           (XmlRpcDateTime.to_string dt));

    "of_string" >: TestCase
      (fun () ->
         let s = "20070101T14:34:09" in
         assert_equal
           ~printer:string_of_datetime
           (2007, 1, 1, 14, 34, 9, 0)
           (XmlRpcDateTime.of_string s));

    "of_string_z" >: TestCase
      (fun () ->
         let s = "20070101T14:34:09Z" in
         assert_equal
           ~printer:string_of_datetime
           (2007, 1, 1, 14, 34, 9, 0)
           (XmlRpcDateTime.of_string s));

    "to_string_tz" >: TestCase
      (fun () ->
         let s = "20070101T14:34:09-07:00" in
         assert_equal
           ~printer:string_of_datetime
           (2007, 1, 1, 14, 34, 9, -420)
           (XmlRpcDateTime.of_string s));

    "optional_delimiters" >: TestCase
      (fun () ->
         let s1 = "20071013T22:03:09-0700" in
         let s2 = "2007-10-13 22:03:09-07:00" in
         assert_equal
           ~printer:string_of_datetime
           (XmlRpcDateTime.of_string s1)
           (XmlRpcDateTime.of_string s2));
  ]

let tests = test :: tests
