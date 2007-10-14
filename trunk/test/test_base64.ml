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

let test = "test_base64" >:::
  [
    "encode" >::
      (fun () ->
         assert_equal
           ~printer:(fun s -> s)
           "UnVsaW5nIHRoZSBjb3VudHJ5IGlzIGxpa2UgY29va2luZyBhIHNtYWxsIGZpc2guCkFwcHJvYWNoIHRoZSB1bml2ZXJzZSB3aXRoIFRhbywKQW5kIGV2aWwgaXMgbm90IHBvd2VyZnVsLApCdXQgaXRzIHBvd2VyIHdpbGwgbm90IGJlIHVzZWQgdG8gaGFybSBvdGhlcnMuCk5vdCBvbmx5IHdpbGwgaXQgZG8gbm8gaGFybSB0byBvdGhlcnMsCkJ1dCB0aGUgc2FnZSBoaW1zZWxmIHdpbGwgYWxzbyBiZSBwcm90ZWN0ZWQuClRoZXkgZG8gbm90IGh1cnQgZWFjaCBvdGhlciwKQW5kIHRoZSBWaXJ0dWUgaW4gZWFjaCBvbmUgcmVmcmVzaGVzIGJvdGguCg=="
           (XmlRpcBase64.str_encode "\
Ruling the country is like cooking a small fish.
Approach the universe with Tao,
And evil is not powerful,
But its power will not be used to harm others.
Not only will it do no harm to others,
But the sage himself will also be protected.
They do not hurt each other,
And the Virtue in each one refreshes both.
"));

    "decode" >::
      (fun () ->
         assert_equal
           ~printer:(fun s -> s)
           "When someone says \"I want a programming language in which I need only\nsay what I wish done,\" give him a lollipop.\n"
           (XmlRpcBase64.str_decode "V2hlbiBzb21lb25lIHNheXMgIkkgd2FudCBhIHByb2dyYW1taW5nIGxhbmd1YWdlIGluIHdoaWNo\nIEkgbmVlZCBvbmx5CnNheSB3aGF0IEkgd2lzaCBkb25lLCIgZ2l2ZSBoaW0gYSBsb2xsaXBvcC4K\n"));
  ]

let tests = test :: tests
