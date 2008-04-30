XmlRpc Light, a small XmlRpc client based on Xml Light and Ocamlnet
===================================================================
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,,
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

What is XmlRpc Light?
---------------------

XmlRpc Light is an XmlRpc client written in OCaml. It requires Xml-Light and
Ocamlnet. This code should be considered Alpha software, and the interface may
change to better fit the needs of the community.

Installation:
-------------

For Debian users:

    sudo apt-get install libxml-light-ocaml-dev libnethttpd-ocaml-dev

    make
    sudo make install

For other environments, install xml-light and ocamlnet from sources available
at the following locations:

    Xml-Light: http://tech.motion-twin.com/xmllight.html
    Ocamlnet: http://sourceforge.net/projects/ocamlnet

And then run, as usual:

    make
    sudo make install

To uninstall XmlRpc-Light, which you will also need to do if you have
already installed a previous version, type:

    sudo make uninstall

Usage:
------

Simple example:

    let rpc = new XmlRpc.client "http://localhost:8000" in
    let result = rpc#call "echo" [`String "hello!"] in
    print_endline (XmlRpc.dump result) ]}

See the Ocamldoc-generated documentation in the "doc" directory for more
details.

Contributors:
-------------

Dave Benjamin (dave@ramenlabs.com)

License:
--------

See LICENSE
