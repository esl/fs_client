fs_client
=========

Introduction
------------

TCP based Erlang client for communicating with an Erlang term aware Julia server. If you wish to offload computations from Erlang to a backend service in any other language, this is one way to do it. This project is intended to work with FsServer written in Julia which depends on FsBert.

Usage
-----

To build the client, just type make.

`$ make
$ erl -pa ebin/`

`> fs_client_test:all().`

Examples
--------

The fs_client_test.erl file demonstrates how one may call a series of functions in Julia from a provided module called FsExtern. In FsServer.jl, this module has been temporarily excluded (it is meant as a placeholder for user defined modules that do something useful in Julia). This Erlang module can be used to test that the calls to user provided functions work appropriately end to end.




