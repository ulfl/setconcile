Config
======

Simple application for holding a system wide accessible read-only
configuration. The configuration is read from a file at system
startup. The config file is on the form of an Erlang R17
dictionary. Expressions are evaluated at load time. Example:

    Timeout = 60 * 60,

    #{
      %% Name of the node.
      node_name             => "a",

      %% Some timeout.
      timeout               => Timeout,
     }.

Config: Copyright (c) 2015 Ulf Leopold.
