#!/usr/bin/env nix
#! nix shell nixpkgs#erlang --command bash

erlc benchmark.erl
erl -noshell -s benchmark start 1000000 -s init stop
