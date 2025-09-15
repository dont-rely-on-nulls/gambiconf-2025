#!/usr/bin/env bash

nix shell nixpkgs#erlang --command sh -c "erlc benchmark.erl && erl -noshell -s benchmark start 1000000 -s init stop"
