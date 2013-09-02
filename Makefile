.PHONY: all clean compile test

all: compile

clean:
	rebar clean

compile: clean
	rebar compile

test: compile
	rm -rf .eunit
	rebar eunit