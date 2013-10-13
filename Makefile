.PHONY: all clean compile test

all: compile

clean:
	rm erl_crash.dump -f
	rebar clean

compile: clean
	rebar compile

test: compile
	rm -rf .eunit
	rebar eunit
