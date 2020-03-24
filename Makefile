REBAR=./rebar3

all: compile

$(REBAR):
	wget https://s3.amazonaws.com/rebar3/rebar3 -O $(REBAR)
	chmod +x $(REBAR)

compile: $(REBAR)
	@$(REBAR) compile
