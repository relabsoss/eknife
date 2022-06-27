REBAR=./rebar3

all: compile

$(REBAR):
	wget https://s3.amazonaws.com/rebar3/rebar3 -O $(REBAR)
	chmod +x $(REBAR)

compile: $(REBAR)
	$(REBAR) compile

xref: $(REBAR)
	$(REBAR) xref

commit: $(REBAR)
	ERL_AFLAGS="-enable-feature all" $(REBAR) format
	git commit .
