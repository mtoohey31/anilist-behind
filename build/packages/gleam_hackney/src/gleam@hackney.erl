-module(gleam@hackney).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([send_bits/1, send/1]).
-export_type([error/0]).

-type error() :: invalid_utf8_response | {other, gleam@dynamic:dynamic_()}.

-spec normalise_header({binary(), binary()}) -> {binary(), binary()}.
normalise_header(Header) ->
    {gleam@string:lowercase(erlang:element(1, Header)),
        erlang:element(2, Header)}.

-spec send_bits(gleam@http@request:request(gleam@bytes_builder:bytes_builder())) -> {ok,
        gleam@http@response:response(bitstring())} |
    {error, error()}.
send_bits(Request) ->
    gleam@result:then(
        begin
            _pipe = Request,
            _pipe@1 = gleam@http@request:to_uri(_pipe),
            _pipe@2 = gleam@uri:to_string(_pipe@1),
            gleam_hackney_ffi:send(
                erlang:element(2, Request),
                _pipe@2,
                erlang:element(3, Request),
                erlang:element(4, Request)
            )
        end,
        fun(Response) ->
            Headers = gleam@list:map(
                erlang:element(3, Response),
                fun normalise_header/1
            ),
            {ok, erlang:setelement(3, Response, Headers)}
        end
    ).

-spec send(gleam@http@request:request(binary())) -> {ok,
        gleam@http@response:response(binary())} |
    {error, error()}.
send(Req) ->
    gleam@result:then(
        begin
            _pipe = Req,
            _pipe@1 = gleam@http@request:map(
                _pipe,
                fun gleam_stdlib:wrap_list/1
            ),
            send_bits(_pipe@1)
        end,
        fun(Resp) -> case gleam@bit_array:to_string(erlang:element(4, Resp)) of
                {ok, Body} ->
                    {ok, gleam@http@response:set_body(Resp, Body)};

                {error, _} ->
                    {error, invalid_utf8_response}
            end end
    ).
