-module(gleamql).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([new/0, set_query/2, set_variable/3, set_host/2, set_path/2, set_header/3, send/2, set_decoder/2]).
-export_type([request/1, graph_ql_error/0, gql_success/1, gql_errors/0, gql_error/0]).

-type request(ILJ) :: {request,
        gleam@http@request:request(binary()),
        gleam@option:option(binary()),
        gleam@option:option(list({binary(), gleam@json:json()})),
        gleam@option:option(fun((gleam@dynamic:dynamic_()) -> {ok, ILJ} |
            {error, list(gleam@dynamic:decode_error())}))}.

-type graph_ql_error() :: {error_message, binary()} |
    {unexpected_status, integer()} |
    {unrecognised_response, binary()} |
    {unknown_error, gleam@dynamic:dynamic_()}.

-type gql_success(ILK) :: {success_response, ILK}.

-type gql_errors() :: {error_response, list(gql_error())}.

-type gql_error() :: {gql_error, binary()}.

-spec new() -> request(any()).
new() ->
    {request,
        begin
            _pipe = gleam@http@request:new(),
            gleam@http@request:set_method(_pipe, post)
        end,
        none,
        none,
        none}.

-spec set_query(request(ILN), binary()) -> request(ILN).
set_query(Req, Query) ->
    erlang:setelement(3, Req, {some, Query}).

-spec set_variable(request(ILQ), binary(), gleam@json:json()) -> request(ILQ).
set_variable(Req, Key, Value) ->
    Variables = [{Key, Value} |
        begin
            _pipe = erlang:element(4, Req),
            gleam@option:unwrap(_pipe, gleam@list:new())
        end],
    erlang:setelement(4, Req, {some, Variables}).

-spec set_host(request(IMD), binary()) -> request(IMD).
set_host(Req, Host) ->
    erlang:setelement(
        2,
        Req,
        begin
            _pipe = erlang:element(2, Req),
            gleam@http@request:set_host(_pipe, Host)
        end
    ).

-spec set_path(request(IMG), binary()) -> request(IMG).
set_path(Req, Path) ->
    erlang:setelement(
        2,
        Req,
        begin
            _pipe = erlang:element(2, Req),
            gleam@http@request:set_path(_pipe, Path)
        end
    ).

-spec set_header(request(IMJ), binary(), binary()) -> request(IMJ).
set_header(Req, Key, Value) ->
    erlang:setelement(
        2,
        Req,
        begin
            _pipe = erlang:element(2, Req),
            gleam@http@request:set_header(_pipe, Key, Value)
        end
    ).

-spec status_is_ok(integer()) -> boolean().
status_is_ok(Status) ->
    Status =:= 200.

-spec send(
    request(ILT),
    fun((gleam@http@request:request(binary())) -> {ok,
            gleam@http@response:response(binary())} |
        {error, any()})
) -> {ok, gleam@option:option(ILT)} | {error, graph_ql_error()}.
send(Req, Send) ->
    Request = begin
        _pipe = erlang:element(2, Req),
        gleam@http@request:set_body(
            _pipe,
            begin
                _pipe@4 = gleam@json:object(
                    [{<<"query"/utf8>>,
                            begin
                                _pipe@1 = erlang:element(3, Req),
                                _pipe@2 = gleam@option:unwrap(
                                    _pipe@1,
                                    <<""/utf8>>
                                ),
                                gleam@json:string(_pipe@2)
                            end},
                        {<<"variables"/utf8>>,
                            gleam@json:object(
                                begin
                                    _pipe@3 = erlang:element(4, Req),
                                    gleam@option:unwrap(
                                        _pipe@3,
                                        gleam@list:new()
                                    )
                                end
                            )}]
                ),
                gleam@json:to_string(_pipe@4)
            end
        )
    end,
    gleam@result:then(
        begin
            _pipe@5 = Request,
            _pipe@6 = Send(_pipe@5),
            gleam@result:map_error(
                _pipe@6,
                fun(E) -> {unknown_error, gleam@dynamic:from(E)} end
            )
        end,
        fun(Resp) ->
            Errors_decoder = gleam@dynamic:decode1(
                fun(Field@0) -> {error_response, Field@0} end,
                gleam@dynamic:field(
                    <<"errors"/utf8>>,
                    gleam@dynamic:list(
                        gleam@dynamic:decode1(
                            fun(Field@0) -> {gql_error, Field@0} end,
                            gleam@dynamic:field(
                                <<"message"/utf8>>,
                                fun gleam@dynamic:string/1
                            )
                        )
                    )
                )
            ),
            case begin
                _pipe@7 = erlang:element(2, Resp),
                status_is_ok(_pipe@7)
            end of
                true ->
                    case erlang:element(5, Req) of
                        {some, Decoder} ->
                            case gleam@json:decode(
                                erlang:element(4, Resp),
                                gleam@dynamic:decode1(
                                    fun(Field@0) -> {success_response, Field@0} end,
                                    gleam@dynamic:field(
                                        <<"data"/utf8>>,
                                        Decoder
                                    )
                                )
                            ) of
                                {ok, Response} ->
                                    {ok, {some, erlang:element(2, Response)}};

                                {error, _} ->
                                    {error,
                                        {unrecognised_response,
                                            erlang:element(4, Resp)}}
                            end;

                        none ->
                            {ok, none}
                    end;

                false ->
                    case gleam@json:decode(
                        erlang:element(4, Resp),
                        Errors_decoder
                    ) of
                        {ok, Response@1} ->
                            case begin
                                _pipe@8 = erlang:element(2, Response@1),
                                gleam@list:first(_pipe@8)
                            end of
                                {ok, Error} ->
                                    {error,
                                        {error_message,
                                            erlang:element(2, Error)}};

                                {error, _} ->
                                    {error,
                                        {unrecognised_response,
                                            erlang:element(4, Resp)}}
                            end;

                        {error, _} ->
                            {error,
                                {unexpected_status, erlang:element(2, Resp)}}
                    end
            end
        end
    ).

-spec set_decoder(
    request(IMM),
    fun((gleam@dynamic:dynamic_()) -> {ok, IMM} |
        {error, list(gleam@dynamic:decode_error())})
) -> request(IMM).
set_decoder(Req, Decoder) ->
    erlang:setelement(5, Req, {some, Decoder}).
