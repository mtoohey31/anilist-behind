-module(glint).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([with_config/2, with_pretty_help/2, without_pretty_help/1, with_name/2, as_gleam_module/1, command/1, description/2, unnamed_args/2, named_args/2, flag/3, flag_tuple/2, flags/2, group_flags/3, global_flags/2, group_flag/4, global_flag/3, global_flag_tuple/2, group_flag_tuple/3, default_pretty_help/0, add/3, new/0, help_flag/0, execute/2, run_and_handle/3, run/2]).
-export_type([config/0, pretty_help/0, glint/1, args_count/0, command/1, command_input/0, command_node/1, out/1, metadata/0, flag_help/0, command_help/0]).

-type config() :: {config,
        gleam@option:option(pretty_help()),
        gleam@option:option(binary()),
        boolean()}.

-type pretty_help() :: {pretty_help,
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour()}.

-opaque glint(GKV) :: {glint, config(), command_node(GKV)}.

-type args_count() :: {eq_args, integer()} | {min_args, integer()}.

-opaque command(GKW) :: {command,
        fun((command_input()) -> GKW),
        gleam@dict:dict(binary(), glint@flag:flag()),
        binary(),
        gleam@option:option(args_count()),
        list(binary())}.

-type command_input() :: {command_input,
        list(binary()),
        gleam@dict:dict(binary(), glint@flag:flag()),
        gleam@dict:dict(binary(), binary())}.

-type command_node(GKX) :: {command_node,
        gleam@option:option(command(GKX)),
        gleam@dict:dict(binary(), command_node(GKX)),
        gleam@dict:dict(binary(), glint@flag:flag())}.

-type out(GKY) :: {out, GKY} | {help, binary()}.

-type metadata() :: {metadata, binary(), binary()}.

-type flag_help() :: {flag_help, metadata(), binary()}.

-type command_help() :: {command_help,
        metadata(),
        list(flag_help()),
        list(metadata()),
        gleam@option:option(args_count()),
        list(binary())}.

-spec with_config(glint(GLE), config()) -> glint(GLE).
with_config(Glint, Config) ->
    erlang:setelement(2, Glint, Config).

-spec with_pretty_help(glint(GLH), pretty_help()) -> glint(GLH).
with_pretty_help(Glint, Pretty) ->
    _pipe = erlang:setelement(2, erlang:element(2, Glint), {some, Pretty}),
    with_config(Glint, _pipe).

-spec without_pretty_help(glint(GLK)) -> glint(GLK).
without_pretty_help(Glint) ->
    _pipe = erlang:setelement(2, erlang:element(2, Glint), none),
    with_config(Glint, _pipe).

-spec with_name(glint(GLN), binary()) -> glint(GLN).
with_name(Glint, Name) ->
    _pipe = erlang:setelement(3, erlang:element(2, Glint), {some, Name}),
    with_config(Glint, _pipe).

-spec as_gleam_module(glint(GLQ)) -> glint(GLQ).
as_gleam_module(Glint) ->
    _pipe = erlang:setelement(4, erlang:element(2, Glint), true),
    with_config(Glint, _pipe).

-spec empty_command() -> command_node(any()).
empty_command() ->
    {command_node, none, gleam@dict:new(), gleam@dict:new()}.

-spec do_add(command_node(GMA), list(binary()), command(GMA)) -> command_node(GMA).
do_add(Root, Path, Contents) ->
    case Path of
        [] ->
            erlang:setelement(2, Root, {some, Contents});

        [X | Xs] ->
            erlang:setelement(
                3,
                Root,
                (gleam@dict:update(
                    erlang:element(3, Root),
                    X,
                    fun(Node) -> _pipe = Node,
                        _pipe@1 = gleam@option:lazy_unwrap(
                            _pipe,
                            fun empty_command/0
                        ),
                        do_add(_pipe@1, Xs, Contents) end
                ))
            )
    end.

-spec command(fun((command_input()) -> GMJ)) -> command(GMJ).
command(Runner) ->
    {command, Runner, gleam@dict:new(), <<""/utf8>>, none, []}.

-spec description(command(GMM), binary()) -> command(GMM).
description(Cmd, Description) ->
    erlang:setelement(4, Cmd, Description).

-spec unnamed_args(command(GMP), args_count()) -> command(GMP).
unnamed_args(Cmd, Count) ->
    erlang:setelement(5, Cmd, {some, Count}).

-spec named_args(command(GMS), list(binary())) -> command(GMS).
named_args(Cmd, Args) ->
    erlang:setelement(6, Cmd, Args).

-spec flag(command(GMW), binary(), glint@flag:flag_builder(any())) -> command(GMW).
flag(Cmd, Key, Flag) ->
    erlang:setelement(
        3,
        Cmd,
        gleam@dict:insert(erlang:element(3, Cmd), Key, glint@flag:build(Flag))
    ).

-spec flag_tuple(command(GNB), {binary(), glint@flag:flag_builder(any())}) -> command(GNB).
flag_tuple(Cmd, Tup) ->
    flag(Cmd, erlang:element(1, Tup), erlang:element(2, Tup)).

-spec flags(command(GNG), list({binary(), glint@flag:flag()})) -> command(GNG).
flags(Cmd, Flags) ->
    gleam@list:fold(
        Flags,
        Cmd,
        fun(Cmd@1, _use1) ->
            {Key, Flag} = _use1,
            erlang:setelement(
                3,
                Cmd@1,
                gleam@dict:insert(erlang:element(3, Cmd@1), Key, Flag)
            )
        end
    ).

-spec do_group_flag(
    command_node(GOP),
    list(binary()),
    binary(),
    glint@flag:flag()
) -> command_node(GOP).
do_group_flag(Node, Path, Name, Flag) ->
    case Path of
        [] ->
            erlang:setelement(
                4,
                Node,
                gleam@dict:insert(erlang:element(4, Node), Name, Flag)
            );

        [Head | Tail] ->
            erlang:setelement(
                3,
                Node,
                (gleam@dict:update(
                    erlang:element(3, Node),
                    Head,
                    fun(Node@1) -> _pipe = Node@1,
                        _pipe@1 = gleam@option:unwrap(_pipe, empty_command()),
                        do_group_flag(_pipe@1, Tail, Name, Flag) end
                ))
            )
    end.

-spec group_flags(
    glint(GNY),
    list(binary()),
    list({binary(), glint@flag:flag()})
) -> glint(GNY).
group_flags(Glint, Path, Flags) ->
    gleam@list:fold(
        Flags,
        Glint,
        fun(Glint@1, Flag) ->
            erlang:setelement(
                3,
                Glint@1,
                do_group_flag(
                    erlang:element(3, Glint@1),
                    Path,
                    erlang:element(1, Flag),
                    erlang:element(2, Flag)
                )
            )
        end
    ).

-spec global_flags(glint(GNU), list({binary(), glint@flag:flag()})) -> glint(GNU).
global_flags(Glint, Flags) ->
    group_flags(Glint, [], Flags).

-spec group_flag(
    glint(GOD),
    list(binary()),
    binary(),
    glint@flag:flag_builder(any())
) -> glint(GOD).
group_flag(Glint, Path, Name, Flag) ->
    erlang:setelement(
        3,
        Glint,
        do_group_flag(
            erlang:element(3, Glint),
            Path,
            Name,
            glint@flag:build(Flag)
        )
    ).

-spec global_flag(glint(GNK), binary(), glint@flag:flag_builder(any())) -> glint(GNK).
global_flag(Glint, Key, Flag) ->
    group_flag(Glint, [], Key, Flag).

-spec global_flag_tuple(glint(GNP), {binary(), glint@flag:flag_builder(any())}) -> glint(GNP).
global_flag_tuple(Glint, Tup) ->
    group_flag(Glint, [], erlang:element(1, Tup), erlang:element(2, Tup)).

-spec group_flag_tuple(
    glint(GOJ),
    list(binary()),
    {binary(), glint@flag:flag_builder(any())}
) -> glint(GOJ).
group_flag_tuple(Glint, Path, Flag) ->
    group_flag(Glint, Path, erlang:element(1, Flag), erlang:element(2, Flag)).

-spec args_compare(args_count(), integer()) -> {ok, nil} | {error, snag:snag()}.
args_compare(Expected, Actual) ->
    _pipe = case Expected of
        {eq_args, Expected@1} when Actual =:= Expected@1 ->
            {ok, nil};

        {min_args, Expected@2} when Actual >= Expected@2 ->
            {ok, nil};

        {eq_args, Expected@3} ->
            {error, gleam@int:to_string(Expected@3)};

        {min_args, Expected@4} ->
            {error,
                <<"at least "/utf8, (gleam@int:to_string(Expected@4))/binary>>}
    end,
    gleam@result:map_error(
        _pipe,
        fun(Err) ->
            snag:new(
                <<<<<<"expected: "/utf8, Err/binary>>/binary,
                        " argument(s), provided: "/utf8>>/binary,
                    (gleam@int:to_string(Actual))/binary>>
            )
        end
    ).

-spec default_pretty_help() -> pretty_help().
default_pretty_help() ->
    _assert_subject = gleam_community@colour:from_rgb255(182, 255, 234),
    {ok, Usage_colour} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 597})
    end,
    _assert_subject@1 = gleam_community@colour:from_rgb255(255, 175, 243),
    {ok, Flags_colour} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 598})
    end,
    _assert_subject@2 = gleam_community@colour:from_rgb255(252, 226, 174),
    {ok, Subcommands_colour} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 599})
    end,
    {pretty_help, Usage_colour, Flags_colour, Subcommands_colour}.

-spec is_not_empty(binary()) -> boolean().
is_not_empty(S) ->
    S /= <<""/utf8>>.

-spec sanitize_path(list(binary())) -> list(binary()).
sanitize_path(Path) ->
    _pipe = Path,
    _pipe@1 = gleam@list:map(_pipe, fun gleam@string:trim/1),
    gleam@list:filter(_pipe@1, fun is_not_empty/1).

-spec add(glint(GLV), list(binary()), command(GLV)) -> glint(GLV).
add(Glint, Path, Contents) ->
    erlang:setelement(
        3,
        Glint,
        begin
            _pipe = Path,
            _pipe@1 = sanitize_path(_pipe),
            do_add(erlang:element(3, Glint), _pipe@1, Contents)
        end
    ).

-spec heading_style(binary(), gleam_community@colour:colour()) -> binary().
heading_style(Heading, Colour) ->
    _pipe = Heading,
    _pipe@1 = gleam_community@ansi:bold(_pipe),
    _pipe@2 = gleam_community@ansi:underline(_pipe@1),
    _pipe@3 = gleam_community@ansi:italic(_pipe@2),
    _pipe@4 = gleam_community@ansi:hex(
        _pipe@3,
        gleam_community@colour:to_rgb_hex(Colour)
    ),
    gleam_community@ansi:reset(_pipe@4).

-spec flag_type_info(glint@flag:flag()) -> binary().
flag_type_info(Flag) ->
    case erlang:element(2, Flag) of
        {i, _} ->
            <<"INT"/utf8>>;

        {b, _} ->
            <<"BOOL"/utf8>>;

        {f, _} ->
            <<"FLOAT"/utf8>>;

        {lf, _} ->
            <<"FLOAT_LIST"/utf8>>;

        {li, _} ->
            <<"INT_LIST"/utf8>>;

        {ls, _} ->
            <<"STRING_LIST"/utf8>>;

        {s, _} ->
            <<"STRING"/utf8>>
    end.

-spec build_flags_help(gleam@dict:dict(binary(), glint@flag:flag())) -> list(flag_help()).
build_flags_help(Flag) ->
    gleam@dict:fold(
        Flag,
        [],
        fun(Acc, Name, Flag@1) ->
            [{flag_help,
                    {metadata, Name, erlang:element(3, Flag@1)},
                    flag_type_info(Flag@1)} |
                Acc]
        end
    ).

-spec build_subcommands_help(gleam@dict:dict(binary(), command_node(any()))) -> list(metadata()).
build_subcommands_help(Subcommands) ->
    gleam@dict:fold(
        Subcommands,
        [],
        fun(Acc, Name, Cmd) ->
            [{metadata,
                    Name,
                    begin
                        _pipe = erlang:element(2, Cmd),
                        _pipe@1 = gleam@option:map(
                            _pipe,
                            fun(Command) -> erlang:element(4, Command) end
                        ),
                        gleam@option:unwrap(_pipe@1, <<""/utf8>>)
                    end} |
                Acc]
        end
    ).

-spec build_command_help_metadata(binary(), command_node(any())) -> command_help().
build_command_help_metadata(Name, Node) ->
    {Description, Flags, Unnamed_args, Named_args} = case erlang:element(
        2,
        Node
    ) of
        none ->
            {<<""/utf8>>, [], none, []};

        {some, Cmd} ->
            {erlang:element(4, Cmd),
                build_flags_help(
                    gleam@dict:merge(
                        erlang:element(4, Node),
                        erlang:element(3, Cmd)
                    )
                ),
                erlang:element(5, Cmd),
                erlang:element(6, Cmd)}
    end,
    {command_help,
        {metadata, Name, Description},
        Flags,
        build_subcommands_help(erlang:element(3, Node)),
        Unnamed_args,
        Named_args}.

-spec args_count_to_usage_string(args_count()) -> binary().
args_count_to_usage_string(Count) ->
    case Count of
        {eq_args, 0} ->
            <<""/utf8>>;

        {eq_args, 1} ->
            <<"[ 1 argument ]"/utf8>>;

        {eq_args, N} ->
            <<<<"[ "/utf8, (gleam@int:to_string(N))/binary>>/binary,
                " arguments ]"/utf8>>;

        {min_args, N@1} ->
            <<<<"[ "/utf8, (gleam@int:to_string(N@1))/binary>>/binary,
                " or more arguments ]"/utf8>>
    end.

-spec args_to_usage_string(gleam@option:option(args_count()), list(binary())) -> binary().
args_to_usage_string(Unnamed, Named) ->
    Named_args = begin
        _pipe = Named,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(S) -> <<<<"<"/utf8, S/binary>>/binary, ">"/utf8>> end
        ),
        gleam@string:join(_pipe@1, <<" "/utf8>>)
    end,
    Unnamed_args = begin
        _pipe@2 = gleam@option:map(Unnamed, fun args_count_to_usage_string/1),
        gleam@option:unwrap(_pipe@2, <<"[ ARGS ]"/utf8>>)
    end,
    case {Named_args, Unnamed_args} of
        {<<""/utf8>>, <<""/utf8>>} ->
            <<""/utf8>>;

        {<<""/utf8>>, _} ->
            Unnamed_args;

        {_, <<""/utf8>>} ->
            Named_args;

        {_, _} ->
            <<<<Named_args/binary, " "/utf8>>/binary, Unnamed_args/binary>>
    end.

-spec flag_help_to_string(flag_help()) -> binary().
flag_help_to_string(Help) ->
    <<<<<<<<(<<"--"/utf8>>)/binary,
                    (erlang:element(2, erlang:element(2, Help)))/binary>>/binary,
                "=<"/utf8>>/binary,
            (erlang:element(3, Help))/binary>>/binary,
        ">"/utf8>>.

-spec flags_help_to_usage_strings(list(flag_help())) -> list(binary()).
flags_help_to_usage_strings(Help) ->
    _pipe = Help,
    _pipe@1 = gleam@list:map(_pipe, fun flag_help_to_string/1),
    gleam@list:sort(_pipe@1, fun gleam@string:compare/2).

-spec flags_help_to_usage_string(list(flag_help())) -> binary().
flags_help_to_usage_string(Help) ->
    gleam@bool:guard(Help =:= [], <<""/utf8>>, fun() -> _pipe = Help,
            _pipe@1 = flags_help_to_usage_strings(_pipe),
            _pipe@2 = gleam@list:intersperse(_pipe@1, <<" "/utf8>>),
            _pipe@3 = gleam@string_builder:from_strings(_pipe@2),
            _pipe@4 = gleam@string_builder:prepend(_pipe@3, <<"[ "/utf8>>),
            _pipe@5 = gleam@string_builder:append(_pipe@4, <<" ]"/utf8>>),
            gleam@string_builder:to_string(_pipe@5) end).

-spec flag_help_to_string_with_description(flag_help()) -> binary().
flag_help_to_string_with_description(Help) ->
    <<<<(flag_help_to_string(Help))/binary, "\t\t"/utf8>>/binary,
        (erlang:element(3, erlang:element(2, Help)))/binary>>.

-spec subcommand_help_to_string(metadata()) -> binary().
subcommand_help_to_string(Help) ->
    case erlang:element(3, Help) of
        <<""/utf8>> ->
            erlang:element(2, Help);

        _ ->
            <<<<(erlang:element(2, Help))/binary, "\t\t"/utf8>>/binary,
                (erlang:element(3, Help))/binary>>
    end.

-spec string_map(binary(), fun((binary()) -> binary())) -> binary().
string_map(S, F) ->
    case S of
        <<""/utf8>> ->
            <<""/utf8>>;

        _ ->
            F(S)
    end.

-spec new() -> glint(any()).
new() ->
    {glint, {config, none, none, false}, empty_command()}.

-spec subcommands_help_to_string(list(metadata()), config()) -> binary().
subcommands_help_to_string(Help, Config) ->
    gleam@bool:guard(
        Help =:= [],
        <<""/utf8>>,
        fun() -> <<(case erlang:element(2, Config) of
                    none ->
                        <<"SUBCOMMANDS:"/utf8>>;

                    {some, Pretty} ->
                        heading_style(
                            <<"SUBCOMMANDS:"/utf8>>,
                            erlang:element(4, Pretty)
                        )
                end)/binary, (begin
                    _pipe = Help,
                    _pipe@1 = gleam@list:map(
                        _pipe,
                        fun subcommand_help_to_string/1
                    ),
                    _pipe@2 = gleam@list:sort(
                        _pipe@1,
                        fun gleam@string:compare/2
                    ),
                    _pipe@3 = gleam@list:map(
                        _pipe@2,
                        fun(_capture) ->
                            gleam@string:append(<<"\n\t"/utf8>>, _capture)
                        end
                    ),
                    gleam@string:concat(_pipe@3)
                end)/binary>> end
    ).

-spec command_help_to_usage_string(command_help(), config()) -> binary().
command_help_to_usage_string(Help, Config) ->
    App_name = case erlang:element(3, Config) of
        {some, Name} when erlang:element(4, Config) ->
            <<"gleam run -m "/utf8, Name/binary>>;

        {some, Name@1} ->
            Name@1;

        none ->
            <<"gleam run"/utf8>>
    end,
    Flags = flags_help_to_usage_string(erlang:element(3, Help)),
    Args = args_to_usage_string(
        erlang:element(5, Help),
        erlang:element(6, Help)
    ),
    <<<<<<<<<<(case erlang:element(2, Config) of
                            none ->
                                <<"USAGE:"/utf8>>;

                            {some, Pretty} ->
                                heading_style(
                                    <<"USAGE:"/utf8>>,
                                    erlang:element(2, Pretty)
                                )
                        end)/binary, "\n\t"/utf8>>/binary, App_name/binary>>/binary, (string_map(
                    erlang:element(2, erlang:element(2, Help)),
                    fun(_capture) ->
                        gleam@string:append(<<" "/utf8>>, _capture)
                    end
                ))/binary>>/binary, (case Args of
                <<""/utf8>> ->
                    <<" "/utf8>>;

                _ ->
                    <<<<" "/utf8, Args/binary>>/binary, " "/utf8>>
            end)/binary>>/binary, Flags/binary>>.

-spec help_flag() -> binary().
help_flag() ->
    <<(<<"--"/utf8>>)/binary, "help"/utf8>>.

-spec flags_help_to_string(list(flag_help()), config()) -> binary().
flags_help_to_string(Help, Config) ->
    gleam@bool:guard(
        Help =:= [],
        <<""/utf8>>,
        fun() -> <<(case erlang:element(2, Config) of
                    none ->
                        <<"FLAGS:"/utf8>>;

                    {some, Pretty} ->
                        heading_style(
                            <<"FLAGS:"/utf8>>,
                            erlang:element(3, Pretty)
                        )
                end)/binary, (begin
                    _pipe = [<<"--help\t\t\tPrint help information"/utf8>> |
                        gleam@list:map(
                            Help,
                            fun flag_help_to_string_with_description/1
                        )],
                    _pipe@1 = gleam@list:sort(_pipe, fun gleam@string:compare/2),
                    _pipe@2 = gleam@list:map(
                        _pipe@1,
                        fun(_capture) ->
                            gleam@string:append(<<"\n\t"/utf8>>, _capture)
                        end
                    ),
                    gleam@string:concat(_pipe@2)
                end)/binary>> end
    ).

-spec command_help_to_string(command_help(), config()) -> binary().
command_help_to_string(Help, Config) ->
    Header_items = begin
        _pipe = [erlang:element(2, erlang:element(2, Help)),
            erlang:element(3, erlang:element(2, Help))],
        _pipe@1 = gleam@list:filter(_pipe, fun is_not_empty/1),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    _pipe@2 = [Header_items,
        command_help_to_usage_string(Help, Config),
        flags_help_to_string(erlang:element(3, Help), Config),
        subcommands_help_to_string(erlang:element(4, Help), Config)],
    _pipe@3 = gleam@list:filter(_pipe@2, fun is_not_empty/1),
    gleam@string:join(_pipe@3, <<"\n\n"/utf8>>).

-spec cmd_help(list(binary()), command_node(any()), config()) -> binary().
cmd_help(Path, Cmd, Config) ->
    _pipe = Path,
    _pipe@1 = gleam@list:reverse(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<" "/utf8>>),
    _pipe@3 = build_command_help_metadata(_pipe@2, Cmd),
    command_help_to_string(_pipe@3, Config).

-spec execute_root(
    list(binary()),
    config(),
    command_node(GPF),
    list(binary()),
    list(binary())
) -> {ok, out(GPF)} | {error, binary()}.
execute_root(Path, Config, Cmd, Args, Flag_inputs) ->
    Res = begin
        _pipe@7 = (gleam@option:map(
            erlang:element(2, Cmd),
            fun(Contents) ->
                gleam@result:'try'(
                    gleam@list:try_fold(
                        Flag_inputs,
                        gleam@dict:merge(
                            erlang:element(4, Cmd),
                            erlang:element(3, Contents)
                        ),
                        fun glint@flag:update_flags/2
                    ),
                    fun(New_flags) ->
                        gleam@result:'try'(
                            begin
                                Named = gleam@list:zip(
                                    erlang:element(6, Contents),
                                    Args
                                ),
                                case gleam@list:length(Named) =:= gleam@list:length(
                                    erlang:element(6, Contents)
                                ) of
                                    true ->
                                        {ok, maps:from_list(Named)};

                                    false ->
                                        snag:error(
                                            <<"unmatched named arguments: "/utf8,
                                                (begin
                                                    _pipe = erlang:element(
                                                        6,
                                                        Contents
                                                    ),
                                                    _pipe@1 = gleam@list:drop(
                                                        _pipe,
                                                        gleam@list:length(Named)
                                                    ),
                                                    _pipe@2 = gleam@list:map(
                                                        _pipe@1,
                                                        fun(S) ->
                                                            <<<<"'"/utf8,
                                                                    S/binary>>/binary,
                                                                "'"/utf8>>
                                                        end
                                                    ),
                                                    gleam@string:join(
                                                        _pipe@2,
                                                        <<", "/utf8>>
                                                    )
                                                end)/binary>>
                                        )
                                end
                            end,
                            fun(Named_args) ->
                                Args@1 = gleam@list:drop(
                                    Args,
                                    maps:size(Named_args)
                                ),
                                gleam@result:map(
                                    case erlang:element(5, Contents) of
                                        {some, Count} ->
                                            _pipe@3 = Count,
                                            _pipe@4 = args_compare(
                                                _pipe@3,
                                                gleam@list:length(Args@1)
                                            ),
                                            snag:context(
                                                _pipe@4,
                                                <<"invalid number of arguments provided"/utf8>>
                                            );

                                        none ->
                                            {ok, nil}
                                    end,
                                    fun(_) ->
                                        _pipe@5 = {command_input,
                                            Args@1,
                                            New_flags,
                                            Named_args},
                                        _pipe@6 = (erlang:element(2, Contents))(
                                            _pipe@5
                                        ),
                                        {out, _pipe@6}
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )),
        _pipe@8 = gleam@option:unwrap(
            _pipe@7,
            snag:error(<<"command not found"/utf8>>)
        ),
        _pipe@9 = snag:context(_pipe@8, <<"failed to run command"/utf8>>),
        gleam@result:map_error(
            _pipe@9,
            fun(Err) -> {Err, cmd_help(Path, Cmd, Config)} end
        )
    end,
    case Res of
        {ok, Out} ->
            {ok, Out};

        {error, {Snag, Help}} ->
            {error,
                <<<<(snag:pretty_print(Snag))/binary,
                        "\nSee the following help text, available via the '--help' flag.\n\n"/utf8>>/binary,
                    Help/binary>>}
    end.

-spec do_execute(
    command_node(GOX),
    config(),
    list(binary()),
    list(binary()),
    boolean(),
    list(binary())
) -> {ok, out(GOX)} | {error, binary()}.
do_execute(Cmd, Config, Args, Flags, Help, Command_path) ->
    case Args of
        [] when Help ->
            _pipe = Command_path,
            _pipe@1 = cmd_help(_pipe, Cmd, Config),
            _pipe@2 = {help, _pipe@1},
            {ok, _pipe@2};

        [] ->
            execute_root(Command_path, Config, Cmd, [], Flags);

        [Arg | Rest] ->
            case gleam@dict:get(erlang:element(3, Cmd), Arg) of
                {ok, Sub_command} ->
                    Sub_command@1 = erlang:setelement(
                        4,
                        Sub_command,
                        gleam@dict:merge(
                            erlang:element(4, Cmd),
                            erlang:element(4, Sub_command)
                        )
                    ),
                    do_execute(
                        Sub_command@1,
                        Config,
                        Rest,
                        Flags,
                        Help,
                        [Arg | Command_path]
                    );

                _ when Help ->
                    _pipe@3 = Command_path,
                    _pipe@4 = cmd_help(_pipe@3, Cmd, Config),
                    _pipe@5 = {help, _pipe@4},
                    {ok, _pipe@5};

                _ ->
                    execute_root(Command_path, Config, Cmd, Args, Flags)
            end
    end.

-spec execute(glint(GOT), list(binary())) -> {ok, out(GOT)} | {error, binary()}.
execute(Glint, Args) ->
    Help_flag = help_flag(),
    {Help, Args@2} = case gleam@list:pop(Args, fun(S) -> S =:= Help_flag end) of
        {ok, {_, Args@1}} ->
            {true, Args@1};

        _ ->
            {false, Args}
    end,
    {Flags, Args@3} = gleam@list:partition(
        Args@2,
        fun(_capture) -> gleam@string:starts_with(_capture, <<"--"/utf8>>) end
    ),
    do_execute(
        erlang:element(3, Glint),
        erlang:element(2, Glint),
        Args@3,
        Flags,
        Help,
        []
    ).

-spec run_and_handle(glint(GPN), list(binary()), fun((GPN) -> any())) -> nil.
run_and_handle(Glint, Args, Handle) ->
    case execute(Glint, Args) of
        {error, S} ->
            gleam@io:println(S);

        {ok, {help, S}} ->
            gleam@io:println(S);

        {ok, {out, Out}} ->
            Handle(Out),
            nil
    end.

-spec run(glint(any()), list(binary())) -> nil.
run(Glint, Args) ->
    run_and_handle(Glint, Args, fun(_) -> nil end).
