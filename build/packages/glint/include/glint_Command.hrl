-record(command, {
    do :: fun((glint:command_input()) -> any()),
    flags :: gleam@dict:dict(binary(), glint@flag:flag()),
    description :: binary(),
    unnamed_args :: gleam@option:option(glint:args_count()),
    named_args :: list(binary())
}).
