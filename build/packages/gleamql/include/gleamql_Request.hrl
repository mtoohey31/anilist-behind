-record(request, {
    http_request :: gleam@http@request:request(binary()),
    'query' :: gleam@option:option(binary()),
    variables :: gleam@option:option(list({binary(), gleam@json:json()})),
    decoder :: gleam@option:option(fun((gleam@dynamic:dynamic_()) -> {ok, any()} |
        {error, list(gleam@dynamic:decode_error())}))
}).
