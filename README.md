# anilist-behind

List the releasing shows that you're behind on according to [AniList](https://anilist.co/).

## Installation

If you're familiar with [Nix](https://nixos.org/), you can install anlist-behind using the included flake. Otherwise, install [Gleam](https://gleam.run/), clone the repository, and run `gleam export erlang-shipment` from the root of the repository. This will create a directory in `build/erlang-shipment` containing all the compiled bytecode and other assets. Copy this directory somewhere where it won't get deleted. Then create a wrapper script named `anilist-behind` in one of the directories on your path with the contents:

```bash
#!/usr/bin/env bash

# Replace the path below with the path to your entrypoint.sh
exec ".../erlang-shipment/entrypoint.sh" run "$@"
```

Running `anlist-behind --help` should now produce the output below.

## Usage

```console
$ anilist-behind --help
List the releasing shows that you're behind on according to AniList.

If your profile is private, you'll need to visit https://anilist.co/api/v2/oauth/authorize?client_id=18148&response_type=token to obtain a token.

USAGE:
	anilist-behind [ --count=<BOOL> --token=<STRING> --user-name=<STRING> ]

FLAGS:
	--count=<BOOL>		Display only the number of shows.
	--help			Print help information
	--token=<STRING>		AniList API token obtained by following the link above. Only necessary if your profile is private.
	--user-name=<STRING>		Your AniList username.
```
