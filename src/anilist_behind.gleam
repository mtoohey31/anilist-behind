import argv
import gleam/dynamic.{
  type DecodeErrors, type Dynamic, DecodeError, bool, decode1, decode2, decode4,
  field, list, string,
}
import gleam/hackney
import gleam/int.{to_string}
import gleam/io.{debug, println, println_error}
import gleam/json
import gleam/list.{filter, length}
import gleam/option.{type Option, None, Some}
import gleamql.{
  type GraphQLError, type Request, ErrorMessage, UnexpectedStatus, UnknownError,
  UnrecognisedResponse, send, set_decoder, set_header, set_host, set_query,
  set_variable,
}
import gleam/result.{try}
import gleam/string.{concat, join}
import glint/flag.{default, get_bool, get_string}
import glint.{
  type CommandInput, EqArgs, add, command, default_pretty_help, flag, run,
  unnamed_args, with_name, with_pretty_help,
}
import snag.{type Snag, pretty_print}

const user_id_query = "query UserIDQuery($userName: String) {
  User(name: $userName) {
    id
  }
}"

const current_query = "query CurrentQuery($userId: Int, $page: Int) {
  Page(page: $page) {
    pageInfo {
      hasNextPage
    },
    mediaList(userId: $userId, type: ANIME, status_in: [CURRENT, PLANNING]) {
      media {
        episodes,
        nextAiringEpisode {
          episode
        },
        title {
          userPreferred
        },
        status(version: 2)
      },
      progress
    }
  }
}"

type Page {
  Page(page_info: PageInfo, media_list: List(MediaList))
}

type PageInfo {
  PageInfo(has_next_page: Bool)
}

type MediaList {
  MediaList(media: Media, progress: Int)
}

fn is_behind(media_list: MediaList) -> Bool {
  let MediaList(
    Media(total_episodes, next_airing_episode, _, status),
    episode_progress,
  ) = media_list
  case
    option.map(total_episodes, fn(total_episodes) {
      episode_progress == total_episodes
    })
  {
    Some(True) -> False
    _ ->
      case status {
        // BUG: We only want to display shows that are currently releasing, but
        // a show will be listed as "Finished" as soon as its last episode airs.
        // We need to make sure that the last episode wasn't aired recently. We
        // need a similar fix to handle the "Hiatus" status. Using Media.endDate
        // seems like a good way to check this, but there doesn't appear to be
        // a datetime package for gleam yet, so we'd probably have to use erlang
        // FFI for this?
        Releasing ->
          case next_airing_episode {
            Some(AiringSchedule(next_episode)) ->
              episode_progress + 1 < next_episode
            // NOTE: The response might not be accurate in this case, but
            // there's not really another good way to determine this without
            // querying more data than we need up-front, or follow-up queries
            // for each show that ends up in this branch. Hopefully this will
            // be rare.
            None -> False
          }
        _ -> False
      }
  }
}

fn user_preferred_title(media_list: MediaList) -> String {
  let MediaList(Media(title: MediaTitle(user_preferred_title), ..), _) =
    media_list
  user_preferred_title
}

type Media {
  Media(
    episodes: Option(Int),
    next_airing_episode: Option(AiringSchedule),
    title: MediaTitle,
    status: Status,
  )
}

type AiringSchedule {
  AiringSchedule(episode: Int)
}

type MediaTitle {
  MediaTitle(user_preferred: String)
}

type Status {
  Finished
  Releasing
  NotYetReleased
  Cancelled
  Hiatus
}

fn decode_status(d: Dynamic) -> Result(Status, DecodeErrors) {
  use s <- try(string(d))
  case s {
    "FINISHED" -> Ok(Finished)
    "RELEASING" -> Ok(Releasing)
    "NOT_YET_RELEASED" -> Ok(NotYetReleased)
    "CANCELLED" -> Ok(Cancelled)
    "HIATUS" -> Ok(Hiatus)
    u -> Error([DecodeError("status", u, [])])
  }
}

fn set_host_and_headers(req: Request(a), token: Option(String)) -> Request(a) {
  let req1 =
    set_host(req, "graphql.anilist.co")
    |> set_header("Content-Type", "application/json")
    |> set_header("Accept", "application/json")
  case token {
    Some(token) ->
      set_header(req1, "Authorization", string.append("Bearer ", token))
    None -> req1
  }
}

fn user_name_to_id(
  user_name: String,
  token: Option(String),
) -> Result(Int, GraphQLError) {
  gleamql.new()
  |> set_query(user_id_query)
  |> set_variable("userName", json.string(user_name))
  |> set_host_and_headers(token)
  |> set_decoder(field("User", field("id", dynamic.int)))
  |> send(hackney.send)
  |> result.map(fn(opt) {
    let assert Some(res) = opt
    res
  })
}

fn query(
  page: Int,
  user_id: Int,
  token: Option(String),
  current_data_decoder,
  media_list: List(MediaList),
) -> Result(List(MediaList), GraphQLError) {
  let req =
    gleamql.new()
    |> set_query(current_query)
    |> set_variable("userId", json.int(user_id))
    |> set_variable("page", json.int(page))
    |> set_host_and_headers(token)
    |> set_decoder(current_data_decoder)
  use opt <- try(send(req, hackney.send))
  let assert Some(Page(PageInfo(has_next_page), media_list1)) = opt
  let media_list2 = list.append(media_list, media_list1)
  case has_next_page {
    True -> query(page + 1, user_id, token, current_data_decoder, media_list2)
    False -> Ok(media_list2)
  }
}

fn try_snag(
  result: Result(a, Snag),
  apply fun: fn(a) -> Result(c, Nil),
) -> Result(c, Nil) {
  result.try(
    result.map_error(result, fn(snag) { println_error(pretty_print(snag)) }),
    fun,
  )
}

fn try_graphql(
  result: Result(a, GraphQLError),
  apply fun: fn(a) -> Result(c, Nil),
) -> Result(c, Nil) {
  result.try(
    result.map_error(result, fn(graphql_error) {
      case graphql_error {
        ErrorMessage(message) -> println_error(message)
        UnexpectedStatus(status) ->
          println_error(string.append(
            "AniList API responded with unexpected status: ",
            to_string(status),
          ))
        UnrecognisedResponse(response) ->
          println_error(string.append(
            "AniList API responded successfully, but response format was not recognized. See response below.\n\n",
            response,
          ))
        UnknownError(inner) -> {
          println_error(
            "Unknown error when querying AniList API. See error data below.\n",
          )
          debug(inner)
          Nil
        }
      }
    }),
    fun,
  )
}

const count_key = "count"

const token_key = "token"

const user_name_key = "user-name"

fn behind(input: CommandInput) {
  let current_data_decoder =
    field(
      "Page",
      decode2(
        Page,
        field("pageInfo", decode1(PageInfo, field("hasNextPage", bool))),
        field(
          "mediaList",
          list(decode2(
            MediaList,
            field(
              "media",
              decode4(
                Media,
                field("episodes", dynamic.optional(dynamic.int)),
                field(
                  "nextAiringEpisode",
                  dynamic.optional(decode1(
                    AiringSchedule,
                    field("episode", dynamic.int),
                  )),
                ),
                field(
                  "title",
                  decode1(MediaTitle, field("userPreferred", string)),
                ),
                field("status", decode_status),
              ),
            ),
            field("progress", dynamic.int),
          )),
        ),
      ),
    )

  let assert Ok(count) = get_bool(from: input.flags, for: count_key)
  let token = option.from_result(get_string(from: input.flags, for: token_key))
  use user_name <- try_snag(get_string(from: input.flags, for: user_name_key))

  use user_id <- try_graphql(user_name_to_id(user_name, token))

  use media_list <- try_graphql(
    query(1, user_id, token, current_data_decoder, []),
  )

  let filtered_media_list = filter(media_list, is_behind)
  case count {
    True ->
      filtered_media_list
      |> length
      |> to_string
      |> println
    False ->
      filtered_media_list
      |> list.map(user_preferred_title)
      |> join("\n")
      |> println
  }

  Ok(Nil)
}

const client_id = 18_148

// TODO: Use appropriate exit codes when errors occur.

pub fn main() {
  glint.new()
  |> with_name("anilist-behind")
  |> with_pretty_help(default_pretty_help())
  |> add(
    at: [],
    do: command(behind)
      |> flag(
        count_key,
        flag.bool()
          |> default(False)
          |> flag.description("Display only the number of shows."),
      )
      |> flag(
        token_key,
        flag.string()
          |> flag.description(
            "AniList API token obtained by following the link above. Only necessary if your profile is private.",
          ),
      )
      |> flag(
        user_name_key,
        flag.string()
          |> flag.description("Your AniList username."),
      )
      |> unnamed_args(EqArgs(0))
      |> glint.description(
        concat([
          "List the releasing shows that you're behind on according to AniList.

If your profile is private, you'll need to visit https://anilist.co/api/v2/oauth/authorize?client_id=",
          to_string(client_id),
          "&response_type=token to obtain a token.",
        ]),
      ),
  )
  |> run(argv.load().arguments)
}
