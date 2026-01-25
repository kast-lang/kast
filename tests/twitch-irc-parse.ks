# Just parsing twitch irc message
use std.collections.Map;

const Msg = type {
    .tags :: Map.t[String, String],
    .prefix :: Option.t[String],
    .command :: String,
    .params :: List.t[String],
    .trailing :: Option.t[String],
};
let rsplit_at = (s :: String, c :: Char) -> { String, String } => (
    let i = String.last_index_of(c, s);
    {
        String.substring(s, 0, i),
        String.substring(s, i + 1, String.length(s) - i - 1),
    }
);
let parse_tags = (s :: String) -> Map.t[String, String] => (
    let mut tags = Map.create();
    for part in String.split(s, ';') do (
        let { key, value } = String.split_once(part, '=');
        Map.add(&mut tags, key, value);
    );
    
    tags
);
let parse_msg = (msg :: String) -> Msg => with_return (
    let mut unparsed = msg;
    let mut tags = Map.create();
    let mut prefix = :None;
    let mut command = :None;
    let mut params = List.create();
    let mut trailing = :None;
    let add_part = s => (
        let first = String.at(s, 0);
        if first == '@' then (
            tags = parse_tags(
                String.substring(s, 1, String.length(s) - 1)
            );
        ) else if first == ':' then (
            prefix = :Some (String.substring(s, 1, String.length(s) - 1));
        ) else if &command |> Option.is_none then (
            command = :Some s;
        ) else (
            List.push_back(&mut params, s);
        );
    );
    loop (
        if (&command |> Option.is_some)
        and String.at(unparsed, 0) == ':' then (
            trailing = :Some (String.substring(unparsed, 1, String.length(unparsed) - 1));
            break;
        );
        let space_idx = String.index_of(' ', unparsed);
        if space_idx == -1 then (
            add_part(unparsed);
            break;
        );
        { (let part), unparsed } = String.split_once(unparsed, ' ');
        add_part(part);
    );
    {
        .tags,
        .prefix,
        .command = command |> Option.unwrap,
        .params,
        .trailing,
    }
);
const User = newtype {
    .nick :: String,
    .user :: String,
    .host :: String,
};
let parse_user = (s :: String) -> User => (
    let { before_at, host } = String.split_once(s, '@');
    let { nick, user } = String.split_once(before_at, '!');
    { .nick, .user, .host }
);

let raw_msg = "@badge-info=subscriber/53;badges=broadcaster/1,subscriber/0,hornet/1;client-nonce=df1d6cc48c1f4e8e8dc49de271c2203e;color=#FF69B4;display-name=kuviman;emotes=;first-msg=0;flags=;id=3ecda213-8e3c-4a1f-b9b6-9bf22c74b954;mod=0;returning-chatter=0;room-id=105277748;subscriber=1;tmi-sent-ts=1768541275311;turbo=0;user-id=105277748;user-type= :kuviman!kuviman@kuviman.tmi.twitch.tv PRIVMSG #kuviman :test";
let msg = raw_msg |> parse_msg;
dbg.print({ .msg });
