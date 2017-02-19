open Lwt.Infix

type id_cache = {
  ttl: float;
  mutable last_modified: float;
  cache: (string, string) Hashtbl.t;
}

let create_id_cache ttl = {
  ttl;
  last_modified = 0.0;
  cache = Hashtbl.create 10;
}

let cache_flush c =
  Hashtbl.clear c.cache;
  c.last_modified <- 0.0

let cache_update c kvs =
  Hashtbl.clear c.cache;
  List.iter (fun (k, v) -> Hashtbl.add c.cache k v) kvs;
  c.last_modified <- Unix.gettimeofday ()

let cache_refresh c lookup_fun =
  match Unix.gettimeofday () -. c.ttl > c.last_modified with
  | false -> Lwt.return ()
  | true -> lookup_fun () >|= cache_update c

let cache_lookup c k lookup_fun =
  cache_refresh c lookup_fun >|= fun () ->
  try Some (Hashtbl.find c.cache k) with Not_found -> None

type t = {
  base_url: string;
  token: string;
  channel_cache: id_cache;
  group_cache: id_cache;
  user_cache: id_cache;
}

let create ?(channel_ttl=60.0) ?(user_ttl=60.0) base_url token = {
  base_url;
  token;
  channel_cache = create_id_cache channel_ttl;
  group_cache = create_id_cache channel_ttl;
  user_cache = create_id_cache user_ttl;
}

let flush_caches session =
  List.iter cache_flush
    [session.channel_cache; session.group_cache; session.user_cache]
