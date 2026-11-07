(* Assembly for the currently available web backend sessions. Implementations
   are shared with Emacs; this module only selects configuration and capabilities. *)
module S=Reader.Service
module C=Reader.Content_state
module RSS=Feed_service.Connect(struct
 include Rss_backend.Live
 let create ?subscriptions_file ()=Rss_backend.Live.create ?subscriptions_file ()
 let name="rss"
 let label="RSS / Atom"
 let is_source=Rss_backend.is_source
 let source_of_url=Rss_backend.source_of_url
 let url_of_source=Rss_backend.url_of_source
 let get=Subscription.get
 let set=Subscription.set
 let refresh t url=list t url ~after:None
 let archive t url id=let a,files=archive t url id in
   {C.message=a.message;attachments=Rss_backend.Decode.attachments a;raw=None;files}
end)
module JSON=Feed_service.Connect(struct
 include Jsonfeed_backend.Live
 let name="jsonfeed"
 let label="JSON Feed"
 let is_source=Jsonfeed_backend.is_source
 let source_of_url=Jsonfeed_backend.source_of_url
 let url_of_source=Jsonfeed_backend.url_of_source
 let followed=Subscription.followed
 let get=Subscription.get
 let set=Subscription.set
end)
type t={factories:S.factory list;discourse:Discourse_backend.config list}
let field=Reader.Settings.field
let string_opt=function `String s when s<>""->Some s|_->None
let create ~state_dir settings =
 let sites=match field "sites" (field "discourse" settings.Reader.Settings.backend_settings) with
 |`Null->[]|`List xs->List.map(fun j ->
   let site=Reader.Settings.string(field "site" j) in
   match string_opt(field "credentials_file" j)with
   |None->Discourse_backend.config site
   |Some path->
     if Filename.is_relative path then invalid_arg "Use an absolute Discourse credentials file";
     let auth=try Yojson.Safe.from_file path with _->invalid_arg "Cannot read Discourse credentials JSON" in
     Discourse_backend.config ~api_key:(Reader.Settings.string(field "api_key"auth))
       ~client_id:(Reader.Settings.string(field "client_id"auth)) site) xs
 |_->invalid_arg "discourse.sites must be an array" in
 {factories=[RSS.create ~subscriptions_file:(Filename.concat state_dir "rss-subscriptions.json") ();
   JSON.create ~subscriptions_file:(Filename.concat state_dir "jsonfeed-subscriptions.json") ();
   Discourse_service.create sites];discourse=sites}
let supported source=Rss_backend.is_source source || Jsonfeed_backend.is_source source || Discourse_backend.is_source source
let validate settings = List.iter(fun (g:Reader.Settings.group)->
 if Rss_backend.is_source g.source then (if Rss_backend.url_of_source g.source<>g.group then invalid_arg "RSS group must equal its URL")
 else if Jsonfeed_backend.is_source g.source then (if Jsonfeed_backend.url_of_source g.source<>g.group then invalid_arg "JSON Feed group must equal its URL")
 else if Discourse_backend.is_source g.source then (ignore(Discourse_backend.site_of_source g.source);ignore(Discourse_backend.group_of_string g.group))) settings.Reader.Settings.groups
let read ~source ~id =
 if Rss_backend.is_source source then RSS.Request(Rss_backend.Read(Rss_backend.url_of_source source,id))
 else if Jsonfeed_backend.is_source source then JSON.Request(Rss_backend.Read(Jsonfeed_backend.url_of_source source,id))
 else if Discourse_backend.is_source source then Discourse_service.Discourse(Discourse_backend.site_of_source source,Discourse_backend.Thread id)
 else invalid_arg "This backend is not connected to the web frontend yet"
let discover t = [RSS.Followed;JSON.Followed],List.map(fun c ->
 {Reader.Settings.name=c.Discourse_backend.site^" · 通知";source=Discourse_backend.source_of_site c.site;group="notifications"})
 (List.filter(fun c -> c.Discourse_backend.api_key<>"")t.discourse)
