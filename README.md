eroutes
======

[![Build Status](https://travis-ci.org/koluch/eroutes.png?branch=master)](https://travis-ci.org/koluch/eroutes)

Tool for handle url-routes in web applications

For example, you have following routes:
```erlang
[
  {posts,   ["posts"],                {blog_controller, all_posts, [req_method,req_body]}},
  {post,    ["posts",number],         {blog_controller, show_post, [number]}},
  {comment, ["posts",p,"comments",c], {blog_controller, show_comment, [p,c]}},
  {page404, ["*"],                    {blog_controller, show_404, []}}
].
```

You could compile it to erlang module, directly in memory:
```erlang
eroutes:file_to_memory("routes.test", routes_module_name)
```

Than, you could use module ```routes_module_name``` as follows:

```erlang
routes_module_name:handle("/posts/42/"); %% blog_controller:show_post(Number) would been called
routes_module_name:handle("/posts/42/", RequestObject); %% controller:show_post(Number, RequestObject) would been called
routes_module_name:create(post, [{number,42}]); %% function returns "/posts/42"
```

Also, it's possible to compile routes to erlang module file:
```erlang
eroutes:file_to_file("routes.test", routes_module_name, "routes_module_name.erl")
```

will result in file ```routes_module_name.erl``` with content:

```erlang
-module(routes_module_name).
-export([handle/2,create/2]).

%% ....

handle(["posts"], Args) -> 
    Req_method = proplists:get_value(method, Request),
    Req_body = proplists:get_value(body, Request),
    erlang:apply(blog_controller,all_posts, [Req_method, Req_body|Args]);

handle(["posts",Number], Args) -> 
    erlang:apply(blog_controller, show_post, [Number|Args]);

%% ..... 

create(posts, Params) -> [$/|string:join(["posts"], "/")];
create(post, Params) -> [$/|string:join(["posts",check_type(proplists:get_value(number,Params))], "/")];

%% .....
```
