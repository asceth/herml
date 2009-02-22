-module(herml_htmlizer).

-export([render/1, render/2]).

-define(RESERVED_TAG_ATTRS, [tag_name, singleton]).

-define(DOCTYPE_TRANSITIONAL, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n").
-define(DOCTYPE_STRICT, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n").
-define(DOCTYPE_HTML11, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n").
-define(DOCTYPE_XML_START, "<?xml version='1.0' encoding='").
-define(DOCTYPE_XML_END, "' ?>\n").
-define(DOCTYPE_XML, ?DOCTYPE_XML_START ++ "utf-8" ++ ?DOCTYPE_XML_END).
-define(DOCTYPE_FRAMESET, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n").

render(Template) ->
  render(Template, []).

render(Template, Env) ->
  render(Template, Env, []).

%% Internal functions
render([{_, {iter, Match, {var_ref, List}}, Subtemplate}|T], Env, Accum) ->
  Result = lists:map(fun(Item) -> unindent(render(Subtemplate, iteration_env(Match, Item, Env), [])) end, lookup_var(List, Env)),
  render(T, Env, [Result|Accum]);

render([{Depth, {tag_decl, Attrs}, []}|T], Env, Accum) ->
  CloseTag = case detect_terminator(Attrs) of
    ">" ->
      render_inline_end_tag(Attrs);
    _ ->
      "\n"
  end,
  render(T, Env, [render_inline_tag(Depth, Attrs, detect_terminator(Attrs), Env) ++ CloseTag|Accum]);

render([{Depth, {tag_decl, Attrs}, Children}|T], Env, Accum) ->
  B1 = render_tag(Depth, Attrs, ">", Env),
  B2 = B1 ++ render(Children, Env),
  render(T, Env, [B2 ++ render_end_tag(Depth, Attrs)|Accum]);

render([{Depth, {var_ref, VarName}, []}|T], Env, Accum) ->
  render(T, Env, [create_whitespace(Depth) ++ lookup_var(VarName, Env) ++ "\n"|Accum]);

render([{_, {var_ref, VarName}, Children}|T], Env, Accum) ->
  render(T, Env, [lookup_var(VarName, Env) ++ render(Children, Env) | Accum]);

render([{Depth, {fun_call, Module, Fun, Args}, Children}|T], Env, Accum) ->
  Result = create_whitespace(Depth) ++ invoke_fun(Module, Fun, Args, Env) ++ "\n",
  render(T, Env, [Result ++ render(Children, Env) | Accum]);

render([{Depth, {fun_call_env, Module, Fun, Args}, Children}|T], Env, Accum) ->
  {R, NewEnv} = invoke_fun_env(Module, Fun, Args, Env),
  Result = create_whitespace(Depth) ++ R ++ "\n",
  render(T, Env, [Result ++ render(Children, NewEnv) | Accum]);

render([{Depth, {fun_call, Module, Fun, Args}, Children}|T], Env, Accum) ->
  Result = create_whitespace(Depth) ++ invoke_fun(Module, Fun, Args, Env) + "\n",
  render(T, Env, [Result ++ render(Children, Env) | Accum]);

render([{Depth, {fun_call_env, Module, Fun, Args}, Children}|T], Env, Accum) ->
  {R, NewEnv} = invoke_fun_env(Module, Fun, Args, Env),
  Result = create_whitespace(Depth) ++ R ++ "\n",
  render(T, Env, [Result ++ render(Children, NewEnv) | Accum]);

render([{_, {doctype, "Transitional", _}, []}|T], Env, Accum) ->
  render(T, Env, [?DOCTYPE_TRANSITIONAL|Accum]);

render([{_, {doctype, "Strict", _}, []}|T], Env, Accum) ->
  render(T, Env, [?DOCTYPE_STRICT|Accum]);

render([{_, {doctype, "1.1", _}, []}|T], Env, Accum) ->
  render(T, Env, [?DOCTYPE_HTML11|Accum]);

render([{_, {doctype, "XML", Encoding}, []}|T], Env, Accum) when is_list(Encoding) andalso Encoding /= [] ->
  render(T, Env, lists:reverse(?DOCTYPE_XML_START ++ Encoding ++ ?DOCTYPE_XML_END) ++ Accum);

render([{_, {doctype, "XML", []}, []}|T], Env, Accum) ->
  render(T, Env, [?DOCTYPE_XML|Accum]);

render([{_, {doctype, "Frameset", _}, []}|T], Env, Accum) ->
  render(T, Env, [?DOCTYPE_FRAMESET|Accum]);

render([{_, Text, []}|T], Env, Accum) ->
  render(T, Env, [render_text(Text) ++ "\n"|Accum]);

render([{_, Text, Children}|T], Env, Accum) ->
  render(T, Env, [render_text(Text) ++ render(Children, Env)|Accum]);

render([], _Env, Accum) ->
  lists:reverse(Accum).

render_text({text, _, Text}) ->
  Text.

render_tag(Depth, Attrs, Terminator, Env) ->
  create_whitespace(Depth) ++ "<" ++
    proplists:get_value(tag_name, Attrs) ++
    render_attrs(Attrs, Env) ++
    Terminator ++ "\n".

render_inline_tag(Depth, Attrs, Terminator, Env) ->
  create_whitespace(Depth) ++ "<" ++
    proplists:get_value(tag_name, Attrs) ++
    render_attrs(Attrs, Env) ++
    Terminator.

render_end_tag(Depth, Attrs) ->
  create_whitespace(Depth) ++ "</" ++ proplists:get_value(tag_name, Attrs) ++ ">\n".

render_inline_end_tag(Attrs) ->
  "</" ++ proplists:get_value(tag_name, Attrs) ++ ">\n".

render_attrs(Attrs, Env) ->
  lists:foldl(fun(Attr, Accum) ->
                render_attr(Attr, Env, Accum) end, "",
              lists:sort(consolidate_classes(Attrs))).

create_whitespace(Depth) ->
  create_whitespace(Depth, []).

create_whitespace(0, Accum) ->
  lists:flatten(Accum);
create_whitespace(Depth, Accum) ->
  create_whitespace(Depth - 1, ["  "|Accum]).

render_attr({fun_call, Module, Fun, Args}, Env, Accum) ->
  render_attrs(invoke_fun(Module, Fun, Args, Env), Env) ++ Accum;

render_attr({fun_call_env, Module, Fun, Args}, Env, Accum) ->
  {R, NewEnv} = invoke_fun_env(Module, Fun, Args, Env),
  render_attrs(R, NewEnv) ++ Accum;

render_attr({Name, {var_ref, VarName}}, Env, Accum) ->
  Accum ++ " " ++ atom_to_list(Name) ++ "=\"" ++ lookup_var(VarName, Env) ++ "\"";
render_attr({Name, Value}, _Env, Accum) ->
  case lists:member(Name, ?RESERVED_TAG_ATTRS) of
    true ->
      Accum;
    false ->
      Accum ++ " " ++ atom_to_list(Name) ++ "=\"" ++ Value ++ "\""
  end.

invoke_fun(Module, Fun, Args, Env) ->
  FinalArgs = resolve_args(Args, Env),
  apply(Module, Fun, FinalArgs).

invoke_fun_env(Module, Fun, Args, Env) ->
  FinalArgs = resolve_args(Args, Env) ++ [Env],
  apply(Module, Fun, FinalArgs).

resolve_args(Args, Env) ->
  resolve_args(Args, Env, []).

resolve_args([{Type, Value}|T], Env, Accum) when Type =:= string;
                                                 Type =:= number ->
  resolve_args(T, Env, [Value | Accum]);
resolve_args([{var_ref, VarName}|T], Env, Accum) ->
  resolve_args(T, Env, [raw_lookup_var(VarName, Env)|Accum]);
resolve_args([{fun_call, Module, Fun, Args}|T], Env, Accum) ->
  Result = invoke_fun(Module, Fun, Args, Env),
  resolve_args(T, Env, [Result|Accum]);
resolve_args([{fun_call_env, Module, Fun, Args}|T], Env, Accum) ->
  Result = invoke_fun_env(Module, Fun, Args, Env),
  resolve_args(T, Env, [Result|Accum]);
resolve_args([], _Env, Accum) ->
  lists:reverse(Accum).

lookup_var(VarName, Env) ->
  format(proplists:get_value(VarName, Env, ""), Env).
%% needed for function calls, must return raw format of variable
raw_lookup_var(VarName, Env) ->
  proplists:get_value(VarName, Env, "").

format(V, Env) when is_function(V) ->
  VR = V(Env),
  format(VR, Env);
format(V, _Env) when is_integer(V) ->
  integer_to_list(V);
format(V, _Env) when is_list(V) ->
  V;
format(V, _Env) when is_integer(V) ->
  integer_to_list(V);
format(V, _Env) when is_binary(V) ->
  binary_to_list(V);
format(V, _Env) when is_atom(V) ->
  atom_to_list(V);
format(V, _Env) ->
  lists:flatten(io_lib:format("~p", V)).

detect_terminator(Attrs) ->
  case proplists:get_value(singleton, Attrs, false) of
    true ->
      " />";
    false ->
      ">"
  end.

consolidate_classes(Attrs) ->
  case proplists:is_defined(class, Attrs) of
    true ->
      Classes = proplists:get_all_values(class, Attrs),
      NewValue = string:join(Classes, " "),
      [{class, NewValue}|proplists:delete(class, Attrs)];
    _ -> Attrs
  end.

iteration_env({tuple, Matches}, Item, Env) ->
  iteration_env_list(Matches, tuple_to_list(Item), Env);
iteration_env({list, Matches}, Item, Env) ->
  iteration_env_list(Matches, Item, Env);
iteration_env(ignore, _Item, Env) ->
  Env;
iteration_env({var_ref, Name}, Item, Env) ->
  [{Name, Item}|Env];
iteration_env(_,_,Env) -> Env.

iteration_env_list([Match|Matches], [Item|Items], Env) ->
  iteration_env_list(Matches, Items, iteration_env(Match, Item, Env));
iteration_env_list(Matches, [], _Env) when is_list(Matches) andalso length(Matches) =/= 0 ->
  throw(bad_match);
iteration_env_list([], [], Env) ->
  Env;
iteration_env_list([], _, _Env) ->
  throw(bad_match).

unindent(List) ->
  Flat = lists:flatten(List),
  Split = string:tokens(Flat, "\n"),
  Stripped = lists:map(fun strip_leading_indent/1, Split),
  string:join(Stripped, "\n") ++ "\n".

strip_leading_indent([32,32|T]) -> T;
strip_leading_indent(String) -> String.
