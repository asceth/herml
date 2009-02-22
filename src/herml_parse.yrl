Nonterminals
tag_decl tag_stem id_attr class_attr attr_list attrs attr name
var_ref fun_call shortcuts
class_list iter_item iter iter_list template_stmt
doctype_name param param_list
attr_name attr_name_list function_call
fun_param_list fun_params fun_param.

Terminals
tag_start class_start id_start number
lcurly rcurly lbrace rbrace lparen rparen
at comma chr colon slash
dash lt space bang underscore string.

Rootsymbol template_stmt.

template_stmt -> tag_decl : '$1'.
template_stmt -> iter : '$1'.
template_stmt -> fun_call : '$1'.

iter -> dash space lbrace iter_item rbrace space lt dash space var_ref : {iter, '$4', '$10'}.

iter_item -> underscore : ignore.
iter_item -> var_ref : '$1'.
iter_item -> lcurly iter_list rcurly: {tuple, '$2'}.
iter_item -> lbrace iter_list rbrace: {list, '$2'}.

iter_list -> iter_item : ['$1'].
iter_list -> iter_item comma space iter_list : ['$1'|'$4'].
iter_list -> iter_item comma iter_list : ['$1'|'$3'].

var_ref -> at name : {var_ref, unwrap('$2')}.

param -> var_ref : '$1'.
param -> fun_call : '$1'.
param -> name : {name, name_to_atom('$1')}.
param -> string : unwrap_param('$1').
param -> number : unwrap_param('$1').

param_list -> param : ['$1'].
param_list -> param comma param_list : ['$1'|'$3'].
param_list -> param comma space param_list : ['$1'|'$4'].

fun_call -> at name colon name lparen rparen : {fun_call, name_to_atom('$2'), name_to_atom('$4'), []}.
fun_call -> at name colon name lparen param_list rparen : {fun_call, name_to_atom('$2'), name_to_atom('$4'), '$6'}.
fun_call -> at name colon name : {fun_call, name_to_atom('$2'), name_to_atom('$4'), []}.

fun_call -> at at name colon name lparen rparen : {fun_call_env, name_to_atom('$3'), name_to_atom('$5'), []}.
fun_call -> at at name colon name lparen param_list rparen : {fun_call_env, name_to_atom('$3'), name_to_atom('$5'), '$7'}.
fun_call -> at at name colon name : {fun_call_env, name_to_atom('$3'), name_to_atom('$5'), []}.

name -> chr : {name, unwrap('$1')}.

id_attr -> id_start name : unwrap_label_attr(id, '$2').
id_attr -> id_start number : unwrap_label_attr(id, '$2').

class_attr -> class_start name : unwrap_label_attr(class, '$2').

attr_list -> lbrace rbrace : [].
attr_list -> lbrace attrs rbrace : '$2'.
attr_list -> lbrace fun_call rbrace : ['$2'].
attr_list -> lbrace fun_call comma attrs rbrace : ['$2'|'$4'].
attr_list -> lbrace fun_call comma space attrs rbrace : ['$2'|'$5'].

attrs -> attr : ['$1'].
attrs -> attr comma attrs : ['$1'] ++ '$3'.
attrs -> attr comma space attrs : ['$1'] ++ '$4'.

attr_name -> attr_name_list : {name, '$1'}.

attr_name_list -> chr : unwrap('$1').
attr_name_list -> chr attr_name_list : unwrap('$1') ++ '$2'.
attr_name_list -> dash attr_name_list : "-" ++ '$2'.
attr_name_list -> colon attr_name_list : ":" ++ '$2'.

attr -> lcurly attr_name comma string rcurly : {name_to_atom('$2'), unwrap('$4')}.
attr -> lcurly attr_name comma var_ref rcurly : {name_to_atom('$2'), '$4'}.
attr -> lcurly attr_name comma fun_call rcurly : {name_to_atom('$2'), '$4'}.

attr -> lcurly attr_name comma space string rcurly : {name_to_atom('$2'), unwrap('$5')}.
attr -> lcurly attr_name comma space var_ref rcurly : {name_to_atom('$2'), '$5'}.
attr -> lcurly attr_name comma space fun_call rcurly : {name_to_atom('$2'), '$5'}.

doctype_name -> chr : unwrap('$1').
doctype_name -> chr doctype_name : unwrap('$1') ++ '$2'.
doctype_name -> dash : "-".
doctype_name -> dash doctype_name : "-" ++ '$2'.
doctype_name -> class_start : ".".
doctype_name -> class_start doctype_name : "." ++ '$2'.
doctype_name -> number : number_to_list('$1').
doctype_name -> number doctype_name : number_to_list('$1') ++ '$2'.

%% raw variable ref
tag_decl -> var_ref : '$1'.

%% raw function call
tag_decl -> fun_call : '$1'.

%% doctype selector
tag_decl -> bang bang bang : {doctype, "Transitional", []}.
tag_decl -> bang bang bang space : {doctype, "Transitional", []}.
tag_decl -> bang bang bang space doctype_name : {doctype, '$5', []}.
tag_decl -> bang bang bang space doctype_name space doctype_name : {doctype, '$5', '$7'}.

%% singletons or containers
tag_decl -> tag_stem : {tag_decl, '$1'}.
tag_decl -> tag_stem slash : {tag_decl, [{singleton, true}|'$1']}.

%% tag stems (names, ids, classes, attrs)
tag_stem -> tag_start name : [unwrap_label_attr(tag_name, '$2')].
tag_stem -> tag_start name shortcuts : [unwrap_label_attr(tag_name, '$2')|'$3'].
tag_stem -> tag_start name attr_list : lists:append([unwrap_label_attr(tag_name, '$2')], '$3').
tag_stem -> tag_start name shortcuts attr_list : lists:flatten([unwrap_label_attr(tag_name, '$2'), '$3', '$4']).
tag_stem -> shortcuts : [{tag_name, "div"}|'$1'].
tag_stem -> shortcuts attr_list : lists:append([{tag_name, "div"}|'$1'],'$2').

%% id and class shortcuts
shortcuts -> id_attr class_list : ['$1'|'$2'].
shortcuts -> id_attr : ['$1'].
shortcuts -> class_list id_attr : ['$2'|'$1'].
shortcuts -> class_list : '$1'.

class_list -> class_attr class_list : ['$1'|'$2'].
class_list -> class_attr : ['$1'].

Erlang code.
unwrap_label_attr(Label, {_, Value}) ->
  {Label, Value}.

unwrap_param({string, _, Value}) ->
  {string, Value};
unwrap_param({number, _, Value}) ->
  {number, Value}.

unwrap({text, _, Value}) ->
  Value;
unwrap({chr, _, Value}) ->
  Value;
unwrap({string, _, Value}) ->
  Value;
unwrap({space, _, Value}) ->
  Value;
unwrap({name, Value}) ->
  Value;
unwrap({number, _, Value}) ->
  Value;
unwrap({dash, _, Value}) ->
  Value.

number_to_list({number, _, Value}) ->
  integer_to_list(Value).

name_to_atom({name, Value}) ->
  list_to_atom(Value).
