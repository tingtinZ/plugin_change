-module(plugin_change_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, plugin_change).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 change_to_erlang"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),

         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.txt\$"),
         CompileFun = fun(Source, _Opts1) ->
             ModName = filename:basename(Source, ".txt"),
             TargetName = ModName ++ ".erl",
             generate(Source, filename:join(SourceDir, TargetName))
                      end,
         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],

    {ok, State}.

%%    Fun =
%%        fun(Line, [Start, End]) ->
%%            Line1 = string:tokens(Line, [$\s]),
%%            rebar_api:info("Line1 ~p~n", [Line1]),
%%            case Line1 of
%%                ["for","(",_,_,"=",Num1,_,_,"<=",Num2,_,_,_] ->
%%                    Start1 = Num1,
%%                    End1 = Num2,
%%                    [Start1,End1];
%%                ["for","(",_,_,"=",Num1,_,_,"<",Num2,_,_,_] ->
%%                    Start1 = Num1,
%%                    End1 = Num2,
%%                    [Start1,End1];
%%                _ ->
%%                    [Start, End]
%%            end
%%        end,
%%    [Starts, Ends] = lists:foldl(Fun, ["",""], Lines),
%%    Str1 = "-module(hello).\n-export([start/0]).\n",
%%    Str2 = "start() ->\n",
%%    Str3 =  "    lists:foreach(fun(I) -> io:format(\"~p~n\",[I]) end, lists:seq("++Starts++","++Ends++")),\n",
%%    Str4 = "ok.",
%%    Strs = lists:concat([Str1, Str2, Str3, Str4]),
%%    file:write_file(Erl, Strs),


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%

generate(Txt, Erl) ->
    Lines = load_txt(Txt),
    rebar_api:info("Lines ~p~n", [Lines]),
    [LoopStart, LoopEnd, Direction] = take_loop(Lines),
    Str1 = "-module(hello).\n-export([start/0]).\n",
    Str2 = "start() ->\n",
    case Direction of
        forward ->
            Str3 =  "    lists:foreach(fun(I) -> io:format(\"~p~n\",[I]) end, lists:seq("++LoopStart++","++LoopEnd++")),\n";
        reverse ->
            Str3 =  "    lists:foreach(fun(I) -> io:format(\"~p~n\",[I]) end, lists:reverse:(lists:seq("++LoopEnd++","++LoopStart++"))),\n";
    end,

    Str4 = "ok.",
    Strs = lists:concat([Str1, Str2, Str3, Str4]),
    ok =  ok = file:write_file(Erl, Strs),
    rebar_api:info("Generated ~s~n", [Erl]).

load_txt(Txt) ->
    {ok, Bin} = file:read_file(Txt),
    String = unicode:characters_to_list(Bin, latin1),
    ReadLines = string:tokens(String, [$\r,$\n]),
    [ string:strip(L) || L <- ReadLines].


take_loop(Lines) ->
    Fun =
        fun(Line, [Start, End, Direction]) ->
            LeftBracket = string:str(Line, "("),
            RightBracket = string:rstr(Line, ")"),
            if
                LeftBracket>0 andalso RightBracket>0 ->
                    Param = string:sub_string(Line, LeftBracket+1, RightBracket-1),
                    Format = Line -- Param,
                    case delete_blank(Format) of
                        "for()" ->
                            [Param1, Param2, Param3] = string:tokens(Format, ";"),
                            StartNum = analyse_param1(Param1),
                            [Operator, EndNum] = analyse_param2(Param2),
                            NewDirection = analyse_param3(Param3),
                            case Operator of
                                "<" ->
                                    NewEndNum = erlang:integer_to_list(string:to_integer(EndNum) - 1);
                                ">" ->
                                    NewEndNum = erlang:integer_to_list(string:to_integer(EndNum) + 1);
                                _ ->
                                    NewEndNum = EndNum
                            end,
                            [StartNum, NewEndNum, NewDirection];
                        _ ->
                            [Start, End, Direction]
                    end;
                true ->
                    [Start, End, Direction]
            end
        end,
    lists:foldl(Fun, ["","",forward], Lines).


delete_blank(Str) ->
    lists:concat(string:tokens(Str, [$\s])).


analyse_param1(Param) ->
    Assign = string:str(Param,"="),
    if
        Assign > 0  ->
            lists:last(string:tokens(Param,"="));
        true ->
            "0"
    end.

analyse_param2(Param) ->
    Param1 = delete_blank(Param),
    Operator1 = string:str(Param1, "<"),
    Operator2 = string:str(Param1, "<="),
    Operator3 = string:str(Param1, ">"),
    Operator4 = string:str(Param1, ">="),
    if
        Operator1 >0 andalso Operator2=:=0 ->
            Num = lists:last(string:tokens(Param1,"<")),
            ["<", Num];
        Operator2 >0 ->
            Num = lists:last(string:tokens(Param1,"<=")),
            ["<=", Num];
        Operator3 >0 andalso Operator4=:=0 ->
            Num = lists:last(string:tokens(Param1,">")),
            [">", Num];
        Operator4 >0 ->
            Num = lists:last(string:tokens(Param1,">=")),
            [">=", Num];
        true ->
            ["","0"]
    end.

analyse_param3(Param) ->
    AutoPlus = string:str(Param, "++"),
    AutoMinus = string:str(Param, "--"),
    if
        AutoPlus>0 ->
            forward;
        AutoMinus>0 ->
            reverse;
        true ->
            forward
    end.

generate(CSV, Erl, Hrl) ->
    Tuples = load_csv(CSV),
    ModName = filename:basename(CSV, ".csv"),
    Module = generate_module(ModName, Tuples),
    HrlCode = generate_hrl(Tuples),
    Formatted = erl_prettypr:format(Module),
    ok = file:write_file(Erl, Formatted),
    ok = file:write_file(Hrl, HrlCode),
    rebar_api:info("Generated ~s~n", [Erl]).

load_csv(SourceFile) ->
    {ok, Bin} = file:read_file(SourceFile),
    csv_to_tuples(unicode:characters_to_list(Bin, latin1)).

csv_to_tuples(String) ->
    Lines = string:tokens(String, [$\r,$\n]),
    [ begin
          [Code, Message, Proto] = string:tokens(Line, ","),
          {list_to_integer(Code), Message, Proto ++ "_pb"}
      end
        || Line <- Lines].


generate_hrl(Tuples) ->
    Fun = fun({Code, Msg, _}, Str) ->
        Msg1 = change_msg(Msg),
        lists:concat([Str, "-define(CMD_",string:to_upper(Msg1),",", Code, ").\n"])
          end,
    lists:foldl(Fun, "", Tuples).




generate_module(Name, Tuples) ->
    %% TODO: Add generated doc comment at the top
    Mod = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(Name)]),
    ExportsList = [
        erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(1))
        || Fun <- [msg_type, msg_code, decoder_for] ],

    Exports = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportsList)]),

    Clauses = generate_msg_type(Tuples) ++
        generate_msg_code(Tuples) ++
        generate_decoder_for(Tuples),

    erl_syntax:form_list([Mod, Exports|Clauses]).

generate_decoder_for(Tuples) ->
    Spec = erl_syntax:text("-spec decoder_for(non_neg_integer()) -> module().\n"),
    Name = erl_syntax:atom(decoder_for),
    Clauses = [
        erl_syntax:clause([erl_syntax:integer(Code)],
            none,
            [erl_syntax:atom(Mod)])
        || {Code, _, Mod} <- Tuples ],
    [ Spec, erl_syntax:function(Name, Clauses) ].

generate_msg_code(Tuples) ->
    Spec = erl_syntax:text("-spec msg_code(atom()) -> non_neg_integer()."),
    Name = erl_syntax:atom(msg_code),
    Clauses = [
        erl_syntax:clause([erl_syntax:atom(Msg)], none, [erl_syntax:integer(Code)])
        || {Code, Msg, _} <- Tuples ],
    [ Spec, erl_syntax:function(Name, Clauses) ].

generate_msg_type(Tuples) ->
    Spec = erl_syntax:text("-spec msg_type(non_neg_integer()) -> atom()."),
    Name = erl_syntax:atom(msg_type),
    Clauses = [
        erl_syntax:clause([erl_syntax:integer(Code)], none, [erl_syntax:atom(Msg)])
        || {Code, Msg, _} <- Tuples ],
    CatchAll = erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(undefined)]),
    [ Spec, erl_syntax:function(Name, Clauses ++ [CatchAll]) ].

change_msg(Msg) ->
    Fun = fun(Chr, Str) ->
        case Chr >= $A andalso Chr =< $Z of
            true ->
                LowChr = char_to_lower(Chr),
                Str ++ [$_, LowChr];
            false ->
                Str ++ [Chr]
        end
          end,
    lists:foldl(Fun, "", Msg).


-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.
%%
%%-spec char_to_upper(char()) -> char().
%%char_to_upper($a) -> $A;
%%char_to_upper($b) -> $B;
%%char_to_upper($c) -> $C;
%%char_to_upper($d) -> $D;
%%char_to_upper($e) -> $E;
%%char_to_upper($f) -> $F;
%%char_to_upper($g) -> $G;
%%char_to_upper($h) -> $H;
%%char_to_upper($i) -> $I;
%%char_to_upper($j) -> $J;
%%char_to_upper($k) -> $K;
%%char_to_upper($l) -> $L;
%%char_to_upper($m) -> $M;
%%char_to_upper($n) -> $N;
%%char_to_upper($o) -> $O;
%%char_to_upper($p) -> $P;
%%char_to_upper($q) -> $Q;
%%char_to_upper($r) -> $R;
%%char_to_upper($s) -> $S;
%%char_to_upper($t) -> $T;
%%char_to_upper($u) -> $U;
%%char_to_upper($v) -> $V;
%%char_to_upper($w) -> $W;
%%char_to_upper($x) -> $X;
%%char_to_upper($y) -> $Y;
%%char_to_upper($z) -> $Z;
%%char_to_upper(Ch) -> Ch.

