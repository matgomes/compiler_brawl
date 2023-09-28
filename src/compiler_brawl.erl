-module(compiler_brawl).

-export([main/1]).

main(Input) ->
    {ok, Content} = file:read_file(Input),
    Json = jsone:decode(Content),
    Expr = maps:get(<<"expression">>, Json),
    eval(Expr, #{}),
    erlang:halt(0).

eval(#{<<"kind">> := <<"Print">>, <<"value">> := Value}, Env) ->
    Data = eval(Value, Env),
    io:format("~s~n", [to_string(Data)]),
    Data;

eval(#{<<"kind">> := <<"Str">>, <<"value">> := Value}, _) ->
    Value;

eval(#{<<"kind">> := <<"Bool">>, <<"value">> := Value}, _) ->
    Value;

eval(#{<<"kind">> := <<"Int">>, <<"value">> := Value}, _) ->
    Value;

eval(#{<<"kind">> := <<"Tuple">>, <<"first">> := First, <<"second">> := Second}, Env) ->
    {tuple, {eval(First, Env), eval(Second, Env)}};

eval(#{<<"kind">> := <<"Let">>, <<"value">> := Value, <<"name">> := Name, <<"next">> := Next}, Env) ->
    EvaluatedValue = eval(Value, Env),
    NameText = maps:get(<<"text">>, Name),
    NewEnv = maps:put(NameText, EvaluatedValue, Env),
    eval(Next, NewEnv);

eval(#{<<"kind">> := <<"Var">>, <<"text">> := Text}, Env) ->
    eval(maps:get(Text, Env), Env);

eval(#{<<"kind">> := <<"Function">>, <<"parameters">> := Params, <<"value">> := Value}, _) ->
    {func, {Params, Value}};

eval(#{<<"kind">> := <<"Call">>, <<"callee">> := Callee, <<"arguments">> := Arguments}, Env) ->
    {func, {Params, Value}} = eval(Callee, Env),

    ParamsArgumentsTuples =
        lists:zipwith(fun(#{<<"text">> := Text}, Args) -> {Text, Args} end, Params, Arguments),
    NewEnv =
        lists:foldl(fun({Param, Arg}, Acc) -> Acc#{Param => eval(Arg, Env)} end,
                    #{},
                    ParamsArgumentsTuples),

    case maps:find(<<"text">>, Callee) of
        {ok, Text} ->
            memoize(fun eval/2, [Value, maps:merge(Env, NewEnv)], {Text, NewEnv});
        error ->
            eval(Value, maps:merge(Env, NewEnv))
    end;

eval(#{<<"kind">> := <<"If">>, <<"condition">> := Condition, <<"then">> := Then, <<"otherwise">> := Otherwise}, Env) ->
    ConditionResult = eval(Condition, Env),
    if ConditionResult -> eval(Then, Env);
       true -> eval(Otherwise, Env)
    end;

eval(#{<<"kind">> := <<"First">>, <<"value">> := Value}, Env) ->
    #{<<"first">> := First} = Value,
    eval(First, Env);

eval(#{<<"kind">> := <<"Second">>, <<"value">> := Value}, Env) ->
    #{<<"second">> := Second} = Value,
    eval(Second, Env);

eval(#{<<"kind">> := <<"Binary">>, <<"lhs">> := Lhs, <<"rhs">> := Rhs, <<"op">> := Op}, Env) ->
    LhsValue = eval(Lhs, Env),
    RhsValue = eval(Rhs, Env),
    case Op of
        <<"Add">> -> sum(LhsValue, RhsValue);
        <<"Sub">> -> LhsValue - RhsValue;
        <<"Mul">> -> LhsValue * RhsValue;
        <<"Div">> -> LhsValue / RhsValue;
        <<"Rem">> -> LhsValue rem RhsValue;
        <<"Eq">>  -> LhsValue == RhsValue;
        <<"Neq">> -> LhsValue /= RhsValue;
        <<"Lt">>  -> LhsValue < RhsValue;
        <<"Gt">>  -> LhsValue > RhsValue;
        <<"Lte">> -> LhsValue =< RhsValue;
        <<"Gte">> -> LhsValue >= RhsValue;
        <<"And">> -> LhsValue andalso RhsValue;
        <<"Or">>  -> LhsValue orelse RhsValue
    end;

eval(V, _) -> V.

sum(A, B) when is_number(A) and is_number(B) -> A + B;

sum(A, B) ->
    String = lists:concat([to_string(A), to_string(B)]),
    list_to_binary(String).

to_string(Term) when is_binary(Term) -> binary_to_list(Term);
to_string(Term) when is_number(Term) -> integer_to_list(trunc(Term));
to_string({func, _}) -> "<#closure>";

to_string({tuple, {First, Second}}) ->
    lists:concat(["(", to_string(First), ", ", to_string(Second), ")"]);

to_string(Term) -> Term.

memoize(Func, Args, Key) ->
    case get(Key) of
        undefined ->
            Result = apply(Func, Args),
            put(Key, Result),
            Result;
        Result ->
            Result
    end.
