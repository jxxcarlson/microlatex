module Parser.Expression exposing
    ( State
    , applyRule
    , eval
    , isReducible
    , parse
    , parseToState
    , prepare
    , rulesFP
    , rulesOnce
    , rulesTwice
    )

import Either exposing (Either(..))
import List.Extra
import Parser.Expr exposing (Expr(..), ExprT(..))
import Parser.Helpers as Helpers exposing (Step(..), loop)
import Parser.Match as M exposing (match, reducible)
import Parser.Symbol as Symbol exposing (Symbol(..), convertTokens2)
import Parser.Token as Token exposing (Meta, Token(..), TokenType(..))


prepare : List Token -> List ExprT
prepare exprs =
    let
        f expr =
            case expr of
                T (S t m) ->
                    E (Text t m)

                _ ->
                    expr
    in
    exprs
        |> List.map T
        |> List.map f


applyRule : List ExprT -> Maybe ExprT
applyRule exprs =
    case exprs of
        [ T (S t m) ] ->
            Just (E (Text t m))

        [ T (BS m1), T (S t m2), EL exprList ] ->
            let
                end =
                    case List.head exprList of
                        Nothing ->
                            m2.end

                        Just expr ->
                            expr |> getMeta |> .end

                m =
                    { begin = m1.begin, end = end, index = m1.index }
            in
            Just (E (Expr t exprList m))

        [ T (LB _), E e, T (RB _) ] ->
            Just (EL [ e ])

        [ EL exprList1, EL exprList2 ] ->
            Just (EL (exprList1 ++ exprList2))

        _ ->
            Nothing


rulesTwice : List ExprT -> List ExprT
rulesTwice exprs =
    case rulesOnce exprs of
        a :: rest ->
            a :: rulesOnce rest

        _ ->
            exprs


rulesFP : List ExprT -> List ExprT
rulesFP exprs =
    let
        exprs2 =
            rulesTwice exprs
    in
    if exprs == exprs2 then
        exprs

    else
        rulesFP (rulesTwice exprs2)


rulesOnce : List ExprT -> List ExprT
rulesOnce exprs =
    case exprs of
        (T (S t m)) :: rest ->
            E (Text t m) :: rest

        (EL exprList) :: (E (Text t m2)) :: (T (BS m1)) :: rest ->
            let
                end =
                    case List.head exprList of
                        Nothing ->
                            m2.end

                        Just expr ->
                            expr |> getMeta |> .end

                m =
                    { begin = m1.begin, end = end, index = m1.index }
            in
            E (Expr t exprList m) :: rest

        (T (RB _)) :: (E e) :: (T (LB _)) :: rest ->
            EL [ e ] :: rest

        (EL exprList2) :: (EL exprList1) :: rest ->
            EL (exprList1 ++ exprList2) :: rest

        _ ->
            exprs


getMeta : Expr -> Meta
getMeta expr =
    case expr of
        Expr _ _ m ->
            m

        Text _ m ->
            m

        Verbatim _ _ m ->
            m

        Error _ ->
            { begin = 0, end = 0, index = -1 }



-- TYPES


type alias State =
    { step : Int
    , tokens : List Token
    , numberOfTokens : Int
    , tokenIndex : Int
    , committed : List Expr
    , stack : List Token
    , messages : List String
    , lineNumber : Int
    }



-- STATE FOR THE PARSER


initWithTokens : Int -> List Token -> State
initWithTokens lineNumber tokens =
    { step = 0
    , tokens = List.reverse tokens
    , numberOfTokens = List.length tokens
    , tokenIndex = 0
    , committed = []
    , stack = []
    , messages = []
    , lineNumber = lineNumber
    }


init : String -> State
init str =
    let
        tokens =
            Token.run str |> List.reverse
    in
    { step = 0
    , tokens = tokens
    , numberOfTokens = List.length tokens
    , tokenIndex = 0
    , committed = []
    , stack = []
    , messages = []
    , lineNumber = 0
    }



-- Exposed functions


parse : Int -> String -> List Expr
parse lineNumber str =
    str
        |> Token.run
        |> initWithTokens lineNumber
        |> run
        |> .committed


parseToState : Int -> String -> State
parseToState lineNumber str =
    str
        |> Token.run
        |> initWithTokens lineNumber
        |> run



-- PARSER
-- PARSER


run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })


nextStep : State -> Step State State
nextStep state =
    case List.Extra.getAt state.tokenIndex state.tokens of
        Nothing ->
            if List.isEmpty state.stack then
                Done state

            else
                -- the stack is not empty, so we need to handle the parse error
                recoverFromError state

        Just token ->
            pushToken token { state | tokenIndex = state.tokenIndex + 1 }
                |> reduceState
                |> (\st -> { st | step = st.step + 1 })
                |> Loop



-- PUSH


pushToken : Token -> State -> State
pushToken token state =
    case token of
        S _ _ ->
            pushOrCommit token state

        W _ _ ->
            pushOrCommit token state

        MathToken _ ->
            pushOnStack token state

        CodeToken _ ->
            pushOnStack token state

        BS _ ->
            pushOnStack token state

        LB _ ->
            pushOnStack token state

        RB _ ->
            pushOnStack token state

        TokenError _ _ ->
            pushOnStack token state


pushOnStack : Token -> State -> State
pushOnStack token state =
    { state | stack = token :: state.stack }


pushOrCommit : Token -> State -> State
pushOrCommit token state =
    if List.isEmpty state.stack then
        commit token state

    else
        push token state


commit : Token -> State -> State
commit token state =
    case exprOfToken token of
        Nothing ->
            state

        Just expr ->
            { state | committed = expr :: state.committed }


exprOfToken : Token -> Maybe Expr
exprOfToken token =
    case token of
        S str loc ->
            Just (Text str loc)

        W str loc ->
            Just (Text str loc)

        _ ->
            Nothing


push : Token -> State -> State
push token state =
    { state | stack = token :: state.stack }



-- REDUCE


reduceState : State -> State
reduceState state =
    let
        peek =
            List.Extra.getAt state.tokenIndex state.tokens

        isStringToken maybeTok =
            case maybeTok of
                Just (S _ _) ->
                    True

                _ ->
                    False

        reducible_ =
            (isReducible state.stack
                && isStringToken peek
            )
                || ((List.head state.stack |> Maybe.map Token.type_)
                        == Just TMath
                        && (List.head (List.drop (List.length state.stack - 1) state.stack) |> Maybe.map Token.type_)
                        == Just TMath
                   )
    in
    if state.tokenIndex >= state.numberOfTokens || reducible_ then
        let
            symbols =
                state.stack |> Symbol.convertTokens |> List.reverse
        in
        case List.head symbols of
            Just B ->
                case eval state.lineNumber (state.stack |> List.reverse) of
                    (Expr "invisible" [ Text message _ ] _) :: rest ->
                        { state | stack = [], committed = rest ++ state.committed, messages = Helpers.prependMessage state.lineNumber message state.messages }

                    whatever ->
                        { state | stack = [], committed = whatever ++ state.committed }

            -- { state | stack = [], committed = eval (state.stack |> List.reverse) ++ state.committed }
            Just M ->
                { state | stack = [], committed = Verbatim "math" (Debug.log "MATHH" (Token.toString <| List.reverse state.stack)) { begin = 0, end = 0, index = 0 } :: state.committed }

            Just C ->
                { state | stack = [], committed = Verbatim "code" (Token.toString <| unbracket <| List.reverse state.stack) { begin = 0, end = 0, index = 0 } :: state.committed }

            _ ->
                state

    else
        state


{-| remove first and last token
-}
unbracket : List a -> List a
unbracket list =
    List.drop 1 (List.take (List.length list - 1) list)


{-| areBracketed tokns == True iff tokens are derived from "[ ... ]"
-}
areBracketed : List Token -> Bool
areBracketed tokens =
    List.map Token.type_ (List.take 1 tokens)
        == [ TLB ]
        && List.map Token.type_ (List.take 1 (List.reverse tokens))
        == [ TRB ]


eval : Int -> List Token -> List Expr
eval lineNumber tokens =
    case tokens of
        -- The reversed token list is of the form [LB name EXPRS RB], so return [Expr name (evalList EXPRS)]
        (S t m2) :: rest ->
            Text t m2 :: evalList lineNumber rest

        (BS m1) :: (S name m2) :: rest ->
            [ Expr name (evalList lineNumber rest) m1 ]

        _ ->
            [ errorMessageInvisible lineNumber "missing macro name", errorMessage <| "??" ]


evalList : Int -> List Token -> List Expr
evalList lineNumber tokens =
    case List.head tokens of
        Just token ->
            case Token.type_ token of
                TLB ->
                    case M.match (Symbol.convertTokens2 tokens) of
                        Nothing ->
                            [ errorMessageInvisible lineNumber "Error on match", Text "error on match" dummyLoc ]

                        Just k ->
                            let
                                ( a, b ) =
                                    M.splitAt (k + 1) tokens

                                aa =
                                    a |> List.take (List.length a - 1) |> List.drop 1
                            in
                            eval lineNumber aa ++ evalList lineNumber b

                _ ->
                    case exprOfToken token of
                        Just expr ->
                            expr :: evalList lineNumber (List.drop 1 tokens)

                        Nothing ->
                            [ errorMessageInvisible lineNumber "Error converting token", Text "error converting Token" dummyLoc ]

        _ ->
            []


errorMessageInvisible : Int -> String -> Expr
errorMessageInvisible lineNumber message =
    let
        m =
            -- message ++ " (line " ++ String.fromInt lineNumber ++ ")"
            message

        --  ++ " (line " ++ String.fromInt lineNumber ++ ")"
    in
    Expr "invisible" [ Text m dummyLoc ] dummyLoc


errorMessage : String -> Expr
errorMessage message =
    Expr "red" [ Text message dummyLoc ] dummyLoc


errorMessageBold : String -> Expr
errorMessageBold message =
    Expr "bold" [ Expr "red" [ Text message dummyLoc ] dummyLoc ] dummyLoc


errorMessage2 : String -> Expr
errorMessage2 message =
    Expr "blue" [ Text message dummyLoc ] dummyLoc


colorRed : Expr -> Expr
colorRed expr =
    Expr "red" [ expr ] dummyLoc


colorFirstElementRed : State -> State
colorFirstElementRed state =
    let
        ( a, b ) =
            M.splitAt 1 state.committed

        newCommitted =
            List.map colorRed a ++ b
    in
    { state | committed = newCommitted }


addErrorMessage : String -> State -> State
addErrorMessage message state =
    let
        committed =
            errorMessage message :: state.committed
    in
    { state | committed = committed }


isReducible : List Token -> Bool
isReducible tokens =
    let
        preliminary =
            tokens |> List.reverse |> Symbol.convertTokens2 |> List.filter (\sym -> sym /= O)
    in
    if preliminary == [] then
        False

    else
        preliminary |> M.reducible


recoverFromError : State -> Step State State
recoverFromError state =
    case List.reverse state.stack of
        -- brackets with no intervening text
        (LB _) :: (RB meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage "[?]" :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , messages = Helpers.prependMessage state.lineNumber "Brackets need to enclose something" state.messages
                }

        -- consecutive left brackets
        (LB _) :: (LB meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage "[" :: state.committed
                    , stack = []
                    , tokenIndex = meta.index
                    , messages = Helpers.prependMessage state.lineNumber "You have consecutive left brackets" state.messages
                }

        -- missing right bracket // OK
        (LB _) :: (S fName meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage (errorSuffix rest) :: errorMessage2 ("[" ++ fName) :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , messages = Helpers.prependMessage state.lineNumber "Missing right bracket" state.messages
                }

        -- space after left bracket // OK
        (LB _) :: (W " " meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage "[ - can't have space after the bracket " :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , messages = Helpers.prependMessage state.lineNumber "Can't have space after left bracket - try [something ..." state.messages
                }

        -- left bracket with nothing after it.  // OK
        (LB meta) :: [] ->
            Done
                { state
                    | committed = errorMessage "[...?" :: state.committed
                    , stack = []
                    , tokenIndex = 0
                    , numberOfTokens = 0
                    , messages = Helpers.prependMessage state.lineNumber "That left bracket needs something after it" state.messages
                }

        -- extra right bracket
        (RB meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage " extra ]?" :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , messages = Helpers.prependMessage state.lineNumber "Extra right bracket(s)" state.messages
                }

        -- dollar sign with no closing dollar sign
        (MathToken meta) :: rest ->
            let
                content =
                    Token.toString rest

                message =
                    if content == "" then
                        "$?$"

                    else
                        "$ "
            in
            Loop
                { state
                    | committed = errorMessage message :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , numberOfTokens = 0
                    , messages = Helpers.prependMessage state.lineNumber "opening dollar sign needs to be matched with a closing one" state.messages
                }

        -- backtick with no closing backtick
        (CodeToken meta) :: rest ->
            let
                content =
                    Token.toString rest

                message =
                    if content == "" then
                        "`?`"

                    else
                        "` "
            in
            Loop
                { state
                    | committed = errorMessageBold message :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , numberOfTokens = 0
                    , messages = Helpers.prependMessage state.lineNumber "opening backtick needs to be matched with a closing one" state.messages
                }

        _ ->
            recoverFromError1 state


errorSuffix rest =
    case rest of
        [] ->
            "]?"

        (W _ _) :: [] ->
            "]?"

        _ ->
            ""


recoverFromError1 : State -> Step State State
recoverFromError1 state =
    let
        k =
            Symbol.balance <| Symbol.convertTokens (List.reverse state.stack)

        newStack =
            List.repeat k (RB dummyLoc) ++ state.stack

        newSymbols =
            Symbol.convertTokens (List.reverse newStack)

        reducible =
            M.reducible newSymbols
    in
    if reducible then
        Done <|
            addErrorMessage " ]? " <|
                reduceState <|
                    { state
                        | stack = newStack
                        , tokenIndex = 0
                        , numberOfTokens = List.length newStack
                        , committed = errorMessage "[" :: state.committed
                        , messages = Helpers.prependMessage state.lineNumber ("Unmatched brackets: added " ++ String.fromInt k ++ " right brackets") state.messages
                    }

    else
        Done
            { state
                | committed =
                    bracketError k
                        -- :: Expr "blue" [ Text (" " ++ Token.toString state.tokens) dummyLoc ] dummyLoc
                        :: state.committed
                , messages = Helpers.prependMessage state.lineNumber (bracketErrorAsString k) state.messages
            }


bracketError : Int -> Expr
bracketError k =
    if k < 0 then
        let
            brackets =
                List.repeat -k "]" |> String.join ""
        in
        errorMessage <| " " ++ brackets ++ " << Too many right brackets (" ++ String.fromInt -k ++ ")"

    else
        let
            brackets =
                List.repeat k "[" |> String.join ""
        in
        errorMessage <| " " ++ brackets ++ " << Too many left brackets (" ++ String.fromInt k ++ ")"


bracketErrorAsString : Int -> String
bracketErrorAsString k =
    if k < 0 then
        "Too many right brackets (" ++ String.fromInt -k ++ ")"

    else
        "Too many left brackets (" ++ String.fromInt k ++ ")"



-- HELPERS


dummyTokenIndex =
    0


dummyLoc =
    { begin = 0, end = 0, index = dummyTokenIndex }



-- LOOP
