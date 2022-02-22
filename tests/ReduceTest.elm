module ReduceTest exposing (..)

import Expect exposing (Expectation)
import Parser.Expr exposing (..)
import Parser.Expression as Expression
import Parser.Token as PT exposing (Meta, Token(..))
import Test exposing (..)


{-| fake meta
-}
fm =
    { begin = 0, index = 0, end = 0 }


suite : Test
suite =
    Test.only <|
        describe "applyRule"
            [ test "Rule 1" <|
                \_ ->
                    [ T (S "foo" fm) ]
                        |> Expression.applyRule
                        |> Expect.equal (Just (E (Text "foo" fm)))
            , test "Rule 2" <|
                \_ ->
                    [ T (BS fm), T (S "foo" fm), EL [ Text "bar" fm ] ]
                        |> Expression.applyRule
                        |> Expect.equal (Just (E (Expr "foo" [ Text "bar" fm ] fm)))
            , test "Rule 3" <|
                \_ ->
                    [ T (LB fm), E (Text "foo" fm), T (RB fm) ]
                        |> Expression.applyRule
                        |> Expect.equal (Just (EL [ Text "foo" fm ]))
            , test "Rule 4" <|
                \_ ->
                    [ EL [ Text "foo" fm ], EL [ Text "bar" fm ] ]
                        |> Expression.applyRule
                        |> Expect.equal (Just (EL [ Text "foo" fm, Text "bar" fm ]))
            ]
