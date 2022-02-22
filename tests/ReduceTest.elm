module ReduceTest exposing (..)

import Expect exposing (Expectation)
import Parser.Expr exposing (..)
import Parser.Expression as PE
import Parser.Token as PT exposing (Meta, Token(..))
import Test exposing (..)


{-| fake meta
-}
fm =
    { begin = 0, index = 0, end = 0 }


suite : Test
suite =
    Test.only <|
        describe "reduce"
            [ describe "applyRule"
                [ test "Rule 1" <|
                    \_ ->
                        [ T (S "foo" fm) ]
                            |> PE.applyRule
                            |> Expect.equal (Just (E (Text "foo" fm)))
                , test "Rule 2" <|
                    \_ ->
                        [ T (BS fm), T (S "foo" fm), EL [ Text "bar" fm ] ]
                            |> PE.applyRule
                            |> Expect.equal (Just (E (Expr "foo" [ Text "bar" fm ] fm)))
                , test "Rule 3" <|
                    \_ ->
                        [ T (LB fm), E (Text "foo" fm), T (RB fm) ]
                            |> PE.applyRule
                            |> Expect.equal (Just (EL [ Text "foo" fm ]))
                , test "Rule 4" <|
                    \_ ->
                        [ EL [ Text "foo" fm ], EL [ Text "bar" fm ] ]
                            |> PE.applyRule
                            |> Expect.equal (Just (EL [ Text "foo" fm, Text "bar" fm ]))
                ]
            , describe "applyRuleR"
                [ test "Rule 1" <|
                    \_ ->
                        [ T (S "foo" fm) ]
                            |> PE.applyRuleList
                            |> Expect.equal [ E (Text "foo" fm) ]
                , test "Rule 2" <|
                    \_ ->
                        [ EL [ Text "bar" fm ], E (Text "foo" fm), T (BS fm) ]
                            |> PE.applyRuleList
                            |> Expect.equal [ E (Expr "foo" [ Text "bar" fm ] fm) ]
                , test "Rule 3" <|
                    \_ ->
                        [ T (RB fm), E (Text "foo" fm), T (LB fm) ]
                            |> PE.applyRuleList
                            |> Expect.equal [ EL [ Text "foo" fm ] ]
                , test "Rule 4" <|
                    \_ ->
                        [ EL [ Text "bar" fm ], EL [ Text "foo" fm ] ]
                            |> PE.applyRuleList
                            |> Expect.equal [ EL [ Text "foo" fm, Text "bar" fm ] ]
                , test "\\foo{bar}" <|
                    \_ ->
                        PT.run "\\foo{bar}"
                            |> PE.prepare
                            |> PE.applyRuleList
                            |> PE.applyRuleList
                            |> Expect.equal [ E (Expr "foo" [ Text "bar" { begin = 5, end = 7, index = 3 } ] { begin = 0, end = 7, index = 0 }) ]
                , test "\\foo{bar}{baz}" <|
                    \_ ->
                        PT.run "\\foo{bar}{baz}"
                            |> PE.prepare
                            |> PE.applyRuleListR
                            |> PE.applyRuleListR
                            |> PE.applyRuleListR
                            |> Expect.equal [ E (Expr "foo" [ Text "bar" { begin = 5, end = 7, index = 3 }, Text "baz" { begin = 10, end = 12, index = 6 } ] { begin = 0, end = 7, index = 0 }) ]
                , test "\\foo{\\bar{baz}}" <|
                    \_ ->
                        PT.run "\\foo{\\bar{baz}}"
                            |> PE.prepare
                            |> PE.applyRuleListR
                            |> PE.applyRuleListR
                            |> PE.applyRuleListR
                            |> PE.applyRuleListR
                            |> Expect.equal [ E (Expr "foo" [ Expr "bar" [ Text "baz" { begin = 10, end = 12, index = 6 } ] { begin = 5, end = 12, index = 3 } ] { begin = 0, end = 12, index = 0 }) ]

                --
                , test "\\foo{bar} (FP)" <|
                    \_ ->
                        PT.run "\\foo{bar}"
                            |> PE.prepare
                            |> PE.applyRuleListFP
                            |> Expect.equal [ E (Expr "foo" [ Text "bar" { begin = 5, end = 7, index = 3 } ] { begin = 0, end = 7, index = 0 }) ]
                , test "\\foo{bar}{baz} (FP)" <|
                    \_ ->
                        PT.run "\\foo{bar}{baz}"
                            |> PE.prepare
                            |> PE.applyRuleListFP
                            |> Expect.equal [ E (Expr "foo" [ Text "bar" { begin = 5, end = 7, index = 3 }, Text "baz" { begin = 10, end = 12, index = 6 } ] { begin = 0, end = 7, index = 0 }) ]
                , test "\\foo{\\bar{baz}} (FP)" <|
                    \_ ->
                        PT.run "\\foo{\\bar{baz}}"
                            |> PE.prepare
                            |> PE.applyRuleListFP
                            |> Expect.equal [ E (Expr "foo" [ Expr "bar" [ Text "baz" { begin = 10, end = 12, index = 6 } ] { begin = 5, end = 12, index = 3 } ] { begin = 0, end = 12, index = 0 }) ]
                ]
            ]
