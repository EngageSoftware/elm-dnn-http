module Tests exposing (suite)

import Dict
import Engage.Http
import Engage.Localization
import Expect
import Http exposing (header)
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Engage HTTP"
        [ configDecoderTests
        , nullDecoderTests
        , serverErrorDecoderTests
        , getErrorMessageTests
        ]


configDecoderTests : Test
configDecoderTests =
    describe "configDecoder"
        [ test "Can decode baseUrl" <|
            \_ ->
                """
                {
                    "baseUrl": "/API/Tests",
                    "headers": {}
                }
            """
                    |> Decode.decodeString Engage.Http.configDecoder
                    |> Result.map .baseUrl
                    |> Expect.equal (Ok "/API/Tests")
        , test "Can decode empty headers" <|
            \_ ->
                """
                { "baseUrl": "/API/Tests", "headers": {} }
            """
                    |> Decode.decodeString Engage.Http.configDecoder
                    |> Result.map .headers
                    |> Expect.equal (Ok [])
        , test "Can decode headers" <|
            \_ ->
                """
                { 
                    "baseUrl": "/API/Tests", 
                    "headers": {
                        "ModuleId": "1",
                        "TabId": "2",
                        "Something-Else": "This is another value"
                    } 
                }
            """
                    |> Decode.decodeString Engage.Http.configDecoder
                    |> Result.map .headers
                    |> Result.toMaybe
                    |> Maybe.withDefault []
                    |> Expect.equalLists [ header "ModuleId" "1", header "TabId" "2", header "Something-Else" "This is another value" ]
        , test "Cannot decode without headers" <|
            \_ ->
                """
                { 
                    "baseUrl": "/API/Tests"
                }
            """
                    |> Decode.decodeString Engage.Http.configDecoder
                    |> Expect.err
        , test "Cannot decode without baseUrl" <|
            \_ ->
                """
                { 
                    "headers": {}
                }
            """
                    |> Decode.decodeString Engage.Http.configDecoder
                    |> Expect.err
        ]


nullDecoderTests : Test
nullDecoderTests =
    describe "nullDecoder"
        [ test "Can decode null" <|
            \_ ->
                "null"
                    |> Decode.decodeString Engage.Http.nullDecoder
                    |> Expect.equal (Ok ())
        , test "Cannot decode invalid JSON" <|
            \_ ->
                "nullish"
                    |> Decode.decodeString Engage.Http.nullDecoder
                    |> Expect.err
        , test "Cannot decode object" <|
            \_ ->
                """ { "value": "val" } """
                    |> Decode.decodeString Engage.Http.nullDecoder
                    |> Expect.err
        , test "Cannot decode array" <|
            \_ ->
                """ [{ "value": "val" }] """
                    |> Decode.decodeString Engage.Http.nullDecoder
                    |> Expect.err
        ]


serverErrorDecoderTests : Test
serverErrorDecoderTests =
    describe "serverErrorDecoder"
        [ test "Can decode ExceptionMessage" <|
            \_ ->
                """
                { "ExceptionMessage": "You didn't supply the right things" }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.empty })
                    |> Expect.equal (Ok "You didn't supply the right things")
        , test "Can decode exceptionMessage" <|
            \_ ->
                """
                { "exceptionMessage": "You didn't supply the right things" }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.empty })
                    |> Expect.equal (Ok "You didn't supply the right things")
        , test "Can decode Message" <|
            \_ ->
                """
                { "Message": "You didn't supply the right things" }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.empty })
                    |> Expect.equal (Ok "You didn't supply the right things")
        , test "Can decode Messages" <|
            \_ ->
                """
                { "Messages": ["You didn't supply the right things", "You should try harder"] }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.empty })
                    |> Expect.equal (Ok "You didn't supply the right things You should try harder ")
        , test "Can decode localized Messages" <|
            \_ ->
                """
                { "Messages": ["You didn't supply the right things", "You should try harder"] }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.fromDict (Dict.fromList [ ( "You should try harder.Error", "You're good enough!" ) ]) })
                    |> Expect.equal (Ok "You didn't supply the right things You're good enough! ")
        , test "Can decode messages" <|
            \_ ->
                """
                { "messages": ["You didn't supply the right things", "You should try harder"] }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.empty })
                    |> Expect.equal (Ok "You didn't supply the right things You should try harder ")
        , test "Can decode localized messages" <|
            \_ ->
                """
                { "messages": ["You didn't supply the right things", "You should try harder"] }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.fromDict (Dict.fromList [ ( "You should try harder.Error", "You're good enough!" ) ]) })
                    |> Expect.equal (Ok "You didn't supply the right things You're good enough! ")
        , test "Can decode ExceptionMessage list" <|
            \_ ->
                """
                { "ExceptionMessage": ["You didn't supply the right things", "You should try harder"] }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.empty })
                    |> Expect.equal (Ok "You didn't supply the right things You should try harder ")
        , test "Can decode localized ExceptionMessage list" <|
            \_ ->
                """
                { "ExceptionMessage": ["You didn't supply the right things", "You should try harder"] }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.fromDict (Dict.fromList [ ( "You should try harder.Error", "You're good enough!" ) ]) })
                    |> Expect.equal (Ok "You didn't supply the right things You're good enough! ")
        , test "Can decode exceptionMessage list" <|
            \_ ->
                """
                { "exceptionMessage": ["You didn't supply the right things", "You should try harder"] }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.empty })
                    |> Expect.equal (Ok "You didn't supply the right things You should try harder ")
        , test "Can decode localized exceptionMessage list" <|
            \_ ->
                """
                { "exceptionMessage": ["You didn't supply the right things", "You should try harder"] }
            """
                    |> Decode.decodeString (Engage.Http.serverErrorDecoder { localization = Engage.Localization.fromDict (Dict.fromList [ ( "You should try harder.Error", "You're good enough!" ) ]) })
                    |> Expect.equal (Ok "You didn't supply the right things You're good enough! ")
        ]


getErrorMessageTests : Test
getErrorMessageTests =
    describe "getErrorMessage"
        [ test "Can get BadUrl message" <|
            \_ ->
                Engage.Http.BadUrl "This is not a URL"
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.empty }
                    |> Expect.equal "This is not a URL"
        , test "Can get BadBody message" <|
            \_ ->
                Engage.Http.BadBody "This is the body"
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.empty }
                    |> Expect.equal "This is the body"
        , test "Can get Timeout message" <|
            \_ ->
                Engage.Http.Timeout
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.empty }
                    |> Expect.equal "[NetworkTimeout]"
        , test "Can get localized Timeout message" <|
            \_ ->
                Engage.Http.Timeout
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.fromDict (Dict.fromList [ ( "NetworkTimeout.Text", "The network timed out" ) ]) }
                    |> Expect.equal "The network timed out"
        , test "Can get NetworkError message" <|
            \_ ->
                Engage.Http.NetworkError
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.empty }
                    |> Expect.equal "[NetworkError]"
        , test "Can get localized NetworkError message" <|
            \_ ->
                Engage.Http.NetworkError
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.fromDict (Dict.fromList [ ( "NetworkError.Text", "The network errored" ) ]) }
                    |> Expect.equal "The network errored"
        , test "Can get default BadStatus message" <|
            \_ ->
                Engage.Http.BadStatus 404 """ { "ErrorMessage": "It wasn't found" } """
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.empty }
                    |> Expect.equal "Server.Error"
        , test "Can get localized default BadStatus message" <|
            \_ ->
                Engage.Http.BadStatus 404 """ { "ErrorMessage": "It wasn't found" } """
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.fromDict (Dict.fromList [ ( "Server.Error", "The server errored" ) ]) }
                    |> Expect.equal "The server errored"
        , test "Can get custom BadStatus message" <|
            \_ ->
                Engage.Http.BadStatus 404 """ { "Message": "It wasn't found" } """
                    |> Engage.Http.getErrorMessage { localization = Engage.Localization.empty }
                    |> Expect.equal "It wasn't found"
        ]
