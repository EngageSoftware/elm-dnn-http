module Engage.Http exposing
    ( Config
    , get, post, patch, put, delete
    , getErrorMessage
    , configDecoder, serverErrorDecoder, multipleServerErrorDecoder, nullDecoder
    )

{-| Helpers for working with DNN Web API


# Types

@docs Config


# Http Verbs

@docs get, post, patch, put, delete


# Helper functions

@docs getErrorMessage


# Decode/Encoder

@docs configDecoder, serverErrorDecoder, multipleServerErrorDecoder, nullDecoder

-}

import Engage.Localization as Localization exposing (Localization)
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (..)
import RemoteData as RemoteData exposing (RemoteData)
import String
import Task as Task exposing (Task)
import Url.Builder


{-| Http GET
-}
get : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, queryStringParams : List ( String, String ) } -> Cmd msg
get { baseUrl, headers } toMsg decoder { methodName, queryStringParams } =
    request "GET"
        headers
        (urlwithQueryString baseUrl methodName queryStringParams)
        Http.emptyBody
        toMsg
        decoder


{-| Http POST
-}
post : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Cmd msg
post { baseUrl, headers } toMsg decoder { methodName, value } =
    request "POST"
        headers
        (urlwithQueryString baseUrl methodName [])
        (Http.jsonBody value)
        toMsg
        decoder


{-| Http PUT
-}
put : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Cmd msg
put { baseUrl, headers } toMsg decoder { methodName, value } =
    request "PUT"
        headers
        (urlwithQueryString baseUrl methodName [])
        (Http.jsonBody value)
        toMsg
        decoder


{-| Http PATCH
-}
patch : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Cmd msg
patch { baseUrl, headers } toMsg decoder { methodName, value } =
    request "PATCH"
        headers
        (urlwithQueryString baseUrl methodName [])
        (Http.jsonBody value)
        toMsg
        decoder


{-| Http DELETE
-}
delete : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Cmd msg
delete { baseUrl, headers } toMsg decoder { methodName, value } =
    request "DELETE"
        headers
        (urlwithQueryString baseUrl methodName [])
        (Http.jsonBody value)
        toMsg
        decoder



-- Types


{-| Configuration type

  - `baseUrl` the base URL for Http request
  - `headers`: list of the header for Http request

-}
type alias Config =
    { baseUrl : String
    , headers : List Http.Header
    }


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadPayload String



-- Decoders


{-| Json decoder for `Config`
-}
configDecoder : Decode.Decoder Config
configDecoder =
    Decode.succeed Config
        |> required "baseUrl" Decode.string
        |> required "headers" httpHeaderDecoder


httpHeaderDecoder : Decode.Decoder (List Http.Header)
httpHeaderDecoder =
    let
        decoder : List ( String, String ) -> List Http.Header
        decoder headers =
            headers
                |> List.map (\( key, value ) -> Http.header key value)
    in
    Decode.keyValuePairs Decode.string
        |> Decode.map decoder


{-| Null decoder. Useful for when the server doesn't return any value.
-}
nullDecoder : Decode.Decoder ()
nullDecoder =
    Decode.null ()



-- Helpers


request : String -> List Http.Header -> String -> Http.Body -> (RemoteData Error success -> msg) -> Decode.Decoder success -> Cmd msg
request method headers url body toMsg decoder =
    Http.request
        { method = method
        , headers = headers
        , url = url
        , body = body
        , expect = expectJsonAsRemoteData toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


expectJsonAsRemoteData : (RemoteData Error success -> msg) -> Decode.Decoder success -> Http.Expect msg
expectJsonAsRemoteData toMsg decoder =
    let
        parseResponse response =
            case response of
                Http.GoodStatus_ _ body ->
                    body
                        |> Decode.decodeString decoder
                        |> Result.mapError Decode.errorToString
                        |> Result.mapError BadPayload

                Http.BadStatus_ { statusCode } body ->
                    Err (BadStatus statusCode body)

                Http.NetworkError_ ->
                    Err NetworkError

                Http.Timeout_ ->
                    Err Timeout

                Http.BadUrl_ url ->
                    Err (BadUrl url)

        resultToMsg result =
            result
                |> RemoteData.fromResult
                |> toMsg
    in
    Http.expectStringResponse resultToMsg parseResponse


ensureStringEndsWithSlash : String -> String
ensureStringEndsWithSlash baseUrl =
    if String.endsWith "/" baseUrl then
        baseUrl

    else
        baseUrl ++ "/"


urlwithQueryString : String -> String -> List ( String, String ) -> String
urlwithQueryString baseUrl methodName queryStringParams =
    queryStringParams
        |> List.map (\( key, val ) -> Url.Builder.string key val)
        |> Url.Builder.toQuery
        |> (\query -> ensureStringEndsWithSlash baseUrl ++ methodName ++ query)


{-| Get the localized error message from the `Error`
-}
getErrorMessage : { a | localization : Localization } -> Error -> String
getErrorMessage args error =
    case error of
        BadUrl url ->
            url

        Timeout ->
            Localization.localizeString "NetworkTimeout" args

        NetworkError ->
            Localization.localizeString "NetworkError" args

        BadStatus statusCode body ->
            let
                defaultMessage =
                    Localization.localizeStringWithDefault "Server.Error" "Server.Error" args
            in
            body
                |> Decode.decodeString (serverErrorDecoder args)
                |> Result.toMaybe
                |> Maybe.withDefault defaultMessage

        BadPayload errorMessage ->
            errorMessage


{-| Json decoder for server error, localized.
-}
serverErrorDecoder : { a | localization : Localization } -> Decoder String
serverErrorDecoder args =
    Decode.oneOf
        [ Decode.field "ExceptionMessage" Decode.string
        , Decode.field "exceptionMessage" Decode.string
        , Decode.field "Message" Decode.string
        , Decode.field "message" Decode.string
        , multipleServerErrorDecoder args
        ]


{-| Json decoder for multiple server error messages, localized.
-}
multipleServerErrorDecoder : { a | localization : Localization } -> Decoder String
multipleServerErrorDecoder args =
    let
        toMultipleServerErrorDecoder : List String -> Decoder String
        toMultipleServerErrorDecoder messages =
            messages
                |> List.map (\message -> Localization.localizeStringWithDefault message message args)
                |> List.foldr (\localizedMessage newMessage -> localizedMessage ++ " " ++ newMessage) ""
                |> Decode.succeed
    in
    Decode.oneOf
        [ Decode.at [ "ExceptionMessage" ] (Decode.list Decode.string) |> Decode.andThen toMultipleServerErrorDecoder
        , Decode.at [ "exceptionMessage" ] (Decode.list Decode.string) |> Decode.andThen toMultipleServerErrorDecoder
        , Decode.at [ "Messages" ] (Decode.list Decode.string) |> Decode.andThen toMultipleServerErrorDecoder
        , Decode.at [ "messages" ] (Decode.list Decode.string) |> Decode.andThen toMultipleServerErrorDecoder
        ]
