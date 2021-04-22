module Engage.Http exposing
    ( Config, Error(..)
    , get, post, patch, put, delete
    , requestJson, requestString
    , getErrorMessage
    , configDecoder, serverErrorDecoder, multipleServerErrorDecoder, nullDecoder
    )

{-| Helpers for working with DNN Web API


# Types

@docs Config, Error


# HTTP Verbs

@docs get, post, patch, put, delete


# Raw requests

@docs requestJson, requestString


# Helper functions

@docs getErrorMessage


# Decoders

@docs configDecoder, serverErrorDecoder, multipleServerErrorDecoder, nullDecoder

-}

import Engage.Localization as Localization exposing (Localization)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData)
import String
import Url.Builder


{-| HTTP GET
-}
get : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, queryStringParams : List ( String, String ) } -> Cmd msg
get { baseUrl, headers } toMsg decoder { methodName, queryStringParams } =
    requestJson "GET"
        headers
        (urlWithQueryString baseUrl methodName queryStringParams)
        Http.emptyBody
        toMsg
        decoder


{-| HTTP POST
-}
post : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Cmd msg
post { baseUrl, headers } toMsg decoder { methodName, value } =
    requestJson "POST"
        headers
        (urlWithQueryString baseUrl methodName [])
        (Http.jsonBody value)
        toMsg
        decoder


{-| HTTP PUT
-}
put : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Cmd msg
put { baseUrl, headers } toMsg decoder { methodName, value } =
    requestJson "PUT"
        headers
        (urlWithQueryString baseUrl methodName [])
        (Http.jsonBody value)
        toMsg
        decoder


{-| HTTP PATCH
-}
patch : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Cmd msg
patch { baseUrl, headers } toMsg decoder { methodName, value } =
    requestJson "PATCH"
        headers
        (urlWithQueryString baseUrl methodName [])
        (Http.jsonBody value)
        toMsg
        decoder


{-| HTTP DELETE
-}
delete : Config -> (RemoteData Error success -> msg) -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Cmd msg
delete { baseUrl, headers } toMsg decoder { methodName, value } =
    requestJson "DELETE"
        headers
        (urlWithQueryString baseUrl methodName [])
        (Http.jsonBody value)
        toMsg
        decoder



-- Types


{-| Configuration type

  - `baseUrl` the base URL for the HTTP requests
  - `headers`: list of the headers for the HTTP requests

-}
type alias Config =
    { baseUrl : String
    , headers : List Http.Header
    }


{-| When a request fails, the `Failure` value will be one of these. Most of these are copied from `Http.Error` in the `elm/http` module.

When there is a `BadStatus`, both the status code and the body of the response are included. This allows `getErrorMessage` to retrieve any error message in the response, if they are available.

-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadBody String



-- Decoders


{-| Json decoder for `Config`

    import Http
    import Json.Decode as Decode

    """
    {
        "baseUrl": "/API/MyModule/Search",
        "headers": {
            "ModuleId": "100",
            "TabId": "404"
        }
    }
    """
        |> Decode.decodeString Engage.Http.configDecoder
    --> Ok { baseUrl = "/API/MyModule/Search", headers = [ Http.header "ModuleId" "100", Http.header "TabId" "404" ] }

-}
configDecoder : Decode.Decoder Config
configDecoder =
    Decode.map2 Config
        (Decode.field "baseUrl" Decode.string)
        (Decode.field "headers" httpHeaderDecoder)


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


{-| JSON decoder for server error, localized.

The error comes from a string field named "ExceptionMessage", "exceptionMessage", "Message", "message", or otherwise `multipleServerErrorDecoder`.

-}
serverErrorDecoder : { a | localization : Localization } -> Decode.Decoder String
serverErrorDecoder args =
    Decode.oneOf
        [ Decode.field "ExceptionMessage" Decode.string
        , Decode.field "exceptionMessage" Decode.string
        , Decode.field "Message" Decode.string
        , Decode.field "message" Decode.string
        , multipleServerErrorDecoder args
        ]


{-| JSON decoder for multiple server error messages, localized.

The messages come from a string array field named "ExceptionMessage", "exceptionMessage", "Messages", or "message".
These values are then concatenated with spaces.

-}
multipleServerErrorDecoder : { a | localization : Localization } -> Decode.Decoder String
multipleServerErrorDecoder args =
    let
        toMultipleServerErrorDecoder : List String -> Decode.Decoder String
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



-- Helpers


{-| Raw request that expects a JSON response.

This version can use any method and accepts any body (e.g. `Http.fileBody`, `Http.bytesBody`, or `Http.emptyBody`).

-}
requestJson : String -> List Http.Header -> String -> Http.Body -> (RemoteData Error success -> msg) -> Decode.Decoder success -> Cmd msg
requestJson method headers url requestBody toMsg decoder =
    let
        toResult : Http.Metadata -> String -> Result Error success
        toResult _ responseBody =
            responseBody
                |> Decode.decodeString decoder
                |> Result.mapError Decode.errorToString
                |> Result.mapError BadBody
    in
    requestString method headers url requestBody toMsg toResult


{-| Raw request that expects a `String` response (but not necessarily a valid JSON `String`).

This version can use any method and accepts any body (e.g. `Http.fileBody`, `Http.bytesBody`, or `Http.emptyBody`).
Additionally, instead of providing a `Json.Decode.Decoder` to handle the response, you provide a function which can
transform the `String` response body (and response metadata) however you need.

-}
requestString : String -> List Http.Header -> String -> Http.Body -> (RemoteData Error success -> msg) -> (Http.Metadata -> String -> Result Error success) -> Cmd msg
requestString method headers url body toMsg goodStatusToResult =
    Http.request
        { method = method
        , headers = headers
        , url = url
        , body = body
        , expect = Http.expectStringResponse (RemoteData.fromResult >> toMsg) (responseToResult goodStatusToResult)
        , timeout = Nothing
        , tracker = Nothing
        }


responseToResult : (Http.Metadata -> String -> Result Error success) -> Http.Response String -> Result Error success
responseToResult goodStatusToResult response =
    case response of
        Http.GoodStatus_ metadata body ->
            goodStatusToResult metadata body

        Http.BadStatus_ { statusCode } body ->
            Err (BadStatus statusCode body)

        Http.NetworkError_ ->
            Err NetworkError

        Http.Timeout_ ->
            Err Timeout

        Http.BadUrl_ url ->
            Err (BadUrl url)


ensureStringEndsWithSlash : String -> String
ensureStringEndsWithSlash baseUrl =
    if String.endsWith "/" baseUrl then
        baseUrl

    else
        baseUrl ++ "/"


urlWithQueryString : String -> String -> List ( String, String ) -> String
urlWithQueryString baseUrl methodName queryStringParams =
    queryStringParams
        |> List.map (\( key, val ) -> Url.Builder.string key val)
        |> Url.Builder.toQuery
        |> (\query -> ensureStringEndsWithSlash baseUrl ++ methodName ++ query)


{-| Get the localized error message from the `Error`

    import Dict
    import Engage.Localization as Localization exposing (Localization)

    type alias Model =
        { localization : Localization }

    model : Model
    model =
        Model Localization.empty

    decodeError : Engage.Http.Error
    decodeError =
        Engage.Http.BadBody "Problem with the given value: null Expecting an INT"

    Engage.Http.getErrorMessage model decodeError
    --> "Problem with the given value: null Expecting an INT"

    errorFromServer : Engage.Http.Error
    errorFromServer =
        Engage.Http.BadStatus 404 """ { "Message": "The requested item wasn't found" } """

    Engage.Http.getErrorMessage model errorFromServer
    --> "The requested item wasn't found"

    localizedModel : Model
    localizedModel =
        Dict.fromList
            [ ( "NetworkError.Text", "The network had an issue" )
            , ( "Server.Error", "The server had an issue" )
            ]
            |> Localization.fromDict
            |> Model

    statusError : Engage.Http.Error
    statusError =
        Engage.Http.BadStatus 404 """ 404 Error response """

    Engage.Http.getErrorMessage localizedModel statusError
    --> "The server had an issue"

    Engage.Http.getErrorMessage localizedModel Engage.Http.NetworkError
    --> "The network had an issue"

-}
getErrorMessage : { a | localization : Localization } -> Error -> String
getErrorMessage args error =
    case error of
        BadUrl url ->
            url

        Timeout ->
            Localization.localizeStringWithDefault "There was an error communicating with the server, please try again" "NetworkTimeout" args

        NetworkError ->
            Localization.localizeStringWithDefault "There was an error sending your request, please check your connection" "NetworkError" args

        BadStatus statusCode body ->
            let
                defaultMessage : String
                defaultMessage =
                    Localization.localizeStringWithDefault "There was an error processing your request" "Server.Error" args
            in
            body
                |> Decode.decodeString (serverErrorDecoder args)
                |> Result.toMaybe
                |> Maybe.withDefault defaultMessage

        BadBody errorMessage ->
            errorMessage
