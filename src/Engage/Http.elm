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
import RemoteData as RemoteData exposing (WebData)
import RemoteData.Http as RemoteHttp exposing (defaultTaskConfig)
import String
import Task as Task exposing (Task)
import Url.Builder


{-| Http GET
-}
get : Config -> Decode.Decoder success -> { methodName : String, queryStringParams : List ( String, String ) } -> Task () (WebData success)
get { baseUrl, headers } decoder { methodName, queryStringParams } =
    let
        config : RemoteHttp.TaskConfig
        config =
            { defaultTaskConfig | headers = headers }

        url : String
        url =
            urlwithQueryString baseUrl methodName queryStringParams
    in
    RemoteHttp.getTaskWithConfig config url decoder


{-| Http POST
-}
post : Config -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Task () (WebData success)
post { baseUrl, headers } decoder { methodName, value } =
    let
        config : RemoteHttp.TaskConfig
        config =
            { defaultTaskConfig | headers = headers }

        url : String
        url =
            urlwithQueryString baseUrl methodName []
    in
    RemoteHttp.postTaskWithConfig config url decoder value


{-| Http PUT
-}
put : Config -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Task () (WebData success)
put { baseUrl, headers } decoder { methodName, value } =
    let
        config : RemoteHttp.TaskConfig
        config =
            { defaultTaskConfig | headers = headers }

        url : String
        url =
            urlwithQueryString baseUrl methodName []
    in
    RemoteHttp.putTaskWithConfig config url decoder value


{-| Http PATCH
-}
patch : Config -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Task () (WebData success)
patch { baseUrl, headers } decoder { methodName, value } =
    let
        config : RemoteHttp.TaskConfig
        config =
            { defaultTaskConfig | headers = headers }

        url : String
        url =
            urlwithQueryString baseUrl methodName []
    in
    RemoteHttp.patchTaskWithConfig config url decoder value


{-| Http DELETE
-}
delete : Config -> { methodName : String, value : Encode.Value } -> Task () (WebData String)
delete { baseUrl, headers } { methodName, value } =
    let
        config : RemoteHttp.TaskConfig
        config =
            { defaultTaskConfig | headers = headers }

        url : String
        url =
            urlwithQueryString baseUrl methodName []
    in
    RemoteHttp.deleteTaskWithConfig config url value



-- Types


{-| Configuration type

  - `baseUrl` the base URL for Http request
  - `headers`: list of the header for Http request

-}
type alias Config =
    { baseUrl : String
    , headers : List Http.Header
    }



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


{-| Get the localized error message from the `Http.Error`
-}
getErrorMessage : { a | localization : Localization } -> Http.Error -> String
getErrorMessage args error =
    case error of
        Http.BadUrl url ->
            url

        Http.Timeout ->
            Localization.localizeString "NetworkTimeout" args

        Http.NetworkError ->
            Localization.localizeString "NetworkError" args

        Http.BadStatus statusCode ->
            let
                defaultMessage =
                    Localization.localizeStringWithDefault "Server.Error" "Server.Error" args
            in
            Localization.localizeStringWithDefault defaultMessage (String.fromInt statusCode) args

        Http.BadBody errorMessage ->
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
