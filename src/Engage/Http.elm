module Engage.Http
    exposing
        ( Config
        , configDecoder
        , delete
        , get
        , getErrorMessage
        , multipleServerErrorDecoder
        , nullDecoder
        , patch
        , post
        , put
        , serverErrorDecoder
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
import RemoteData.Http as RemoteHttp
import String
import Task as Task exposing (Task)


{-| Http GET
-}
get : Config -> Decode.Decoder success -> { methodName : String, queryStringParams : List ( String, String ) } -> Task Never (WebData success)
get { baseUrl, headers } decoder { methodName, queryStringParams } =
    let
        config : RemoteHttp.Config
        config =
            { headers = headers
            , withCredentials = False
            , timeout = Nothing
            }

        url : String
        url =
            urlwithQueryString baseUrl methodName queryStringParams
    in
    RemoteHttp.getTaskWithConfig config url decoder


{-| Http POST
-}
post : Config -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Task Never (WebData success)
post { baseUrl, headers } decoder { methodName, value } =
    let
        config : RemoteHttp.Config
        config =
            { headers = headers
            , withCredentials = False
            , timeout = Nothing
            }

        url : String
        url =
            urlwithQueryString baseUrl methodName []
    in
    RemoteHttp.postTaskWithConfig config url decoder value


{-| Http PUT
-}
put : Config -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Task Never (WebData success)
put { baseUrl, headers } decoder { methodName, value } =
    let
        config : RemoteHttp.Config
        config =
            { headers = headers
            , withCredentials = False
            , timeout = Nothing
            }

        url : String
        url =
            urlwithQueryString baseUrl methodName []
    in
    RemoteHttp.putTaskWithConfig config url decoder value


{-| Http PATCH
-}
patch : Config -> Decode.Decoder success -> { methodName : String, value : Encode.Value } -> Task Never (WebData success)
patch { baseUrl, headers } decoder { methodName, value } =
    let
        config : RemoteHttp.Config
        config =
            { headers = headers
            , withCredentials = False
            , timeout = Nothing
            }

        url : String
        url =
            urlwithQueryString baseUrl methodName []
    in
    RemoteHttp.patchTaskWithConfig config url decoder value


{-| Http DELETE
-}
delete : Config -> { methodName : String, value : Encode.Value } -> Task Never (WebData String)
delete { baseUrl, headers } { methodName, value } =
    let
        config : RemoteHttp.Config
        config =
            { headers = headers
            , withCredentials = False
            , timeout = Nothing
            }

        url : String
        url =
            urlwithQueryString baseUrl methodName []
    in
    RemoteHttp.deleteTaskWithConfig config url value



-- Types


{-| Configuration type

  - `baseUrl` the base URL for Http request
  - `headers`: `list of the header for Http request

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
    decode Config
        |> required "baseUrl" Decode.string
        |> required "headers" httpHeaderDecoder


httpHeaderDecoder : Decode.Decoder (List Http.Header)
httpHeaderDecoder =
    let
        decoder : List ( String, String ) -> Decode.Decoder (List Http.Header)
        decoder headers =
            headers
                |> List.map (\( key, value ) -> Http.header key value)
                |> decode
    in
    Decode.keyValuePairs Decode.string
        |> Decode.andThen decoder


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
    RemoteHttp.url (ensureStringEndsWithSlash baseUrl ++ methodName) queryStringParams


{-| Get the localized error message from the `Http.Error`
-}
getErrorMessage : { a | localization : Localization } -> Http.Error -> String
getErrorMessage args error =
    case error of
        Http.BadUrl errorMessage ->
            errorMessage

        Http.Timeout ->
            Localization.localizeString "NetworkTimeout" args

        Http.NetworkError ->
            Localization.localizeString "NetworkError" args

        Http.BadStatus response ->
            response.body
                |> Decode.decodeString (serverErrorDecoder args)
                |> Result.toMaybe
                |> Maybe.withDefault "Server.Error"

        Http.BadPayload errorMessage { body } ->
            errorMessage ++ " " ++ body


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
        |> Decode.map (\message -> Localization.localizeStringWithDefault message message args)


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
