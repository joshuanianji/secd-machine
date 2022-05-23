module Lib.UtilSpec exposing (suite)

import Expect
import Fuzz
import Lib.Util exposing (..)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Lib.Util"
        [ getLocationInfoTest ]


getLocationInfoTest : Test
getLocationInfoTest =
    let
        createPageInfo : Int -> Int -> { pageSize : Int, chunkSize : Int }
        createPageInfo pageSize chunkSize =
            { pageSize = pageSize, chunkSize = chunkSize }

        createLocationInfoReturn : Int -> Int -> Int -> { pageNum : Int, pageLocation : Int, chunkLocation : Int }
        createLocationInfoReturn pageNum pageLocation chunkLocation =
            { pageNum = pageNum, pageLocation = pageLocation, chunkLocation = chunkLocation }

        addLocInfoReturn : { pageSize : Int, chunkSize : Int } -> { pageNum : Int, pageLocation : Int, chunkLocation : Int } -> Int
        addLocInfoReturn { pageSize, chunkSize } { pageNum, pageLocation, chunkLocation } =
            chunkLocation + pageLocation * chunkSize + pageNum * pageSize * chunkSize

        defaultPageInfo =
            createPageInfo 10 15
    in
    Test.describe "Lib.Util.getLocationInfo"
        [ Test.test "0 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 0 defaultPageInfo
                    |> Expect.equal (createLocationInfoReturn 0 0 0)
        , Test.test "1 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 1 defaultPageInfo
                    |> Expect.equal (createLocationInfoReturn 0 0 1)
        , Test.test "2 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 2 defaultPageInfo
                    |> Expect.equal (createLocationInfoReturn 0 0 2)
        , Test.test "15 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 15 defaultPageInfo
                    |> Expect.equal (createLocationInfoReturn 0 1 0)
        , Test.test "14 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 14 defaultPageInfo
                    |> Expect.equal (createLocationInfoReturn 0 0 14)
        , Test.test "16 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 16 defaultPageInfo
                    |> Expect.equal (createLocationInfoReturn 0 1 1)
        , Test.test "149 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 149 (createPageInfo 10 15)
                    |> Expect.equal (createLocationInfoReturn 0 9 14)
        , Test.test "150 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 150 defaultPageInfo
                    |> Expect.equal (createLocationInfoReturn 1 0 0)
        , Test.test "151 for pageInfo = {10, 15}" <|
            \_ ->
                getLocationInfo 151 defaultPageInfo
                    |> Expect.equal (createLocationInfoReturn 1 0 1)
        , Test.fuzz (Fuzz.intRange 4 10000) "Fuzz for {10, 15}" <|
            \n ->
                getLocationInfo n defaultPageInfo
                    |> addLocInfoReturn defaultPageInfo
                    |> Expect.equal n
        ]
