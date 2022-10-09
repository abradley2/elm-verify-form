module Example exposing (..)

import Expect exposing (Expectation)
import String.Nonempty exposing (NonemptyString)
import Test exposing (..)
import Verify
import Verify.Form exposing (FieldValidator, FormValidator)


type alias Form =
    { firstName : String
    , firstNameError : Maybe String
    , lastName : String
    , lastNameError : Maybe String
    }


type alias ValidatedForm =
    { firstName : NonemptyString
    , lastName : NonemptyString
    }


firstNameValidator : FieldValidator String Form NonemptyString
firstNameValidator input =
    case String.Nonempty.fromString input of
        Just firstName ->
            Ok firstName

        Nothing ->
            Err (\form -> { form | firstNameError = Just "First name cannot be empty" })


lastNameValidator : FieldValidator String Form NonemptyString
lastNameValidator input =
    case String.Nonempty.fromString input of
        Just lastName ->
            Ok lastName

        Nothing ->
            Err (\form -> { form | lastNameError = Just "Last name cannot be empty" })


formValidator : FormValidator Form ValidatedForm
formValidator =
    Verify.Form.validate ValidatedForm
        |> Verify.Form.verify .firstName firstNameValidator
        |> Verify.Form.verify .lastName lastNameValidator


suite : Test
suite =
    describe "form verify"
        [ test "Works for a valid form" <|
            \_ ->
                { firstName = "John"
                , firstNameError = Nothing
                , lastName = "Doe"
                , lastNameError = Nothing
                }
                    |> Verify.Form.run formValidator
                    |> ([ .firstName >> String.Nonempty.toString >> Expect.equal "John"
                        , .lastName >> String.Nonempty.toString >> Expect.equal "Doe"
                        ]
                            |> List.map (\fn -> Result.map fn >> Result.withDefault (Expect.fail "Expected a valid result"))
                            |> Expect.all
                       )
        , test "Works for an invalid form" <|
            \_ ->
                { firstName = ""
                , firstNameError = Nothing
                , lastName = ""
                , lastNameError = Nothing
                }
                    |> Verify.Form.run formValidator
                    |> (\result ->
                            case result of
                                Ok _ ->
                                    Expect.fail "Expected an invalid result"

                                Err formWithErrors ->
                                    Expect.all
                                        [ .firstName >> Expect.equal ""
                                        , .firstNameError >> Expect.equal (Just "First name cannot be empty")
                                        , .lastName >> Expect.equal ""
                                        , .lastNameError >> Expect.equal (Just "Last name cannot be empty")
                                        ]
                                        formWithErrors
                       )
        , test "We can skip validation with 'keep'" <|
            \_ ->
                { firstName = "Tony", lastName = "Bradley" }
                    |> Verify.Form.run
                        (Verify.Form.validate Tuple.pair
                            |> Verify.Form.keep .firstName
                            |> Verify.Form.keep .lastName
                        )
                    |> (\result ->
                            case result of
                                Err _ ->
                                    Expect.fail "Expected a valid result"

                                Ok form ->
                                    Expect.equal ( "Tony", "Bradley" ) form
                       )
        , test "We can use custom validators with 'liftValidator'" <|
            \_ ->
                { firstName = ""
                , firstNameError = Nothing
                , lastName = ""
                , lastNameError = Nothing
                }
                    |> Verify.Form.run
                        (Verify.Form.validate ValidatedForm
                            |> Verify.Form.verify
                                .firstName
                                ((String.Nonempty.fromString >> Result.fromMaybe ( "First name cannot be empty", [] ))
                                    |> Verify.Form.liftValidator (\( err, _ ) form -> { form | firstNameError = Just err })
                                )
                            |> Verify.Form.verify
                                .lastName
                                ((String.Nonempty.fromString >> Result.fromMaybe ( "Last name cannot be empty", [] ))
                                    |> Verify.Form.liftValidator (\( err, _ ) form -> { form | lastNameError = Just err })
                                )
                        )
                    |> (\result ->
                            case result of
                                Ok _ ->
                                    Expect.fail "Expected an invalid result"

                                Err formWithErrors ->
                                    Expect.all
                                        [ .firstName >> Expect.equal ""
                                        , .firstNameError >> Expect.equal (Just "First name cannot be empty")
                                        , .lastName >> Expect.equal ""
                                        , .lastNameError >> Expect.equal (Just "Last name cannot be empty")
                                        ]
                                        formWithErrors
                       )
        ]
