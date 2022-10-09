module Example exposing (..)

import Expect exposing (Expectation)
import String.Nonempty exposing (NonemptyString)
import Test exposing (..)
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
    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
