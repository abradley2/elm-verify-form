module Verify.Form exposing
    ( FormValidator, FieldValidator
    , keep, liftValidator, run, validate, verify
    )

{-| This module is based around a simple type alias over
[elm-verify](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest)'s
[Validator](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest/Verify#Validator)
type that I commonly find myself using.

Whereas the `Validator` type is defined as

    type alias Validator error input verified =
        input -> Result ( error, List error ) verified

The `FormValidator` types is defined

    type alias FormValidator input verified =
        input -> Result (form -> form) verified

[Validator](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest/Verify#Validator) is great if
you wish for the failure of validation to be a list of errors, but what if we have those errors expressed
on our model?

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

We don't need a unified "error" type. Our errors are just part of our model. Here's a quick example
of how this looks in action:

    import Verify.Form exposing (FieldValidator, FormValidator)

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

    formValidator : FormValidator Model ValidatedForm
    formValidator =
        Verify.Form.ok ValidatedForm
            |> Verify.Form.verify .firstName firstNameValidator
            |> Verify.Form.verify .lastName lastNameValidator

now on form submit we may handle things thusly

    update : Msg -> Form -> ( Form, Cmd Msg )
    update msg form =
        case msg of
            SubmitForm ->
                case Verify.Form.run formValidator form of
                    Ok validatedForm ->
                        ( form, submitForm validatedForm )

                    Err formWithErrors ->
                        ( formWithErrors, Cmd.none )

You may still get the full utility of
[elm-verify](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest)'s helper modules
and `Validator` type. To turn a `Validator` into a `FieldValidator`, you may use `liftValidator`.
We only need to provide a function that states where on the form we should put a list of errors
if any occur.

    import String.Verify
    import Verify
    import Verify.Form exposing (FieldValidator)

    nameValidator : FieldValidator String { form | nameErrors : Maybe (List String) } String
    nameValidator =
        String.Verify.notBlank "Name cannot be empty"
            |> Verify.compose (String.Verify.minLength 2 "Name must be at least 2 characters long")
            |> Verify.compose (String.Verify.maxLength 50 "Name must be at most 50 characters long")
            |> Verify.compose
                (\input ->
                    if input == "Tony" then
                        Err ( "Tony is not a valid name", [] )

                    else
                        Ok input
                )
            |> Verify.Form.liftValidator (\errors form -> { form | nameErrors = Just errors })


# Types

@docs FormValidator, FieldValidator


# Common Helpers

@docs keep, liftValidator, run, validate, verify

-}

import Verify exposing (Validator)


{-| Simple type alias over [Validator](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest/Verify#Validator)
that forgoes an error type in favor of an "Error setter" for the the `input` type.
-}
type alias FormValidator form verified =
    Validator (form -> form) form verified


{-| A type alias to represent a validator that is applied to a field on a form.
-}
type alias FieldValidator field form verified =
    field -> Result (form -> form) verified


{-| Just a re-export of `Verify.validate` for convenience.
-}
validate : verified -> FormValidator input verified
validate =
    Verify.validate


{-| Used to apply a `FormValidator` to an input form.
-}
run : FormValidator form validatedForm -> form -> Result form validatedForm
run validator initialForm =
    validator initialForm
        |> Result.mapError (\( err, errors ) -> err :: errors)
        |> Result.mapError (List.foldr (<|) initialForm)


{-| Creates a `FormValidator` that automatically passes for a given field. Useful if we don't want
to enforce validation on something, but still want to keep it in the pipeline. Similar to
[Verify.keep](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest/Verify#keep)
-}
keep :
    (form -> field)
    -> FormValidator form (field -> finally)
    -> FormValidator form finally
keep getter =
    verify
        getter
        Ok


{-| Pipeline function that takes a field getter and a `FieldValidator` to transform a `FormValidator`
-}
verify :
    (form -> field)
    -> FieldValidator field form verified
    -> FormValidator form (verified -> finally)
    -> FormValidator form finally
verify getter fieldValidator formValidator =
    Verify.verify
        identity
        (liftFieldValidator fieldValidator getter)
        formValidator


liftFieldValidator : FieldValidator input form verified -> (form -> input) -> FormValidator form verified
liftFieldValidator fieldValidator getter form =
    let
        input =
            getter form
    in
    fieldValidator input
        |> Result.mapError (\err -> ( err, [] ))


{-| Convenience function for using the [Validator](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest/Verify#Validator)
type as a `FieldValidator`. The first argument tells the resulting `FormValidator` where on the form this `FieldValidator` will
apply any errors to.
-}
liftValidator : (List error -> form -> form) -> Validator error input verified -> FieldValidator input form verified
liftValidator fromErrors fn =
    fn
        >> Result.mapError (\( err, errors ) -> fromErrors (err :: errors))
