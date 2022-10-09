# Verify.Form

This module is based around a simple type alias over
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

    nameValidator : FieldValidator String { form | nameErrors : Maybe (String, List String) } String
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