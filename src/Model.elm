module Model exposing (..)

import Bootstrap.Navbar as Navbar
import Types exposing (..)
import PositionForm
import OrderForm


type alias Model =
    { navbar : Navbar.State
    , orders : List Types.Order
    , user : Participant
    , positionForm : Maybe PositionForm.Model
    , orderForm : OrderForm.Model
    }


type Msg
    = NoOp
    | NavbarMsg Navbar.State
    | OpenPositionForm OrderId
    | UpdatePositionForm PositionForm.Msg
    | SavePositionForm
    | EditOrder OrderId
    | UpdateOrderForm OrderForm.Msg
    | SaveOrderForm
