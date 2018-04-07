module Model exposing (..)

import Bootstrap.Navbar as Navbar
import Types exposing (..)
import PositionForm
import OrderForm
import SyncAPI
import Sync


type alias Model =
    { navbar : Navbar.State
    , orders : List Types.Order
    , user : Participant
    , syncState : Sync.SyncState
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
    | OrderOrder OrderId
    | FullSync SyncAPI.SyncData
    | SyncParticipant SyncAPI.User
    | SyncPosition SyncAPI.Position
    | SyncOrder SyncAPI.Order
