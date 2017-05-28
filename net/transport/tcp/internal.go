package tcp


type ControlHeader uint8 

const CreateNewConnection = ControlHeader(0)
const CloseConnection = ControlHeader(1)
const CloseSocket = ControlHeader(2)
const CloseEndPoint = ControlHeader(3)
const ProbeSocket = ControlHeader(4)
const ProbeSocketAct = ControlHeader(5)


func (rs ControlHeader) String() string {
    switch rs {
        case CreateNewConnection: return "CreateNewConnection"
        case CloseConnection: return "CloseConnection"
        case CloseSocket: return "CloseSocket"
        case CloseEndPoint: return "CloseEndPoint"
        case ProbeSocket: return "ProbeSocket"
        case ProbeSocketAct: return "ProbeSocketAct"
        default: return "ControlHeaderUnknown" // Cannot panic.
    }
}

type ConnectionRequestResponse uint8 

const ConnectionRequestAccepted = ConnectionRequestResponse(0)
const ConnectionRequestInvalid = ConnectionRequestResponse(1)
const ConnectionRequestCrossed = ConnectionRequestResponse(2)
const ConnectionRequestHostMismatch = ConnectionRequestResponse(3)


func (rs ConnectionRequestResponse) String() string {
    switch rs {
        case ConnectionRequestAccepted: return "ConnectionRequestAccepted"
        case ConnectionRequestInvalid: return "ConnectionRequestInvalid"
        case ConnectionRequestCrossed: return "ConnectionRequestCrossed"
        case ConnectionRequestHostMismatch: return "ConnectionRequestHostMismatch"
        default: return "ConnectionRequestResponseUnknown" // Cannot panic.
    }
}

