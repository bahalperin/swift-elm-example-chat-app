import Vapor

class Room {
    var connections: [String: WebSocket]

    func bot(_ message: String) throws {
        try send(name: "Bot", message: message)
    }

    func send(name: String, message: String) throws {
        let message = message.truncated(to: 256)

        var msg = Message(username: name, content: message)
        try? msg.save()
        let json = try msg.makeJSON()

        for (username, socket) in connections {
            guard username != name else {
                continue
            }

            try socket.send(json)
        }
    }

    init() {
        connections = [:]
    }
}
